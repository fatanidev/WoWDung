#lang racket

(require srfi/1)
(require srfi/13)

(define objects '((1 "a Silver Dagger")
                  (7 "a Silver Sword")
                  (9 "a Holy Mace")
                  (1 "a Silver Coin")))

(define descriptions '(
                       ;; start of the game
                       (1 "You are in the Elwen Forest.")
                       ;; first city
                       (2 "You are in the Storm City.")
                       (3 "You are in the Storm Dungeon.")
                       (4 "You are in the Prisoner's Cell.")
                       (5 "You are in the Execution Room")
                       ;; east city (accessed through Elwen Forest)
                       (6 "You are in EastFall.")
                       (7 "You are in EastFall's Camp")
                       (8 "You are at the Riverbank of EastFalls.")
                       ;; Horde camp
                       (9 "Now you are at LightShore")
                       (10 "You are at a Fel Cave")
                       (11 "You are at Ragnar Barracks")
                       
                       ;; final boss
                       (12 "You are in the Main Room .")))

(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define pick '(((get) pick) ((pickup) pick) ((pick) pick)))
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))
(define actions `(,@look ,@quit ,@pick ,@put ,@inventory))

(define decisiontable `((1 ((Storm City) 2) (( Eastfall) 6) ,@actions)
                        
                        (2 ((Elwen Forest) 1) ((Storm Dungeon) 3)  ,@actions)
                        (3 ((Storm City) 2) ((Prisoners Cell) 4) ((Execution Room) 5) ,@actions)
                        (4 ((Storm Dungeon) 3)  ,@actions)
                        (5 ((Storm Dungeon) 3) ,@actions)
                        
                        (6 ((Storm City) 2) ((EastFalls Camp) 7) ,@actions)
                        (7 ((EastFall) 6) ((RiverBank of EastFalls) 8) ,@actions)
                        (8 ((Eastfalls Camp) 7) ((Light Shore) 9) ,@actions)
                        
                        (9 ((Riverbank of EastFalls) 8) ((Fel Cave) 10) ((Ragnar Barracks) 11) ,@actions)
                        (10 ((Light Shore) 9) ,@actions)
                        (11 ((Light Shore) 9) ((Boss Room) 12) ,@actions)
                        (12 ((Light Shore 9) ,@actions))))

(define objectdb (make-hash))
(define inventorydb (make-hash))

(define (add-object db id object)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons object record)))
      (hash-set! db id (cons object empty))))

(define (add-objects db)
  (for-each
   (lambda (r) 
     (add-object db (first r) (second r))) objects))

(add-objects objectdb)

(define (display-objects db id)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (output (string-join record " and ")))
      (when (not (equal? output ""))
        (if (eq? id 'bag)
            (printf "You are carrying ~a.\n" output)
            (printf "You can see ~a.\n" output))))))

(define (remove-object-from-room db id str)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item) 
             (printf "I don't see that item in the room!\n"))
            (else
             (printf "Added ~a to your bag.\n" (first item))
             (add-object inventorydb 'bag (first item))
             (hash-set! db id result))))))

(define (remove-object-from-inventory db id str)
  (when (hash-has-key? db 'bag)
    (let* ((record (hash-ref db 'bag))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item)
             (printf "You are not carrying that item!\n"))
            (else
             (printf "Removed ~a from your bag.\n" (first item))
             (add-object objectdb id (first item))
             (hash-set! db 'bag result))))))
             
(define (pick-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-room objectdb id item)))

(define (put-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-inventory inventorydb id item)))

(define (display-inventory)
  (display-objects inventorydb 'bag))

(define (slist->string l)
  (string-join (map symbol->string l)))

(define (get-directions id)
  (let ((record (assq id decisiontable)))
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      (cond ((= 0 n)
             (printf "You appear to have entered a room with no exits.\n"))
            ((= 1 n)
             (printf "You can see an exit to the ~a.\n" (slist->string (caar result))))
            (else
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               (printf "You can see exits to the ~a.\n" (string-join lostr " and "))))))))

(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

(define (get-description id)
  (car (assq-ref descriptions id)))

(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    (map (lambda (key) (car key)) keys)))


;; outputs a list in the form: (0 0 0 2 0 0)
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       ;; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))

(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
      #f
      (list-index (lambda (x) (eq? x n)) list-of-numbers))))


(define (lookup id tokens)
  (let* ((record (assv-ref decisiontable id))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index 
      (cadr (list-ref record index))
      #f)))

(define (display-description id)
  (printf "~a\n" (get-description id)))

(define (startgame initial-id)
  (let loop ((id initial-id) (description #t))
    (when description
      (display-description id)
      (display-objects objectdb id))
    (printf "> ")
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
        (cond ((number? response)
               (loop response #t))
              ((eq? #f response)
               (printf "huh? I didn't understand that!\n")
               (loop id #f))
              ((eq? response 'look)
               (get-directions id)
               (loop id #t))
              ((eq? response 'pick)
               (pick-item id input)
               (loop id #f))
              ((eq? response 'drop)
               (put-item id input)
               (loop id #f))
              ((eq? response 'inventory)
               (display-inventory)
               (loop id #f))              
              ((eq? response 'quit)
               (printf "So Long, and Thanks for All the Fish...\n")
               (exit)))))))


(startgame 1)

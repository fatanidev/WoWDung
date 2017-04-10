#lang racket

(require srfi/1)
(require srfi/13)
;;############################################################################################
;;#######         Defining items that will be accessible throughout the game           #######
;;#######         Defining items that will be accessible throughout the game           #######
;;#######                                                                              #######
;;#######                                                                              #######
;;############################################################################################

;; defines the objects that users can interact with throughout the game
;; the number represents the room id and the text represents the name of the item
(define objects '((1 "a Silver Dagger")
                  (3 "a Silver Sword" )
                  (9 "a Holy Mace")
                  (1 "a Silver Coin")))

;; defines the monsters that users can interact with throughout the game
;; the number represents the room id and the text represents the name of the monster
(define monsters '((1 "wolf")
                  (2 "guard")
                  (5 "worgen")
                  (10 "undead")
                  (8 "murloc")
                  (12 "gulda")
                  (4 "orc")))


;; Different areas for the user to explore
(define descriptions '(
                       ;; start of the game
                       (1 "############ You are in the Elwen Forest. ############")
                       ;; first city
                       (2 "############ You are in the Storm City. ############")
                       (3 "############ You are in the Storm Dungeon ############")
                       (4 "############ You are in the Prisoner's Cell ############")
                       (5 "############ You are in the Execution Room ############")
                       ;; east city (accessed through Elwen Forest)
                       (6 "############ You are in EastFall ############")
                       (7 "############ You are in EastFall's Camp ############")
                       (8 "############ You are at the Riverbank of EastFalls ############")
                       ;; Horde camp
                       (9 "############ Now you are at LightShore ############")
                       (10 "############ You are at a Fel Cave ############")
                       (11 "############ You are at Ragnar Barracks ############")
                       
                       ;; final boss
                       (12 "You are in the Boss Room .")))

(define (make-running-total)
               (let ([n 0])
                 (lambda ()
                   (set! n (+ n 1))
                   n)))

;; commands and their decision tables that are avaliable to the users
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define pick '(((get) pick) ((pickup) pick) ((pick) pick)))
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define vendor '(((vendor) vendor) ((buy) vendor) ((purchase) vendor)))
(define inventory '(((inventory) inventory) ((bag)  inventory)))
(define fight '(((fight) fight) ((kill)  fight) ((hit)  fight)))
(define status '(((killed) status)  ((deaths)  status)))
(define actions `(,@look ,@quit ,@pick ,@put ,@vendor ,@fight ,@status ,@inventory))

;; decision tables for the areas
(define decisiontable `((1 ((storm city) 2) (( eastfall) 6) ,@actions)
                        
                        (2 ((elwen forest) 1) ((storm dungeon) 3)  ,@actions)
                        (3 ((storm city) 2) ((prisoners cell) 4) ((execution room) 5) ,@actions)
                        (4 ((storm dungeon) 3)  ,@actions)
                        (5 ((storm dungeon) 3) ,@actions)
                        
                        (6 ((storm city) 2) ((eastfalls camp) 7) ,@actions)
                        (7 ((eastfall) 6) ((riverbank) 8) ,@actions)
                        (8 ((eastfalls camp) 7) ((light shore) 9) ,@actions)
                        
                        (9 ((riverbank) 8) ((fel cave) 10) ((ragnar barracks) 11) ,@actions)
                        (10 ((light shore) 9) ,@actions)
                        (11 ((light shore) 9) ((Boss Room) 12) ,@actions)
                        (12 ((light shore 9) ,@actions))))

;; defines the database (hash tables)
;; creates the object database
(define objectdb (make-hash))
;; creates the inventory database
(define inventorydb (make-hash))
;; creates the monster database
(define monsterdb (make-hash))



;;defining how to add objects into the database
;; this is used to add objects into a room or into the inventory
(define (add-object db id object)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons object record)))
      (hash-set! db id (cons object empty))))
;; defining the function to add the object using columns to be able to chose the rooms etc.
(define (add-objects db type)
       (cond ((equal? type 1)
           (for-each
            (lambda (r)
              (add-object db (first r) (second r))) objects))
           ((equal? type 2)
              (for-each
               (lambda (r)
                 (add-object db (first r) (second r))) monsters))))

;; adds the object database
(add-objects objectdb 1)
;; addst the monsters into the db.
(add-objects monsterdb 2)

;; displaying the objects or monsters, type 1 for items and type 2 for monsters

(define (display-objects db id type)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (output (string-join record " and ")))
      (when (not (equal? output ""))
        (cond ((equal? type 1)
        (if (eq? id 'bag)
            (printf "You are carrying ~a.\n" output)
            (printf "You can see ~a.\n" output)))
              ((equal? type 2)
         (if(eq? id 'npc)
            (printf "You have killed ~a. \n" output )           
            (printf "You can see ~a.\n" output))))))))

;; removes object or monsters from room
(define (remove-object-from-room db id str type)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result))
           (rnd (random 10)))
      (cond ((null? item)
             (printf "i dont see  in that room \n"))
             (else
              ;; type 1 is to add an item into your inventory
              (cond ((equal? type 1)
                      (printf "Added ~a to your bag.\n" (first item))
                      (add-object inventorydb 'bag (first item))
                      (hash-set! db id result))
                    ;; type 2 is used for monsters
                    ((equal? type 2)             
                     (printf "~a  Destroyed \n" (first item))
                     (add-object monsterdb 'npc (first item))
                     (add-object monsterdb 'kills (first item))
                     ;; undead drops an item required to kill final boss
                     (cond ((equal? "undead" (first item))
                            (add-object inventorydb 'bag "Fel Sword")
                            (printf "Looted from ~a, a Fel Sword been added to your bag.\n" (first item)))
                           ;; each monster drops a gold coin when killed
                           ((equal? rnd 1)     
                            (add-object inventorydb 'bag "Gold Coin")
                            (printf "Looted from ~a, a Gold Coin been added to your bag.\n" (first item)))
                           (else
                            (printf "No loot from this npc \n")))      
                     (hash-set! db id result))
                    ((equal? type 3)
                     (printf "Removed ~a from your bag.\n" (first item))
                     (add-object objectdb id (first item))
                     (hash-set! db 'bag result))))))))

;; checks the inventory to see if user has specific item to drop or to kill an npc
(define (inventory_check db id str npc type)
  (when (hash-has-key? db 'bag)
    (let* ((record (hash-ref db 'bag))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item)
             (printf "you need ~a " str))
            (else
             ;;type 1 for npc, if user has that item they can kill npc
             (cond ((equal? type 1)
                    (remove-object-from-room monsterdb id npc 2))
                   ;; removes an item from bag
                   ((equal? type 2)
                    (printf "Removed ~a from your bag.\n" (first item))
                    (add-object objectdb id (first item))
                    (hash-set! db 'bag result))
                   ;;vendor, removes an item and drops Fel sword
                   ((equal? type 3)
                    (printf "Removed ~a from your bag.\n" (first item))
                    (add-object objectdb id "Fel Sword")
                    (printf "The Vendor dropped a Fel Sword \n")
                    (hash-set! db 'bag result))))))))
            
(define (pick-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-room objectdb id item 1)))

;; kill monster
(define (kill-monster id input)
  (let ((npc (string-join (cdr (string-split input)))))
    (cond ((equal? "wolf" npc)
           (inventory_check inventorydb id "Silver Dagger" npc 1))
          ((equal? "orc" npc)
           (inventory_check inventorydb id "Silver Sword" npc 1))
          ((equal? "undead" npc)
           (inventory_check inventorydb id "Holy Mace" npc 1))
          ((equal? "gulda" npc)
           (inventory_check inventorydb id "Fel Sword" npc 1))
          (else     
           (remove-object-from-room monsterdb id npc 2)))))

(define (put-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (inventory_check inventorydb id item 0 2)))

(define (vendor-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (inventory_check inventorydb id "Gold Coin" 0 3)))

;;displaying kills
(define (display type)
  (cond ((equal? type 1)
        (display-objects monsterdb 'npc 2))
        ((equal? type 2)
        (display-objects inventorydb 'bag 1))))


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

(define (asso-ref assqlist id type)
  (cond ((equal? type 1)
         (cdr (assq id assqlist)))
        ((equal? type 2)
         (cdr (assv id assqlist)))))

(define (get-description id)
  (car (asso-ref descriptions id 1)))

(define (get-keywords id)
  (let ((keys (asso-ref decisiontable id 1)))
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
  (let* ((record (asso-ref decisiontable id 2))
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
      (display-objects monsterdb id 2)      
      (display-objects objectdb id 1))    
    (printf "> ")
    (let* ((input (string-downcase(read-line)))
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
              ((eq? response 'vendor)
               (vendor-item id input)
               (loop id #f))
              ((eq? response 'drop)
               (put-item id input)
               (loop id #f))
               ((eq? response 'fight)
               (kill-monster id input)
               (loop id #f))
              ((eq? response 'inventory)
               (display 2)
               (loop id #f))
              ((eq? response 'status)
               (display 1)
               (loop id #f))
              ((eq? response 'quit)
               (printf "So Long, and Thanks for All the Fish...\n")
               (exit)))))))

(add-object inventorydb 'bag "Welcome Token")
(startgame 1)

;;-------------------- List --------------------
;; Predicates on Lists
;; consp function -> check if argument is a cons cell
(consp "aa")              ;= nil
(consp '(1 . 3))          ;= t
;; atom function -> check if argument is an atom
(atom "a")                ;= t
(atom '(1 . "a"))         ;= nil
(not (consp "a"))         ;= t
;; listp function -> check if argument is a cons cell
(listp '(1))              ;= t
(listp '(1 3))            ;= t
(listp '())               ;= t
;; nlistp function -> check if argument is not a list
(nlistp 1)                ;= t
(not (nlistp "1"))        ;= nil
;; null function -> check if argument is 'nil'
(null 1)                  ;= nil
(null nil)                ;= t
(null '(1))               ;= nil
(null '())                ;= t

;;List Elements
;; car function -> return the value referred to by the first slot of the cons cell
(car '(1 . 3))                 ;= 1
(car '(a b c))                 ;= a
(car '())                      ;= nil
;; cdr function -> return the value referred to by the first slot of the cons cell
(cdr '(3 . 4))                 ;= 4
(cdr '(a b c))                 ;= (b c)
(cdr '())                      ;= nil
;; car-safe function -> take the CAR of a cons cell while avoiding errors for other data types.
(car-safe 1)                   ;= nil
(car "a")                      ; error!
(car-safe "a")                 ;= nil
(car-safe '(5 . 3))            ;= 5
;; cdr-safe function -> take the CDR of a cons cell while avoiding errors for other data types.
(cdr "a")                      ; error!
(cdr-safe "a")                 ;= nil
(cdr-safe '(5 . 3))            ;= 3
;; nth function -> return the Nth element of LIST
(nth 2 '(1 2 3 4))             ;= 3
(nth 10 '(1 2 3 4))            ;= nil
(car (nthcdr 2 '(1 2 3 4)))    ;= 3
;; nthcdr function -> return the Nth CDR of LIST
(nthcdr 1 '(1 2 3 4))          ;= (2 3 4)
(nthcdr 10 '(1 2 3 4))         ;= nil
(nthcdr 0 '(1 2 3 4))          ;= (1 2 3 4)
;; last function -> return the last link of LIST
(last '(1 3 5 7))              ;= 7
(last '(1 3 5 7) 2)            ;= (5 7)
(last '(1 3 5 7) 0)            ;= nil
(last '(1 3 5 7) 12)           ;= (1 3 5 7)
(last '())                     ;= nil
;; safe-length function -> return length of LIST with no risk of either an error or an infinite loop.
(safe-length 1)                ;= 0
(length 1)                     ; error !
(safe-length '(1 2 3 4))       ;= 4
;; caar function -> the same as (car (car CONS-CELL))
(caar '((b) a ))               ;= b
;; cadr function -> the same as (car (cdr CONS-CELL)) or (nth 1 CONS-CELL)
(cadr '(a (b c)))              ;= (b c)
;; cdar function -> the same as (cdr (car CONS-CELL))
(cdar '((a c) b))              ;= (c)
;; cddr function -> the same as (cdr (cdr CONS-CELL))
(cddr '((a c) b e))            ;= (e)
;; butlast function -> return the last X with the last element or the last N elements, removed.
(butlast '(1 2 3 4 5))         ;= (1 2 3 4)
(butlast '(1 2 3 4 5) 2)       ;= (1 2 3)
;; nbutlast function -> a version of 'butlast'
(nbutlast '(1 2 3 4 5))        ;= (1 2 3 4)
;; pop macro -> provide a convenient way to examine the CAR of a list, and take it off the list, all at once.
(pop x)
;; equivalent to
(prog1 (car listname) (setq listname (cdr listname)))

;; Building Lists
;; cons function -> create a new cons cell, make arg1 the CAR, and arg2 the CDR
(cons 1 '(2))                  ;= (1 2)
(cons 1 '())                   ;= (1)
(cons 1 2)                     ;= (1 . 2)
;; list function -> create a list with OBJECTS as its elements
(list 1 2 3 4 5)               ;= (1 2 3 4 5)
(list 1 2 '(3 4 5) 'foo)       ;= (1 2 (3 4 5) foo)
(list)                         ;= nil
;; make-list function -> creates a list of LENGTH elements
(make-list 3 'pigs)            ;= (pigs pigs pigs)
(make-list 0 'pigs)            ;= nil
(setq l (make-list 3 '(a b)))  ;=((a b) (a b) (a b))
(eq (car l)(cadr l))           ;= t
;; append function -> return a list containing all the elements of SEQUENCES(must be list)
(setq ll (make-list 2 '(a b)))
(append '(1 2 3) ll)           ;= (1 2 3 (a b) (a b))
(append [a b] "cd" nil)        ;= (a b 99 100)
(apply 'append '((a b c) nil (x y z))) ;= (a b c x y z) with 'apply', we can append all the lists in a list of lists
(append)                       ;= nil
(append '(x y) 'z)             ;= (x y . z)
(append '(x y) [z])            ;= (x y . [z])
;; copy-tree function -> retuan a copy of the tree
(copy-tree '(1 2 (3 4) (2. 3)))    ;= (1 2 (3 4) (2 3))
(copy-tree '(1 2 (3 4) (2. 3)) t)  ;= (1 2 (3 4) (2 3)) so...what is different?
;; number-sequence function -> return a list of numbers [START.....END]
(number-sequence 4 9)          ;= (4 5 6 7 8 9)
(number-sequence 9 4 -1)       ;= (9 8 7 6 5 4)
(number-sequence 8)            ;= (8)
(number-sequence 5 8 -1)       ;= nil
(number-sequence 1.5 6 2)      ;= (1.5 3.5 5.5)

;; Modifying List Variables
;; push macro -> create a new list whose CAR is ELEMENT and whose CDR is the list
(setq l '(a b))                ;= (a b)
(push 'c l)                    ;= (c a b)
(pop l)                        ;= c
;; add-to-list function -> set the variable SYMBOL by consing ELEMENT onto the old value(if ElEMENT is not already a member of that value)
(setq foo '(a b))              ;= (a b)
(add-to-list 'foo 'c)          ;= (c a b)
(add-to-list 'foo 'b)          ;= (c a b)
;; add-to-ordered-listp function -> set the variable SYMBOL
(setq foo '())                 ;= nil
(add-to-ordered-list 'foo 'a 1);= (a)
(add-to-ordered-list 'foo 'c 3);= (a c)
(add-to-ordered-list 'foo 'b 2);= (a b c)
(add-to-ordered-list 'foo 'b 4);= (a c b)
(add-to-ordered-list 'foo 'd)  ;= (a c b d)
(add-to-ordered-list 'foo 'e)  ;= (a c b e d)

;; Modifying Lists
;; setcar function -> sotre OBEJCT as the new CAR of CONS replacing its previous CAR
(setq x '(1 2))                ;= (1 2)
(setcar x 4)                   ;= 4
x                              ;= (4 2)
;; when a cons cell is part of the shared structure of serveral lists.
;; storing a new CAR into the cons changes one element of each of these lists:
(setq x1 '(a b c))             ;= (a b c)
(setq x2 (cons 'z (cdr x1)))   ;= (z b c)
(setcar (cdr x1) 'foo)
x1                             ;= (a foo c)
x2                             ;= (z foo c)
;; Replace the CAR of a link that is not shared
(setcar x1 'baz)               ;= baz
x1                             ;= (baz foo c)
x2                             ;= (z foo c)

;; setcdr function -> stores OBJECT as the new CDR of CONS, replacing its previous CDR.
(setq x '(1 2 3))              ;= (1 2 3)
(setcdr x '(4))                ;= (4)
x                              ;= (1 4)
;; You can delete elemnts froom the middle of a list by altering the CDRs of the cons cells in the list.
(setq x1 '(a b c))             ;= (a b c)
(setcdr x1 (cdr (cdr x1)))     ;= (c)
x1                             ;= (a c)
;; It is equally easy to inert a new elment by changing CDRs
(setq x1 '(a b c))             ;= (a b c)
(setcdr x1 (cons 'd (cdr x1))) ;=(d b c)
x1                             ;=(a d b c)

;; Functions that Rearrange Lists
;; nconc function -> return a list containing all the elements of LISTS
(setq x '(1 2 3))              ;= (1 2 3)
(nconc x '(4 5))               ;= (1 2 3 4 5)
x                              ;= (1 2 3 4 5)
;; The last argument of 'nconc' is not itself modified:
(setq x1 '(1 2 3))             ;= (1 2 3)
(nconc x1 'z)                  ;= (1 2 3 . z)
x1                             ;= (1 2 3 . z)

;; Sets And Lists
;; memq function -> test to see wether OBJECT is a member of LIST, return a list starting with the first occurrence of OBJECT.
(memq 'b '(a b c b a))               ;= (b c b a)
(memq '(2) '((1) (2)))               ;= nil '(2)' and '(2)' are not 'eq'
;; delq function -> removes all elements 'eq' to OBJECT from list, and return resulting list.
(delq 'a '(a b c))                   ;= (b c)
(delq 'b '(a b c))                   ;= (a c)
;; remq function -> return a copy of LIST, with all elements removed which are 'eq' to OBEJCT
(setq sample-list '(a b c a b c))    ;= (a b c a b c)
(remq 'a sample-list)                ;= (b c b c)
sample-list                          ;= (a b c a b c)
;; memql function -> compare members with OBEJCT use 'eql', and return a list starting with the first occurrence of OBJECT.
(memql 1.2 '(1.1 1.2 1.3))           ;= (1.2 1.3)
(memq 1.2 '(1.1 1.2 1.3))            ;= nil
;; member function -> see weather OBJECT is a member of LIST(using 'equal')
(member '(2) '((1) (2)))             ;= ((2))
(memq '(2) '((1) (2)))               ;= nil
(member "foo" '("foo" "bar"))        ;= ("foo" "bar")
;; delete function -> uses 'equal' to compare elements with OBJECT, then cut the element as 'delq' would
(setq l '((2) (1) (2)))              ;= ((2) (1) (2))
(delete '(2) l)                      ;= (1)
;; remove function -> non-desctructive counterpart of 'delete'
(remove '(2) '((2) (1) (2)))         ;= ((1))
(remove '(2) [(2) (1) (2)])          ;= [(1)]
;; member-ignore-case function -> like 'member' but ignores letter-case
(member-ignore-case "A" '("b" "a" "c")) ;= ("a" "c")
;; delete-dups function -> Remove all 'equal' duplicates from LIST
(setq l '((2) (1) (2) (2)))
(delete-dups l)                      ;= ((2) (1) (2))
l                                    ;= ((2) (1))

;; Association Lists
;; assoc function -> return the first association for KEY in ALIST
(setq trees '((pine . cones) (oak . acorns) (maple . seeds)))
(assoc 'oak trees)                   ;= (oak . acorns)
(setq needles-per-cluster
      '((2 "Austrian Pine" "Red Pine")
        (3 "Pitch Pine")
        (5 "White Pine")))
(cdr (assoc 3 needles-per-cluster))  ;= ("Pitch Pine")
(cdr (assoc 2 needles-per-cluster))  ;= ("Austrian Pine" "Red Pine")
;; rassoc function -> return the first association with value in ALIST
(setq trees '((pine . cones) (oak . acorns) (maple . seeds)))
(rassoc 'cones trees)                ;= (pine . cones)
;; assq function -> like 'assoc', but it makes the compare using 'eq'
(setq trees '((pine . cones) (oak . acorns) (maple . seeds)))
(assq 'pine trees)                   ;= (pine .cones)
(setq leaves
      '(("simple leaves" . oak)
        ("compound leaves" . horsechestnut)))
(assq "simple leaves" leaves)        ;= nil
;; alist-get function -> similar to 'assq', compare KEY, retuen VALUE
(setq trees '((pine . cones) (oak . acorns) (maple . seeds)))
(alist-get 'pine trees)              ;= cones
;; rassq function -> return first compare CDR 'eq' VALUE, return 'nil' if not found
(setq trees '((pine . cones) (oak . acorns) (maple . seeds)))
(rassq 'acorns trees)                ;= (oak . acorns)
(rassq 'aa trees)                    ;= nil
;; assoc-default function -> search ALIST for a match for KEY
(assoc-default 'pina trees)          ;= nil
(assoc-default 'pine trees)          ;= cones
;; copy-list function -> create a new copy of each association
(setq trees '((pine . cones) (oak . acorns) (maple . seeds)))
(copy-list trees)                    ;= ((pine . cones) (oak . acorns) (maple . seeds))
(setq copy (copy-list trees))
(eq trees copy)                      ;= nil
(equal trees copy)                   ;= t
;; assq-delete-all function -> delete from ALIST all the elements whose CAR is 'eq' to KEY, return eht shortened alist
(setq alist '((foo 1) (bar 2) (foo 3) (lose 4)))
(assq-delete-all 'foo alist)         ;= ((bar 2) (lose 4))
alist                                ;= ((foo 1) (bar 2) (lose 4))
;; rassq-delete-all function -> delete from ALIST all the elements whose CDR is 'eq' to KEY, return eht shortened alist
(setq trees '((pine . cones) (oak . acorns) (maple . seeds)))
(rassq-delete-all 'acorns trees)     ;= ((pine . cones) (maple . seeds))
trees                                ;= ((pine . cones) (maple . seeds))

;; Property Lists
;;


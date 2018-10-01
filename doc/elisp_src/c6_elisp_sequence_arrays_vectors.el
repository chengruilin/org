;;-------------------- Sequences, Arrays, and Vectors --------------------

;;Sequence Functions
;; sequencep function -> check if the OBJECT is a list, vector, string, bool-vector, or char-table.
(sequencep ?a)                     ;= nil
(sequencep "a")                    ;= t
(sequencep '(a b c))               ;= t
;;length function -> return the number of elements in SEQUENCE
(length '(1 2 3))                  ;= 3
(length ())                        ;= 0
(length "foobar")                  ;= 6
(length [1 2 3])                   ;= 3
;; elt function -> return the element of SEQUENCE indexed by INDEX
(elt [1 2 3 4] 2)                  ;= 3
(elt '(1 2 3 4) 2)                 ;= 3
(string (elt "1234" 3))            ;= "4"
(elt [1 2 3 4] 4)                  ; error! out of index.
(elt [1 2 3 4] -1)                 ; error! out of index.
;; copy-sequence function -> return a copy of SEQUENCE
(setq bar '(1 2))                  ;= (1 2)
(setq x (vector 'foo bar))         ;= [foo (1 2)]
(setq y (copy-sequence x))         ;= [foo (1 2)]
(eq x y)                           ;= nil
(equal x y)                        ;= t
;; reverse function -> create a new sequence whose elements are the elements of SEQUENCE, but in reverse order
(setq x '(1 2 3 4))                ;= (1 2 3 4)
(reverse x)                        ;= (4 3 2 1)
x                                  ;= (1 2 3 4)
(reverse "abc")                    ;= "cba"
;; nreverse function -> reverses the order of the element of SEQUENCE. the original SEQUENCE may be modified
(setq x '(a b c))                  ;= (a b c)
x                                  ;= (a b c)
(nreverse x)                       ;= (c b a)
x                                  ;= (a)
(setq x [1 2 3 4])                 ;= [1 2 3 4]
(nreverse x)                       ;= [4 3 2 1]
x                                  ;= [4 3 2 1]
;; sort function -> sort SEQUENCE stably
(sort '(1 3 4 2) '<)               ;= (1 2 3 4)
(sort '(1 3 4 2) '>)               ;= (4 3 2 1)
;; seq.el library provides the following additional sequence mainpulation macros and functions.
;; seq-elt function -> get the element by index, return places settable using 'setf'
(seq-elt [1 2 3 4] 2)              ;= 3
(setq vec [1 2 3 4])               ;= [1 2 3 4]
(setf (seq-elt vec 2) 5)           ;= 5
vec                                ;= [1 2 5 4]
;; seq-length function -> return the number of elements in SEQUENCE
(seq-length '(1 2 3 4))            ;= 4
;; seqp function -> check argument is a sequence
(seqp [1 3])                       ;= t
(seqp 2)                           ;= nil
;; seq-drop function -> return all but the first N elements of SEQUENCE
(seq-drop [1 2 3 4 5 6] 3)         ;= [4 5 6]
(seq-drop "hello world" -4)        ;= "hello world"
;; seq-take function -> return first N elements if SEQUENCE
(seq-take '(1 2 3 4) 3)            ;= (1 2 3)
(seq-take [1 2 3 4] 0)             ;= []
;; seq-take-while function -> get sequence before the first one for which PREDICATE return 'nil'
(seq-take-while (lambda (elt) (> elt 0)) '(1 2 3 -1 -2)) ;= (1 2 3)
;; seq-drop-while function -> get sequence start from the first one for which PREDICATE return 'nil'
(seq-drop-while (lambda (elt) (> elt 0)) '(1 2 3 -1 -2)) ;= (-1 -2)
;; seq-do function -> apply FUNCTION to each element of SEQUENCE
(seq-do (lambda (elt) (+ elt 2)) '(1 2 3 4 5))
;; seq-map function -> return the result of apply function to each element
(seq-map #'1+ '(2 4 5))             ;= (3 5 6)
;; seq-map-indexed function -> return result of apply function to each element and its index.
(seq-map-indexed (lambda (elt idx)
                   (list idx elt))
                 '(a b c))          ;= ((0 a) (1 b) (2 c))
;; seq-mapn function -> get result of apply FUNCTION to each element
(seq-mapn #'+ '(2 4 6) '(20 40 60)) ;= (22 44 66)
;; seq-filter function -> return a list of all the elements in SEQUENCE for PREDICATE return 'nil'
(seq-filter (lambda (elt) (> elt 0)) [1 -1 3 -3 5])      ;= (1 3 5)
;; seq-remove function -> return a list of all elements
(seq-remove (lambda (elt) (< elt 0)) '(-1 -3 -5))        ;= nil
;; seq-reduce function -> result of calling FUNCTION with INITIAL-VALUE
(seq-reduce #'+ [1 2 3 4] 0)            ;= 10
(seq-reduce #'+ [1 2 3 4] 5)            ;= 15
;; seq-some function -> return the first non- 'nil' value
(seq-some #'numberp ["abc" 1 nil])      ;= t
(seq-some #'null ["abc" 1 nil])         ;= t
;; seq-find function -> return the first element in SEQUENCE
(seq-find #'numberp ["abc" 1 nil])      ;= 1
(seq-find #'numberp ["abc" "abd"])      ;= nil
;; seq-every-p function -> return non- 'nil' if applying PREDICATE to every element
(seq-every-p #'numberp [2 4 5])         ;= t
(seq-every-p #'numberp [2 "a" 5 6])     ;= nil
;; seq-empty-p function -> return non- 'nil' if SEQUENCE is empty
(seq-empty-p "not-empty")               ;= nil
(seq-empty-p "")                        ;= t








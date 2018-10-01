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
;; seq-count function -> return the number of SEQUENCE for which PREDICATE
(seq-count (lambda (elt) (> elt 0)) [-1 2 0 3 -2])   ;= 2
;; seq-sort function -> return a copy of SEQUENCE that is sorted according to FUNCTION
(seq-sort #'> [1 3 4 2])                ;= [4 3 2 1]
;; seq-sort-by function -> similar to 'seq-sort', but the element of SEQUENCE are transformed by applying FUNCTION on them before being sorted
(seq-sort-by #'seq-length #'> ["a" "ab" "abc"])      ;= ["abc" "ab" "a"]
;; seq-contains function -> return first element in SEQUENCE that is equal to ELT
(seq-contains '(symbol1 symbol2) 'symbol1)           ;= symbol1
;; seq-set-equal-p function -> check whether SEQUENCE1 and SEQUENCE2 contain the same elements, regardless of the order.
(seq-set-equal-p '(a b c) '(c b a))                  ;= t
(seq-set-equal-p '(a b c) '(c b))                    ;= nil
;; seq-position function -> get the index of first element in SEQUENCE
(seq-position '(a b c) 'b)                           ;= 1
(seq-position '(a b c) 'd)                           ;= nil
;; seq-uniq function -> get a list of the elements of SEQUENCE with duplicates removed
(setq list '(1 2 2 1 3 3))
(seq-uniq list)                                      ;= (1 2 3)
list                                                 ;= (1 2 2 1 3 3)
;; seq-subseq function -> get a subset of SEQUENCE from START to END
(seq-subseq '(1 2 3 4 5) 1)                          ;= (2 3 4 5)
(seq-subseq '[1 2 3 4 5] 1 3)                        ;= [2 3]
;; seq-concatenate function -> get a sequence made of the concatenation of SEQUENCES
(seq-concatenate 'list '(1 2) '(3 4) [5 6])          ;= (1 2 3 4 5 6)
(seq-concatenate 'string "Hello" " " "World")        ;= "Hello World"
;; seq-mapcat function -> return the result of applying 'seq-concatenate' to the result of applying FUNCTION to each element of SEQUENCE
(seq-mapcat #'seq-reverse '((3 2 1) (6 5 4)))        ;= (1 2 3 4 5 6)
;; seq-partition function -> return a list of the element of SEQUENCE groupd into sub-sequence of length N
(seq-partition '(0 1 2 3 4 5 6 7) 3)                 ;= ((0 1 2) (3 4 5) (6 7))
;; seq-intersection function -> return a list of the elements that appear both SEQUENCES
(seq-intersection [2 3 4 5] [1 3 5 6 7])             ;= (3 5)
;; seq-difference function -> return a list of the elements appear in SEQUENCE1 but not in SEQUENCE2
(seq-difference '(2 3 4 5) [1 3 5 6 7])              ;= (2 4)
;; seq-group-by function -> separate the elements of SEQUENCE into an alist whose keys are the result of applying FUNCTION to each element of SEQUENCE.
(seq-group-by #'integerp '(1 2.1 3 2 3.2))           ;= ((t 1 3 2) (nil 2.1 3.2))
;; seq-into function -> convert the sequence into a sequence of type in ('vector', 'string', 'list')
(seq-into [1 2 3] 'list)                             ;= (1 2 3)
;; seq-min function -> return the smallest element of SEQUENCE
(seq-min [3 1 2])                                    ;= 1
;; seq-max function -> return the largest element of SEQUENCE
(seq-max [1 3 2])                                    ;= 3
;; seq-random-elt function -> return an element of SEQUENCE taken a random
(seq-random-elt [1 2 3 4])                           ;= 3
;; seq-doseq macro -> like 'dolist', except that SEQUENCE can be a lit, vector or string.
(seq-doseq (var list) '(1 2 3))                        ;= (1 2 2 1 3 3)
;; seq-let macro -> binds the variable defined in ARGUMENTS to the elements of SEQUENCE
(seq-let [first second] [1 2 3 4] (list first second)) ;= (1 2)

;; Array Functions
;; arrayp function -> check whether the argument is an array
(arrayp [a])                      ;= t
(arrayp "abc")                    ;= t
;;
(setq primes [2 3 5 7 11 13])     ;= [2 3 5 7 11 13]
(aref primes 4)                   ;= 11
(aref "abcdefg" 1)                ; 98  'b' is ASCII code 98
;; aset function -> set the INDEXth element of ARRAY to be OBJECT
(setq w [foo bar baz])            ;= [foo bar baz]
(aset w 0 'fu)
w                                 ;= [fu bar baz]
;; fillarray function -> fill the array with OBJECT
(setq a [a b c d e f g])          ;= [a b c d e f g]
(fillarray a 0)                   ;= [0 0 0 0 0 0 0]

;; Vector Functions
;; vectorp function -> return 't' if OBJECT is a vector
(vectorp [a])                     ;= t
(vectorp "abc")                   ;= nil
;; vector function -> create and return a vector
(vector 'foo 23 [bar baz] "rats") ;= [foo 23 [bar baz] "rats"
(vector)                          ;= []
;; make-vector function -> return a new vector consisting of LENGTH elements
(setq sleepy (make-vector 9 'Z))  ;= [Z Z Z Z Z Z Z Z Z]
;; vconcat function -> return a new containing all the elements of SEQUENCES
(setq a (vconcat '(A B C) '(D E F))) ;= [A B C D E F]
(eq a (vconcat a))                   ;= nil
(vconcat)                            ;= []

;; Char-Tables
;; make-char-table function -> retuan a newly-created char-table, with subtype(a symbol)
;; no argument to specify the length of the char-table, all char-table have room for any valid character code as index.
(setq a-char-table (make-char-table  'my-test-subtype 100))
;; char-table-p function -> return 't' if OBJECT is a char-table
(char-table-p a-char-table)                     ;= t
;; char-table-subtype function -> return sybtype symbol of CHAR-TABLE
(char-table-subtype a-char-table)               ;= my-test-subtype
;; set-char-table-parent function -> set the parent of CHAR-TABLE to NEW-PARENT
(setq another-char-table (make-char-table 'my-test-subtype))
(set-char-table-parent another-char-table a-char-table)
(aref a-char-table ?A)
;; char-table-parent function -> return the parent of CHAR-TABLE
(char-table-parent another-char-table)
;; char-table-extra-slot function -> return contents of extra slot
(char-table-extra-slot a-char-table 0)
;; set-char-table-extra-slot function -> store VALUE in extra slot N of CHAR-TABLE
(set-char-table-extra-slot a-char-table 0 '(haha Hello World))
;; char-table-range function -> return the value specified in CHAR-TABLE for a range of characters RANGE







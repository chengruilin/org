;;-------------------- Hash Tables --------------------
;; make-hash-table function -> create a new hash table according to the specified arguments
(setq myHash (make-hash-table
              :test 'equal
              :weakness 'key-and-value))

;; Hash Table Access
;; puthash function -> enter an association for KEY in table , with value
(puthash "key1" "value1" myHash)         ;= "value1"
;; gethash function -> looks up KEY in TABLE, return its associated VALUE
(gethash "key2" myHash)                  ;= nil
(gethash "key1" myHash)                  ;= "value1"
;; remhash function -> remove the association for KEY fron TABLE
(puthash "key2" "value2" myHash)
(gethash "key2" myHash)                  ;= "value2"
(remhash "key2" myHash)                  ;= nil
(gethash "key2" myHash)                  ;= nil
;; clrhash function -> remove all the associations fron hash table
(clrhash myHash)
;; maphash function -> call function once for each of the associations in table.
(maphash function table)

;; Defining Hash Comparisons
;; define-hash-table-test function -> define a new hash table test, you can use it as TEST argument in 'make-hash-table'
;; test-fn -> accept two arguments, two keys, return non- 'nil' if they are considered the same
;; hash-fn -> accept one argument, a key, and return an integer thar is the hash code of that key
;;(define-hash-table-test name test-fn hash-fn)
(defun case-fold-string= (a b)
  (eq t (compare-strings a nil nil b nil nil t)))
(defun case-fold-string-hash (a)
  (sxhash-equal (upcase a)))
(define-hash-table-test 'case-fold
  'case-fold-string= 'case-fold-string-hash)
(make-hash-table :test 'case-fold)
;; sxhash-equal function -> return a hash code for Lisp object
(sxhash-equal "a")            ;= 121 if two objects are 'equal', their hash code is same
;; sxhash-eq function -> return a hash code for List object
(sxhash-eq "b")               ;= 1117765081
(sxhash-eq "b")               ;= 1115129385
;; sxhash-eql function -> return a hash code for Lisp object
(sxhash-eql "b")              ;= 1106649113
(sxhash-eql "b")              ;= 1145969165
;; You can use 'sxhash-equal', 'sxhash-eq', 'sxhash-eql' like:
(define-hash-table-test 'contents-hash 'equal 'sxhash-equal)
(make-hash-table :test 'contents-hash)

;; Other Hash Table Functions
(setq myHash (make-hash-table
              :test 'equal
              :weakness 'key-and-value))
(puthash "key" "value" myHash)
(puthash "key1" "value2" myHash)
(puthash "key2" "value2" myHash)
(hash-table-p myHash)                  ;= t
(copy-hash-table myHash)               ; copy hash table
(hash-table-count myHash)              ;= 3
(hash-table-test myHash)               ;= equal
(hash-table-weakness myHash)           ;= key-and-value
(hash-table-rehash-size myHash)        ;= 1.5
(hash-table-rehash-threshold myHash)   ;= 0.8125
(hash-table-size myHash)               ;= 65




;;-------------------- Hash Tables --------------------
;; make-hash-table function -> create a new hash table according to the specified arguments
(setq myHash (make-hash-table
              :test 'equal
              :weakness 'key-and-value))


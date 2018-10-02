;;-------------------- Records --------------------

;; Record Functions
;; recordp function -> return 't' if OBJECT is a record
(recordp "a")                          ;= nil
(recordp #s(a))                        ;= t
;; record function -> create and return a record whose type is TYPE and remaining slots are the rest of the arguments, OBJECTS
(record 'foo 23 [bar baz] "rats")      ;= #s(foo 23 [bar baz] "rats")
;; make-record function -> return a new record with type TYPE and LENGTH moew slots, each initialized to OBJECT
(setq sleepy (make-record 'foo 9 'Z))  ;= #s(foo Z Z Z Z Z Z Z Z Z)


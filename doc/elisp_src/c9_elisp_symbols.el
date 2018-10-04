;;-------------------- Symbols --------------------
;; return the string that is SYMBOL's name.
(symbol-name 'foo)                 ;= "foo"
;; return a newly-allocated, uninterned symbol whose name is NAME.
(setq sym (make-symbol "foo"))     ;= foo
(eq sym 'foo)                      ;= nil
;; return a symbol using 'make-symbol', whose name is make by appending 'gsnsym-couter' to PREFIX
(gensym 'foo)                      ;= foo23
(gensym 'foo)                      ;= foo24
;; return the interned symbol by name.If no such symbol, 'intern' create a new one, add it to the obarray, and return it.
(setq sym (intern "foo"))          ;= foo
(eq sym 'foo)                      ;= t
;; return the symbol in OBARRAY by name, or 'nil' if OBARRAY has no symbol with that name
(intern-soft "frazzle")            ;= nil
(make-symbol "frazzle")            ;= frazzle
(intern-soft "frazzle")            ;= nil
(setq sym (intern "frazzle"))      ;= frazzle
(intern-soft "frazzle")            ;= frazzle
(eq sym 'frazzle)                  ;= t
;; mapatioms function -> call function once with each symbol in the obarray.
(setq count 0)
(defun count-syms (s)
  (setq count (1+ count)))         ;= count-syms
(mapatoms 'count-syms)             ;= nil
count                              ;= 69948
;; delete SYMBOL from the obarray
(unintern "foo")                   ;= t
(unintern "foo")                   ;= nil

;; Accessing Symbol Properties
;; get value of the property
(get 'foo 'prop1)                  ;= nil
;; put value of the property
(put 'foo 'prop1 'value1)          ;= value1
(get 'foo 'prop1)                  ;= value1
;; return the property list
(symbol-plist 'foo)                ;= (prop1 value1)
;; sets SYMBOL's property list to PLIST
(setplist 'foo '(a 1 b (2 3) c nil))  ;= (a 1 b (2 3) c nil)
(symbol-plist 'foo)                   ;= (a 1 b (2 3) c nil)
;; identical to 'get'
(function-get 'foo 'prop1)         ;= nil
;; identical to 'put'
(function-put 'foo 'prop2 'value2) ;= value2


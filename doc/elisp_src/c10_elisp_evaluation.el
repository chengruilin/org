;;-------------------- Evaluation --------------------

;; Self-Evaluating Forms
'123                   ;= 123
123                    ;= 123
(eval '123)            ;= 123
(eval (eval '123))     ;= 123
;; For types that lack a read syntax.
(setq print-exp (list 'print (current-buffer)))  ;= (print #<buffer c10_elisp_evaluation.el>)
(eval print-exp)

;; Symbol Forms
(setq a 123)           ;= 123
(eval a)               ;= 123
a                      ;= 123
(setq nil 23)          ; error!

;; Function Indirection
;; Build this function cell linkage:
;;   -------------       -----        -------        -------
;;  | #<subr car> | <-- | car |  <-- | first |  <-- | erste |
;;   -------------       -----        -------        -------
;; The first element is an anonymous Lisp function, not a symbol.
(symbol-function 'car)        ;= #<subr car>
(fset 'first 'car)            ;= car
(fset 'erste 'first)          ;= car
(erste '(1 2 3))              ;= 1

;; Lisp Macro Evaluation
(defmacro cadr (x)
  (list 'car (list 'cdr x)))

;; Special Forms
;; test whether its argument is a special form
(special-form-p 'car)           ;= nil
(special-form-p 'and)           ;= t

;; Quoting
(quote (+ 1 2))                 ;= (+ 1 2)
(quote foo)                     ;= foo
'foo                            ;= foo
'(+ 1 2)                        ;= (+ 1 2)

;; Backquote
'(a list of (+ 2 3) elements)   ;= (a list of (+ 2 3) elements) use quote
`(a list of ,(+ 2 3) elements)   ;= (a list of 5 elements) use backquote
`(1 2 (3 ,(+ 4 5)))              ;= (1 2 (3 9))

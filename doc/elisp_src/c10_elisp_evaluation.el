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


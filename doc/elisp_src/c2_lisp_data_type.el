;;-------------------- Numbers --------------------

;; Integer

;; all = 44
'(#b101100)
'(#o54)
'(#x2c)
'(#24r1k)

;; Float
(isnan 10.5)            ;= nil
(frexp 2.1)             ;= (0.7 . 3)
(ldexp 1.2 3)           ;= 9.6
(copysign 2.3 -2.1)     ;= -2.3
(logb 7.3)              ;= 2

;; Type Predicates
(floatp 1)              ;= nil
(integerp 20)           ;= t
(numberp 1.8)           ;= t
(natnump 29)            ;= t check natural number
(zerop 0)               ;= t

;; Comparison of Numbers
(= 2 2)         ;= t
(eql 2 3)       ;= nil
(/= 2 3)        ;= t
(/= 2 2)        ;= nil
(< 3 4)         ;= t
(<= 3 4)        ;= t
(> 4 5)         ;= nil
(>= 3 5)        ;= nil
(max 2 4 10 9)  ;= 10
(min 2 4 10 9)  ;= 2
(abs -9)        ;= 9

;; Numeric Conversions
(float 2)       ;= 2.0
(truncate 2.23) ;= 2
(floor 3.65)    ;= 3
(floor -1.7)    ;= -2
(ceiling 1.2)   ;= 2
(ceiling -2.9)  ;= -2
(round 1.2)     ;= 1
(round -1.2)    ;= -1
(round -1.7)    ;= -2

;; Arithmetic Operations




;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------

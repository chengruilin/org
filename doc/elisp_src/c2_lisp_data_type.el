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
; 1+ function -> NUMBER plus 1
(setq foo 3)
(1+ foo)
(setq foo (1+ foo)) ;; like ++
;; 1- function -> NUMBER minus 1
(setq foo 4)
(1- foo)
(setq foo (1- foo)) ;; like --
;; - function -> negation and subtraction
(- 10 1 2 3 4)      ;= 0
(- 10)              ;= -10
(-)                 ;= 0
;; * function -> multiplies
(*)                 ;= 1
(* 1)               ;= 1
(* 1 2 3 4)         ;= 1
;; / function -> divides
(/ 6 2)             ;= 3
(/ 5 2)             ;= 2
(/ 5.0 2)           ;= 2.5
(/ 5 2.0)           ;= 2.5
(/ 5.0 2.0)         ;= 2.5
(/ 4.0)             ;= 0.25
(/ 4)               ;= 0
(/ 25 3 2)          ;= 4
(/ -17 6)           ;= -2
;; % function
(% 9 4)             ;= 1
(% -9 4)            ;= -1
(% 9 -4)            ;= 1
(% -9 -4)           ;= -1
;; mod function
(mod 9 4)           ;= 1
(mod -9 4)          ;= 3
(mod 9 -4)          ;= -3
(mod -9 -4)         ;= -1
(mod 5.5 2.5)       ;= 0.5

;;Rounding Operations
(ffloor 2.52)       ;= 2.0
(fceiling 2.15)     ;= 3.0
(ftruncate 3.9)     ;= 3.0
(fround 3.55)       ;= 4.0

;; Bitwise Operations
;; lsh function -> shifts bits in INTEGER to the left COUNT places
(lsh 5 1)                     ;= 10 00000101 -> 00001010 = 5 * 2^1
(lsh 7 1)                     ;= 14 00000111 -> 00001110 = 7 * 2^1
(lsh 3 2)                     ;= 12 00000110 -> 00001100 = 3 * 2^2
(lsh most-positive-fixnum 1)  ;= -2 0111.......1111 -> 1111......111110
;; ash function -> shifts bits in INTEGER to the left COUNT places, or to the right if COUNT is negative
(ash -6 -1)       ;= -3 1111...11010 -> 1111...111101
(ash 2 3)         ;= 16 000010 -> 010000
;; logand function -> return bitwise AND the arguments
(logand 13 12)    ;= 12 1101 AND 1100 -> 1100
(logand 19 12)    ;= 0 10011 AND 1100 -> 00000
;; logior function -> return bitwise inclusive OR of its arguments
(logior 12 5)     ;= 13 1100 OR 0101 -> 1101
(logior 12 5 7)   ;= 15 1100 OR 0101 OR 0111 -> 1111
;; logxor function -> return bitwise exclusive OR of its arguments
(logxor 12 5)     ;= 9 1100 XOR 0101 -> 1001
(logxor 12 5 7)   ;= 14 1100 XOR 0101 XOR 0111
;; lognot function -> return bitwise complement of its argument
(lognot 5)        ;= -6 0000...000101 -> 1111...111010 (30 bits total)


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

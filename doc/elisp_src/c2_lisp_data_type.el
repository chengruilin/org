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

;; Math Functions
;; Argument为弧度，角度 != 弧度
;; 弧度 = (PI * 角度) / 180.0
(/ (* float-pi 90) 180)   ;= 1.572693
(sin 1.572963)            ;= 0.99999 近似于 sin90度 = 1
(cos 1.572963)            ;= 0.002   近似与 cos90度 = 0
(/ (* float-pi 45) 180)   ;= 0.7864815
(tan 0.7864815)           ;= 1.002 近似于 tan45度 = 1
;;asin function -> X > -PI/2 && X < PI/2 && sinX=Y -> (asin Y) = X (Y超过[-1,1]返回NaN)
(asin 1)                  ;= 1.57
;;acos function -> X > 0 && X < PI && cosX=Y -> (acos Y) = X (Y超过[-1,1]返回NaN)
(acos 0)                  ;= 1.57
;;atan function -> X > -PI/2 && X < PI/2 && atanX=Y -> (atan Y) = X
(atan 1)                  ;= 0.78
(atan 1 2)                ;= 0.46 is the angle in radians between the vector [1, 2] and the 'X' axis
;; exp function
(exp 1)                   ;= 2.718 return
;; log function
(log 4)
;; expt function -> return X reised to power Y
(expt 3 2)
;; sqat
(sqrt -10)

;; Random Numbers
;; random function -> return random number in interval [0,LIMIT]
(random)            ; = X X >=0 && X < 100
(random 100)        ; = X X > most-negative-fixnum && X < most-positive-fixnum

;;-------------------- Strings and Characters --------------------

;; Predicates for Strings
;; stringp function -> check argument is a string nor not
(stringp "asd")    ;= t
(stringp 1)        ;= nil
(stringp nil)      ;= nil
(stringp 'a')      ; error!
;; string-or-null-p function -> check argument is a string or 'nil'
(string-or-null-p nil)    ;= t
(string-or-null-p "aab")  ;= t
(string-or-null-p 12)     ;= nil
;; char-or-string-p function -> check argument is a string or a character(i.e., an integer)
(char-or-string-p "asd")  ;= t
(char-or-string-p 12)     ;= t
(char-or-string-p ?a)     ;= t
(char-or-string-p nil)    ;= nil

;; Creating Strings
;; make-string function -> made up of COUNT repetition of CHARACTER. COUNT must >= 0
(make-string 4 ?x)                          ;= "xxxx"
(make-string 0 ?x)                          ;= ""
(make-string -1 ?)                          ; error!
;; string function -> contain the characters
(string ?a ?b ?c)                           ;= "abc"
;; substring function -> return a new string consists of those characters[START, END]
(substring "abcdefg" 0 3)                   ;= "abc"
(substring "abcdefg")                       ;= "abcdefg" just copy the string
(substring "abcdefg" -3 -1)                 ;= "ef" A negative number counts from the end of the string
(substring "abcdefg" -3 nil)                ;= "efg" 'nil' used for END, it stands for the length for the string.
(substring "abcdefg" 0)                     ;= "abcdefg" retun a copy of string
(substring [a b (c)] 1 3)                   ;= [b (c)] also accepts a vector for the first argument
;; substring-no-properties function -> works like substring, but discards all text properties
(substring-no-properties "abcdefg" 0 3)     ;= "abc"
;; concat function -> return a new string consisting of the characters in the arguments passed to it
(concat "abc" "-def")                       ;= "abc-def"
(concat "abc" (list 120 121) [122])         ;= "abcxyz"
(concat "abc" nil "-def")                   ;= "abc-def" 'nil' is an empty sequence
(concat)                                    ;= ""
;; splite-string function -> splits STRING into substrings
(split-string "  tow words ")               ;= ("two" "words")
(split-string "  tow words " split-string-default-separators) ;=("" "two" "words" "")
(split-string "Soup is good food" "o")      ;= ("S" "up is g" "" "d f" "" "d")
(split-string "Soup is good food" "o" t)    ;= ("S" "up is g" "d f" "d")
(split-string "Soup is good food" "o+")     ;= ("S" "up is g" "d f" "d")
(split-string "aooob" "o*")                 ;= ("" "a" "" "b" "")
(split-string "ooaboo" "o*")                ;= ("" "" "a" "b" "")
(split-string "" "")                        ;= ("")
(split-string "Soup is good food" "o*" t)   ;= ("S" "u" "p" " " "i" "s" " " "g" "d" " " "f" "d")
(split-string "Nice doggy!" "" t)           ;= ("N" "i" "c" "e" " " "d" "o" "g" "g" "y" "!")
(split-string "ooo" "o*" t)                 ;= nil
(split-string "ooo" "\\|o+" t)              ;= ("o", "o", "o")

;; Modifying Strings
;; store-substring function -> alters part of string by index and replace string
(store-substring "asdoasdfoa" 0 "insert")   ;= "insertdfoa"
;; clear-string function -> make string a unibyte string and clear its contents to zeros. length change.
(clear-string "aaa")                        ;= nil
(length (clear-string "abcdefg"))           ;= 0

;; Text Comparison
;; char-equal function -> check arguments represent the sam character
(char-equal ?x ?a)                   ;= nil
(char-equal ?x ?x)                   ;= t
(let ((case-fold-search nil))
  (char-equal ?x ?X))                ;= nil ignore differences in case if 'case-fold-search' is non- 'nil'
;; string= function -> check if the two strings match exactly
(string= "asdf" "asdf")              ;= t
(string= "asdf" "asd")               ;= nil
(string= "asdf" "ASDF")              ;= nil
;; string-equal -> another name for 'string='
(string-equal "aaa" "aaa")           ;= t
(string-collate-equalp "a" "a")      ;= t
(string-collate-equalp (string ?\uFF40) (string ?\u1FEF)) ;= t characters with different coding points but the same mean might be considered as equal. Do not use this function compare file names.
;; string< function -> compare two strings a character at a time
(string< "aa" "aa")                  ;= nil
(string< "aa" "ac")                  ;= t
(string< "" "abc")                   ;= t
;; string-lessp function -> another name for 'string<'
(string-lessp "aaa" "bbb")           ;= t
(string-greaterp "cc" "aa")          ;= t compare in the opposite order, equivalent to (string-lessp "aa" "cc")
;; string-collate-lessp function -> compare two strings in collation order
(sort '("11" "12" "1 1" "1 2" "1.1" "1.2") 'string-collate-lessp) ;= ("11" "1 1" "1.1" "12" "1 2" "1.2")
;; string-version-lessp function -> compare numbers in two strings
(string-version-lessp "foo2.png" "foo12.png") ;= t
;; string-prefix-p function -> check if string1 is a prefix of string2
(string-prefix-p "A" "Abc")          ;= t
(string-prefix-p "a" "Abc")          ;= nil
(string-prefix-p "a" "Abc" t)        ;= t you can add 'ignore-case' argument
;; string-suffix-p function -> check if string1 is a suffix of string2
(string-suffix-p "C" "abC")          ;= t
(string-suffix-p "c" "abC")          ;= nil
(string-suffix-p "c" "abC" t)        ;= t you can add 'ignore-case' argument
;; compare-strings function -> strings are compared by the numberic values of their characters
(compare-strings "abcdefg" 1 3 "bbbbabc" 5 7)                 ;= t "bc" compare with "bc"
(compare-strings "abcdefg" 1 3 "bbbbabc" 1 3)                 ;= 2 "bc" compare with "bb"
(compare-strings "abcc" 1 3 "bcbc" 1 3)                       ;= -1 "bc" compare with "cb"
(compare-strings "abcccccc" 1 5 "ZZZZZZZ" 2 4)                ;= 1
(compare-strings "abcccccc" 1 5 "zzzzzzz" 2 4 t)              ;= -1
;; assoc-string function -> like function 'assoc'
(assoc-string "key" (list "keys" "aaa" "key"))                ;= "key"
(assoc-string "key" (list "keys" "aaa" "KEY"))                ;= nil
(assoc-string "key" (list "keys" "aaa" "KEY") t)              ;= "key"





;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------

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
(eq "2" "2")    ;= nil
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

;; String conversion
;; number-to-string function -> convert a NUMBER to string
(number-to-string 222)               ;= "222"
(number-to-string 0222)              ;= "222"
(number-to-string -22)               ;= "-22"
(int-to-string 22)                   ;= "22" a semi-obsolete alias for this function
;; string-to-number function -> return the numeric value of the characters in STRING
(string-to-number "256")             ;= 256
(string-to-number "256 is a number") ;= 256
(string-to-number "Number 22")       ;= 0
(string-to-number "-4.5")            ;= -4.5
;; char-to-string function
(char-to-string ?x)                  ;= "x"
;; string-to-char function -> return the first character in STRING (a integer value)
(string-to-char "")                  ;= 0
(string-to-char "a")                 ;= 97
;; concat function -> vonverts a vector or a list into a string
(concat "aaa" "bbb" "ccc")           ;= "aaabbbccc"
(concat "list-" (list 97 98 99 100)) ;= "list-abcd"
;; vconcat function -> vonvert a string into a vector
(vconcat "abc")                      ;= [97 98 99]
;; append function -> convert a string into a list
(append "cba" (list "a" "b"))        ;= (99 98 97 "a" "b")
;; byte-to-string function -> convert a byte to unibyte string
(byte-to-string 98)                  ;= "b"

;; Formatting Strings
;; format function -> return a string equal to STRING
(progn
  (setq x "foo")
  (eq x (format "%s" x)))                               ;= t object x and (format "%s" x) is the same object
;; %s -> Replace, Without quoting
(format "%s is his name." "Tom")                        ;= "Tom is his name"
;; %S -> Replace, With quoting
(format "%S is his name." "Tom")                        ;= "\"Tom\" is his name"
;; %o -> Replace, with base-eight representation of a unsigned integer
(format "The value is %o." 10)                         ;= "The value is 12."
;; %d -> Replace, with base-ten representation of a signed integer
(format "The value is %d." 10)                         ;= "The value is 10."
;; %x -> Replace, with base-sixteen representation of an unsigned integer, lower case
(format "The value is %x." 12)                         ;= "The value is c."
;; %X -> Replace, with base-sixteen representation of an unsigned integer, upper case
(format "The value is %X." 90)                         ;= "The value is 5A."
;; %c -> Replace, with character
(format "The value is %c." 65)                         ;= "The value is A."
;; %e -> Replace, with exponential notation for a floating-point number
(format "The value is %e." 65000000)                   ;= "The value is 6.500000e+07"
;; %f -> Replace, with decimal-point notation for a floating-point number
(format "The value is %f." 65.23)                      ;= "The value is 65.230000."
;; %% -> Replace, with a single '%'
(format "The value is %%%d." 65)                       ;= "The value is %65."

(format "%2$s, %3$s, %%, %1$s" "x" "y" "z")            ;= "y, z, %, x"
(format "%06d is padded on the left with zeros" 123)   ;= "000123 is padded on the left with zeros"
(format "'%-6d' is padded on the right" 123)           ;= "'123   ' is padded on the right"
(format "The word '%-7s' actually has %d letters in it."
        "foo" (length "foo"))                          ;= "The word 'foo    ' actually has 3 letters in it."
(format "%5d is padded on the left with spaces" 123)   ;= "  123 is padded on the left with spaces"
(format "The word '%7s' has %d letters in it."
        "foo" (length "foo"))                          ;= "The word '    foo' has 3 letters in it."
(format "The word '%7s' has %d letters in it."
        "specification" (length "specification"))      ;= "The word 'specification' has 13 letters in it."
;; format-message function -> like 'format'
(format-message
 "The name of this buffer is ‘%s’." (buffer-name))   ;= "The name of this buffer is ‘c2_lisp_bdata_type.el’."

;; Case Conversion
;; downcase function -> convert string or char to lower case
(downcase "AAAbbbCCC")                      ;= "aaabbbccc"
(print ?X)                                  ;= 88
(downcase ?X)                               ;=120
;; upcase function -> convert string or char to upper case
(upcase "aaaBBBccc")                        ;="AAABBBCCC"
(print ?X)                                  ;= 88
(upcase ?x)                                 ;= 88
;; capitalize function -> capitalize the word of strings
(capitalize "the cat in the hat")           ;= "The Cat In The Hat"
(capitalize ?x)                             ;= 88
;; capitalize-initials function
(upcase-initials "The CAT in the hAt")      ;= "The CAT In The HAt"

;;-------------------- List --------------------
;; Predicates on Lists
;; consp function -> check if argument is a cons cell
(consp "aa")              ;= nil
(consp '(1 . 3))          ;= t
;; atom function -> check if argument is an atom
(atom "a")                ;= t
(atom '(1 . "a"))         ;= nil
(not (consp "a"))         ;= t
;; listp function -> check if argument is a cons cell
(listp '(1))              ;= t
(listp '(1 3))            ;= t
(listp '())               ;= t
;; nlistp function -> check if argument is not a list
(nlistp 1)                ;= t
(not (nlistp "1"))        ;= nil
;; null function -> check if argument is 'nil'
(null 1)                  ;= nil
(null nil)                ;= t
(null '(1))               ;= nil
(null '())                ;= t

;;List Elements
;; car function -> return the value referred to by the first slot of the cons cell
(car '(1 . 3))                 ;= 1
(car '(a b c))                 ;= a
(car '())                      ;= nil
;; cdr function -> return the value referred to by the first slot of the cons cell
(cdr '(3 . 4))                 ;= 4
(cdr '(a b c))                 ;= (b c)
(cdr '())                      ;= nil
;; car-safe function -> take the CAR of a cons cell while avoiding errors for other data types.
(car-safe 1)                   ;= nil
(car "a")                      ; error!
(car-safe "a")                 ;= nil
(car-safe '(5 . 3))            ;= 5
;; cdr-safe function -> take the CDR of a cons cell while avoiding errors for other data types.
(cdr "a")                      ; error!
(cdr-safe "a")                 ;= nil
(cdr-safe '(5 . 3))            ;= 3
;; nth function -> return the Nth element of LIST
(nth 2 '(1 2 3 4))             ;= 3
(nth 10 '(1 2 3 4))            ;= nil
(car (nthcdr 2 '(1 2 3 4)))    ;= 3
;; nthcdr function -> return the Nth CDR of LIST
(nthcdr 1 '(1 2 3 4))          ;= (2 3 4)
(nthcdr 10 '(1 2 3 4))         ;= nil
(nthcdr 0 '(1 2 3 4))          ;= (1 2 3 4)
;; last function -> return the last link of LIST
(last '(1 3 5 7))              ;= 7
(last '(1 3 5 7) 2)            ;= (5 7)
(last '(1 3 5 7) 0)            ;= nil
(last '(1 3 5 7) 12)           ;= (1 3 5 7)
(last '())                     ;= nil
;; safe-length function -> return length of LIST with no risk of either an error or an infinite loop.
(safe-length 1)                ;= 0
(length 1)                     ; error !
(safe-length '(1 2 3 4))       ;= 4
;; caar function -> the same as (car (car CONS-CELL))
(caar '((b) a ))               ;= b
;; cadr function -> the same as (car (cdr CONS-CELL)) or (nth 1 CONS-CELL)
(cadr '(a (b c)))              ;= (b c)
;; cdar function -> the same as (cdr (car CONS-CELL))
(cdar '((a c) b))              ;= (c)
;; cddr function -> the same as (cdr (cdr CONS-CELL))
(cddr '((a c) b e))            ;= (e)
;; butlast function -> return the last X with the last element or the last N elements, removed.
(butlast '(1 2 3 4 5))         ;= (1 2 3 4)
(butlast '(1 2 3 4 5) 2)       ;= (1 2 3)
;; nbutlast function -> a version of 'butlast'
(nbutlast '(1 2 3 4 5))        ;= (1 2 3 4)
;; pop macro -> provide a convenient way to examine the CAR of a list, and take it off the list, all at once.
(pop x)
;; equivalent to
(prog1 (car listname) (setq listname (cdr listname)))

;; Building Lists
;; cons function -> create a new cons cell, make arg1 the CAR, and arg2 the CDR
(cons 1 '(2))                  ;= (1 2)
(cons 1 '())                   ;= (1)
(cons 1 2)                     ;= (1 . 2)
;; list function -> create a list with OBJECTS as its elements
(list 1 2 3 4 5)               ;= (1 2 3 4 5)
(list 1 2 '(3 4 5) 'foo)       ;= (1 2 (3 4 5) foo)
(list)                         ;= nil
;; make-list function -> creates a list of LENGTH elements
(make-list 3 'pigs)            ;= (pigs pigs pigs)
(make-list 0 'pigs)            ;= nil
(setq l (make-list 3 '(a b)))  ;=((a b) (a b) (a b))
(eq (car l)(cadr l))           ;= t
;; append function -> return a list containing all the elements of SEQUENCES(must be list)
(setq ll (make-list 2 '(a b)))
(append '(1 2 3) ll)           ;= (1 2 3 (a b) (a b))
(append [a b] "cd" nil)        ;= (a b 99 100)
(apply 'append '((a b c) nil (x y z))) ;= (a b c x y z) with 'apply', we can append all the lists in a list of lists
(append)                       ;= nil
(append '(x y) 'z)             ;= (x y . z)
(append '(x y) [z])            ;= (x y . [z])
;; copy-tree function -> retuan a copy of the tree
(copy-tree '(1 2 (3 4) (2. 3)))    ;= (1 2 (3 4) (2 3))
(copy-tree '(1 2 (3 4) (2. 3)) t)  ;= (1 2 (3 4) (2 3)) so...what is different?
;; number-sequence function -> return a list of numbers [START.....END]
(number-sequence 4 9)          ;= (4 5 6 7 8 9)
(number-sequence 9 4 -1)       ;= (9 8 7 6 5 4)
(number-sequence 8)            ;= (8)
(number-sequence 5 8 -1)       ;= nil
(number-sequence 1.5 6 2)      ;= (1.5 3.5 5.5)

;; Modifying List Variables
;; push macro -> create a new list whose CAR is ELEMENT and whose CDR is the list
(setq l '(a b))                ;= (a b)
(push 'c l)                    ;= (c a b)
(pop l)                        ;= c
;; add-to-list function -> set the variable SYMBOL by consing ELEMENT onto the old value(if ElEMENT is not already a member of that value)
(setq foo '(a b))              ;= (a b)
(add-to-list 'foo 'c)          ;= (c a b)
(add-to-list 'foo 'b)          ;= (c a b)
;; add-to-ordered-listp function -> set the variable SYMBOL
(setq foo '())                 ;= nil
(add-to-ordered-list 'foo 'a 1);= (a)
(add-to-ordered-list 'foo 'c 3);= (a c)
(add-to-ordered-list 'foo 'b 2);= (a b c)
(add-to-ordered-list 'foo 'b 4);= (a c b)
(add-to-ordered-list 'foo 'd)  ;= (a c b d)
(add-to-ordered-list 'foo 'e)  ;= (a c b e d)

;; Modifying Lists
;; setcar function -> sotre OBEJCT as the new CAR of CONS replacing its previous CAR
(setq x '(1 2))                ;= (1 2)
(setcar x 4)                   ;= 4
x                              ;= (4 2)
;; when a cons cell is part of the shared structure of serveral lists.
;; storing a new CAR into the cons changes one element of each of these lists:
(setq x1 '(a b c))             ;= (a b c)
(setq x2 (cons 'z (cdr x1)))   ;= (z b c)
(setcar (cdr x1) 'foo)
x1                             ;= (a foo c)
x2                             ;= (z foo c)
;; Replace the CAR of a link that is not shared
(setcar x1 'baz)         ;= baz
x1                       ;= (baz foo c)
x2                       ;= (z foo c)

;; setcdr function -> stores OBJECT as the new CDR of CONS, replacing its previous CDR.
(setq x '(1 2 3))              ;= (1 2 3)
(setcdr x '(4))                ;= (4)
x                              ;= (1 4)
;; You can delete elemnts froom the middle of a list by altering the CDRs of the cons cells in the list.
(setq x1 '(a b c))             ;= (a b c)
(setcdr x1 (cdr (cdr x1)))     ;= (c)
x1                             ;= (a c)
;; It is equally easy to inert a new elment by changing CDRs
(setq x1 '(a b c))             ;= (a b c)
(setcdr x1 (cons 'd (cdr x1))) ;=(d b c)
x1                             ;=(a d b c)

;; Functions that Rearrange Lists
;; nconc function -> return a list containing all the elements of LISTS
(setq x '(1 2 3))              ;= (1 2 3)
(nconc x '(4 5))               ;= (1 2 3 4 5)
x                              ;= (1 2 3 4 5)
;; The last argument of 'nconc' is not itself modified:
(setq x1 '(1 2 3))             ;= (1 2 3)
(nconc x1 'z)                  ;= (1 2 3 . z)
x1                             ;= (1 2 3 . z)















;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------
;;-------------------- Numbers --------------------

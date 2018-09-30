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

;;-------------------- Control Structures --------------------
;; Sequencing
;; Special Form: progn forms...
(progn (print "The first form")
       (print "The second form")
       (print "The third form"))
;; "The first form"
;; "The second form"
;; "The third form"
;; -> "The third form"
;; Special Form: prog1 form1 forms...
(prog1 (print "The first form")
  (print "The second form")
  (print "The third form"))
;; "The first form"
;; "The second form"
;; "The third form"
;; -> "The first form"
;; Special Form: prog2 form1 form2 forms...
(prog2 (print "The first form")
    (print "The second form")
  (print "The third form"))
;; "The first form"
;; "The second form"
;; "The third form"
;; -> "The second form"

;; Conditionals
(if nil
    (print 'true)
  'very-false)
;; => very-false



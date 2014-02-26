(##import energy: testing)

(include "src/shared-structure.scm")

(test-begin "SRFI-38: External representation for data with shared structure")

(test-equal "Simple circular"
 (let ((a (cons 'val1 'val2))
       (p (open-string)))
   (set-cdr! a a)
   (write-with-shared-structure a p)
   (get-output-string p))
 "#1=(val1 . #1#)")

(test-end)

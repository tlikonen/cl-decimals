(defpackage :decimals-test
  (:use :cl)
  (:export :run-tests))

(in-package :decimals-test)


(defun report (name form)
  (format t "~A: ~S~%" (if form "pass" "FAIL") name)
  (if form t nil))


(defmacro check (&body forms)
  (let ((totalv (gensym)))
    `(let ((,totalv t))
       ,@(loop for form in forms
               collect `(unless (report ',form ,form)
                          (setf ,totalv nil)))
       ,totalv)))


(defun parse-decimal ()
  (loop for (in out . options)
        in '(("1234567890" 1234567890)
             (" 12.34 " 617/50)
             ("-12.34  " -617/50)
             ("  +12.34" 617/50)
             ("  .34 " 17/50)
             ("  +.34" 17/50)
             ("  -.34" -17/50)
             ("  34." 34)
             (" +34." 34)
             ("34." 34)
             ("0" 0)
             ("+0" 0 :positive-sign #\+)
             ("0.2" 1/5)
             (".2" 1/5)
             ("+3." 3)
             ("-7 " -7)
             ("00100.00100" 100001/1000)
             ("−12,345" -2469/200 :decimal-separator #\, :negative-sign #\−)
             (" 12,34" 617/50 :decimal-separator #\,)
             ("5d2" 26/5 :decimal-separator #\d))
        always (= (apply #'decimals:parse-decimal-number in options) out)))


(defun parse-decimal-illegal ()
  (loop for (input . options) in '(("abc")
                                   ("")
                                   (".")
                                   ("12,34")
                                   ("12.34" :decimal-separator #\,)
                                   ("--23")
                                   ("-1" :negative-sign #\–)
                                   ("–23"))

        always (typep (handler-case
                          (apply #'decimals:parse-decimal-number input options)
                        (error (c) c))
                      'decimals:decimal-parse-error)))


(defun rounding ()
  (loop for (number divisor output1 output2)
        in '((1/2 1 1 -1/2)
             (3/2 1 2 -1/2)
             (-3/2 1 -2 1/2)
             (5/2 1 3 -1/2)
             (15 10 2 -5)
             (-25 10 -3 5)
             (0 1 0 0))
        always (equal (multiple-value-list
                       (decimals:round-half-away-from-zero number divisor))
                      (list output1 output2))))



(defun format-decimal-magnitude ()
  (equal '("666.667" "666.67" "666.7" "667" "670" "700" "1000")
         (loop for m from -3 upto 3
               collect (decimals:format-decimal-number
                        2000/3 :round-magnitude m))))


(defun format-decimal-pp ()
  (equal '("        0,000 000 1"
           "        0,000 001  "
           "        0,000 01   "
           "        0,000 1    "
           "        0,001      "
           "        0,01       "
           "        0,1        "
           "        1          "
           "       10          "
           "      100          "
           "    1 000          "
           "   10 000          "
           "  100 000          "
           "1 000 000          ")

         (loop for e from -7 upto 6
               collect (decimals:format-decimal-number
                        (expt 10 e) :round-magnitude -7
                        :decimal-separator ","
                        :integer-minimum-width 9
                        :integer-group-separator " "
                        :fractional-minimum-width 10
                        :fractional-group-separator " "))))


(defun format-decimal-pp-trailing-zeros ()
  (equal '("        +0,000 001"
           "        +0,000 010"
           "        +0,000 100"
           "        +0,001 000"
           "        +0,010 000"
           "        +0,100 000"
           "        +1,000 000"
           "       +10,000 000"
           "      +100,000 000"
           "    +1 000,000 000"
           "   +10 000,000 000"
           "  +100 000,000 000"
           "+1 000 000,000 000")

         (loop for e from -6 upto 6
               collect (decimals:format-decimal-number
                        (expt 10 e) :round-magnitude -6
                        :decimal-separator ","
                        :show-trailing-zeros t
                        :integer-minimum-width 10
                        :integer-group-separator " "
                        :fractional-minimum-width 8
                        :fractional-group-separator " "
                        :positive-sign "+"))))


(defun format-decimal-signs ()
  (equal '("plus1" "zero0" "minus1")
         (loop for number in '(1 0 -1)
               collect (decimals:format-decimal-number
                        number
                        :positive-sign "plus"
                        :negative-sign "minus"
                        :zero-sign "zero"))))


(defun format-decimal-separators ()
  (equal '("9<9<9<0<0<0<0<0<0<0*0>0>0>0>0>0>0>0>0>0"
           "99<90<00<00<00*00>00>00>00>00"
           "9<990<000<000*000>000>000>0"
           "99<9000<0000*0000>0000>00"
           "99900<00000*00000>00000")
         (loop for digits from 1 upto 5
               collect (decimals:format-decimal-number
                        9990000000
                        :round-magnitude -10
                        :decimal-separator "*"
                        :integer-group-digits digits
                        :integer-group-separator "<"
                        :fractional-group-digits digits
                        :fractional-group-separator ">"
                        :show-trailing-zeros t))))


(decimals:define-decimal-formatter test-formatter
  (:round-magnitude -6)
  (:decimal-separator ",")
  (:integer-minimum-width 9)
  (:integer-group-separator " ")
  (:fractional-minimum-width 8)
  (:fractional-group-separator " "))


(defun custom-formatter ()
  (and (string= "        0,000 1  "
                (format nil "~/decimals-test::test-formatter/"
                        1/10000))
       (string= "  1,67 "
                (format nil "~-2,3,4/decimals-test::test-formatter/"
                        10/6))))


(defun run-tests ()
  (check
    (rounding)
    (parse-decimal)
    (parse-decimal-illegal)
    (format-decimal-magnitude)
    (format-decimal-pp)
    (format-decimal-pp-trailing-zeros)
    (format-decimal-signs)
    (format-decimal-separators)
    (custom-formatter)))

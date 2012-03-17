(defpackage #:decimals-test
  (:use #:cl)
  (:export #:run-tests))

(in-package #:decimals-test)


(defun report (name form)
  (format t "~A: ~S~%" (if form "pass" "FAIL") name)
  (if form t nil))


(defmacro check (&body forms)
  (let ((totalv (gensym)))
    `(let ((,totalv t))
       ,@(loop :for form :in forms
               :collect `(unless (report ',form ,form)
                           (setf ,totalv nil)))
       ,totalv)))


(defun parse-decimal ()
  (loop :for (in out . options)
        :in '(("1234567890" 1234567890)
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
              ("5d2" 26/5 :decimal-separator #\d)
              ("502" 26/5 :decimal-separator #\0)
              ("502" 26/5 :decimal-separator #\0 :negative-sign #\2)
              ("2502" 26/5 :decimal-separator #\0 :positive-sign #\2)
              ("2502" -26/5 :decimal-separator #\0 :negative-sign #\2))
        :always (ignore-errors
                  (= (apply #'decimals:parse-decimal-number in options) out))))


(defun parse-decimal-illegal ()
  (loop :for (input . options)
        :in '(("abc")
              ("")
              (".")
              ("12.34.")
              ("12,34")
              ("12.34" :decimal-separator #\,)
              ("--23")
              ("-1" :negative-sign #\–)
              ("–23"))

        :always (typep (handler-case
                           (apply #'decimals:parse-decimal-number input options)
                         (error (c) c))
                       'decimals:decimal-parse-error)))


(defun rounding-function ()
  (loop :for (number divisor quotient remainder)
        :in '((1/2 1 1 -1/2)
              (3/2 1 2 -1/2)
              (-3/2 1 -2 1/2)
              (5/2 1 3 -1/2)
              (15 10 2 -5)
              (-25 10 -3 5)
              (0 1 0 0))
        :always (equal (multiple-value-list
                        (decimals:round-half-away-from-zero number divisor))
                       (list quotient remainder))))


(defun format-decimal-round-magnitude ()
  (equal '(("666.6667" ("" "666" "." "6667"))
           ("666.667" ("" "666" "." "667"))
           ("666.67" ("" "666" "." "67"))
           ("666.7" ("" "666" "." "7"))
           ("667" ("" "667" "." ""))
           ("670" ("" "670" "." ""))
           ("700" ("" "700" "." ""))
           ("1000" ("" "1000" "." ""))
           ("0" ("" "0" "." "")))
         (loop :for m :from -4 :upto 4
               :collect (multiple-value-list
                         (decimals:format-decimal-number
                          2000/3 :round-magnitude m)))))


(defun format-decimal-align ()
  (equal '(("      0.000001" ("" "0" "." "000001"))
           ("      0.00001 " ("" "0" "." "00001"))
           ("      0.0001  " ("" "0" "." "0001"))
           ("      0.001   " ("" "0" "." "001"))
           ("      0.01    " ("" "0" "." "01"))
           ("      0.1     " ("" "0" "." "1"))
           ("      1       " ("" "1" "." ""))
           ("     10       " ("" "10" "." ""))
           ("    100       " ("" "100" "." ""))
           ("   1000       " ("" "1000" "." ""))
           ("  10000       " ("" "10000" "." ""))
           (" 100000       " ("" "100000" "." ""))
           ("1000000       " ("" "1000000" "." "")))

           (loop :for e :from -6 :upto 6
                 :collect (multiple-value-list
                           (decimals:format-decimal-number
                            (expt 10 e) :round-magnitude -6
                            :integer-minimum-width 7
                            :fractional-minimum-width 7)))))


(defun format-decimal-trailing-zeros ()
  (equal '(("0,001" ("" "0" "," "001"))
           ("0,010" ("" "0" "," "010"))
           ("0,100" ("" "0" "," "100"))
           ("1,000" ("" "1" "," "000")))
         (loop :for e :from -3 :upto 0
               :collect (multiple-value-list
                         (decimals:format-decimal-number
                          (expt 10 e) :round-magnitude -3
                          :decimal-separator ","
                          :show-trailing-zeros t)))))


(defun format-decimal-padding ()
  (equal '(("<<<0.001" ("" "0" "." "001"))
           ("<<<0.01>" ("" "0" "." "01"))
           ("<<<0.1>>" ("" "0" "." "1"))
           ("<<<1>>>>" ("" "1" "." ""))
           ("<<10>>>>" ("" "10" "." ""))
           ("<100>>>>" ("" "100" "." ""))
           ("1000>>>>" ("" "1000" "." "")))

         (loop :for e :from -3 :upto 3
               :collect (multiple-value-list
                         (decimals:format-decimal-number
                          (expt 10 e)
                          :round-magnitude -3
                          :integer-minimum-width 4
                          :integer-pad-char #\<
                          :fractional-minimum-width 4
                          :fractional-pad-char #\>)))))


(defun format-decimal-padding-signs ()
  (equal '(("...plus1" ("plus" "1" "." ""))
           ("...zero0" ("zero" "0" "." ""))
           ("..minus1" ("minus" "1" "." "")))
         (loop :for number :in '(1 0 -1)
               :collect (multiple-value-list
                         (decimals:format-decimal-number
                          number
                          :integer-minimum-width 8
                          :integer-pad-char #\.
                          :positive-sign "plus"
                          :negative-sign "minus"
                          :zero-sign "zero")))))


(defun format-decimal-digit-groups ()
  (equal '(("1<2<3<4<5<6<7<8<9<0*0>0>0>0>0>0>0>0>0>0"
            ("" "1<2<3<4<5<6<7<8<9<0" "*" "0>0>0>0>0>0>0>0>0>0"))
           ("12<34<56<78<90*00>00>00>00>00"
            ("" "12<34<56<78<90" "*" "00>00>00>00>00"))
           ("1<234<567<890*000>000>000>0"
            ("" "1<234<567<890" "*" "000>000>000>0"))
           ("12<3456<7890*0000>0000>00"
            ("" "12<3456<7890" "*" "0000>0000>00"))
           ("12345<67890*00000>00000"
            ("" "12345<67890" "*" "00000>00000")))
         (loop :for digits :from 1 :upto 5
               :collect (multiple-value-list
                         (decimals:format-decimal-number
                          1234567890
                          :round-magnitude -10
                          :decimal-separator "*"
                          :integer-group-digits digits
                          :integer-group-separator "<"
                          :fractional-group-digits digits
                          :fractional-group-separator ">"
                          :show-trailing-zeros t)))))


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
    (rounding-function)
    (parse-decimal)
    (parse-decimal-illegal)
    (format-decimal-round-magnitude)
    (format-decimal-align)
    (format-decimal-trailing-zeros)
    (format-decimal-padding)
    (format-decimal-padding-signs)
    (format-decimal-digit-groups)
    (custom-formatter)))

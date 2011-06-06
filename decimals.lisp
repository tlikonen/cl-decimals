(defpackage :decimals
  (:use :cl)
  (:export :round-half-away-from-zero
           :format-decimal-number
           :parse-decimal-number
           :decimal-parse-error
           :define-decimal-formatter))

(in-package :decimals)


(defun round-half-away-from-zero (number &optional (divisor 1))

  "Divide NUMBER by DIVISOR and round the result to the nearest integer.
If the result is half-way between two integers round away from zero.

This is similar to CL:ROUND function except that CL:ROUND rounds to an
even integer when number is exactly between two integers. Examples:

  (round-half-away-from-zero 3/2) => 2     (round 3/2) => 2
  (round-half-away-from-zero 5/2) => 3     (round 5/2) => 2
"

  (if (zerop number)
      (values 0 0)
      (let ((value (if (plusp number)
                       (floor (+ (/ number divisor) 1/2))
                       (ceiling (- (/ number divisor) 1/2)))))
        (values value (- number value)))))


(defun divide-into-groups (string &key (separator #\Space) (from-end nil)
                           (group-size 3))

  (setf separator (princ-to-string separator))

  (if (zerop (length separator))
      string
      (flet ((make-groups (string separator)
               (loop with length = (length string)
                     with result = (make-array length :element-type 'character
                                               :fill-pointer 0 :adjustable t)
                     for c across string
                     for i upfrom 1
                     do (vector-push-extend c result)
                     if (and (zerop (rem i group-size))
                             (< i length))
                     do (loop for c across separator
                              do (vector-push-extend c result))
                     finally (return result))))

        (if from-end
            (nreverse (make-groups (reverse string) (reverse separator)))
            (make-groups string separator)))))


(defun decimal-integer-fractional (number &key
                                   (round-magnitude 0)
                                   (rounder #'round-half-away-from-zero))

  (assert (integerp round-magnitude) (round-magnitude)
          "ROUND-MAGNITUDE argument must be an integer.")

  (let ((divisor (expt 10 round-magnitude)))
    (setf number (* divisor (funcall rounder number divisor))))

  (let ((sign (cond ((plusp number) :positive)
                    ((minusp number) :negative)
                    (t :zero))))

    (multiple-value-bind (integer fractional)
        (truncate (abs number))
      (let ((fractional-string
             (with-output-to-string (out)
               (loop with digit = fractional
                     with remainder = 0
                     until (integerp digit)
                     do
                     (multiple-value-setq (digit remainder)
                       (truncate (* digit 10)))
                     (princ digit out)
                     (setf digit remainder)))))
        (list sign (princ-to-string integer) fractional-string)))))


(defun string-align (string width &key (side :left) (char #\Space))
  (if (>= (length string) width)
      string
      (let ((result (make-string width :initial-element char)))
        (ecase side
          (:left (replace result string))
          (:right (replace result string
                           :start1 (- width (length string))))))))


(defun format-decimal-number (number &key
                              (round-magnitude 0)
                              (rounder #'round-half-away-from-zero)
                              (decimal-separator #\.)
                              (integer-group-separator nil)
                              (integer-group-digits 3)
                              (integer-minimum-width 0)
                              (integer-pad-char #\Space)
                              (fractional-group-separator nil)
                              (fractional-group-digits 3)
                              (fractional-minimum-width 0)
                              (fractional-pad-char #\Space)
                              (show-trailing-zeros nil)
                              (positive-sign nil)
                              (negative-sign #\-)
                              (zero-sign nil))

  "Apply specified decimal number formatting rules to NUMBER and return
a formatted string. The second return value is a list of four strings:
sign, integer part, decimal separator and fractional part. NUMBER must
be of type real. Formatting rules are specified with keyword arguments,
as described below. See also the examples at the end.

ROUND-MAGNITUDE                         0

    This is the order of magnitude used for rounding. The value must be
    an integer and it is interpreted as a power of 10.

SHOW-TRAILING-ZEROS                     nil

    If the value is non-nil print all trailing zeros in fractional part.
    Example:

        (format-decimal-number 1/5 :round-magnitude -3
                               :show-trailing-zeros nil)
        => \"0.2\"

        (format-decimal-number 1/5 :round-magnitude -3
                               :show-trailing-zeros t)
        => \"0.200\"

ROUNDER                                 #'round-half-away-from-zero

    The value must be a function and it is used to round number to
    specified round magnitude. The function must work like CL:TRUNCATE,
    CL:FLOOR, CL:CEILING and CL:ROUND, that is, take two arguments,
    number and divisor, and return the quotient as the first value.

    This package introduces another rounding function,
    ROUND-HALF-AWAY-FROM-ZERO, which is used by default. See its
    documentation for more information.

DECIMAL-SEPARATOR                       #\\.

    If the value is non-nil the PRINC output of the value will be added
    between integer and fractional parts. Probably the most useful types
    are character and string.

INTEGER-GROUP-SEPARATOR                 nil
FRACTIONAL-GROUP-SEPARATOR              nil

    If the value is non-nil the digits in integer or fractional parts
    are put in groups. The PRINC output of the value will be added
    between digit groups.

INTEGER-GROUP-DIGITS                    3
FRACTIONAL-GROUP-DIGITS                 3

    The value is an integer defining the size of digit groups.

INTEGER-MINIMUM-WIDTH                   0
FRACTIONAL-MINIMUM-WIDTH                0

    Format integer or fractional part using minimum of this amount of
    characters, possibly using some padding characters (see below).
    DECIMAL-SEPARATOR is included when calculating the width of
    FRACTIONAL-MINIMUM-WIDTH.

INTEGER-PAD-CHAR                        #\\Space
FRACTIONAL-PAD-CHAR                     #\\Space

    The value is the padding character which is used to fill
    INTEGER-MINIMUM-WIDTH or FRACTIONAL-MINIMUM-WIDTH.

POSITIVE-SIGN                           nil
NEGATIVE-SIGN                           #\\-
ZERO-SIGN                               nil

    If non-nil these are used as the leading sign for positive, negative
    and zero numbers. The PRINC output of the value is used.


EXAMPLES
========


DECIMALS> (format-decimal-number -100/6 :round-magnitude -3)
\"-16.667\"
\(\"-\" \"16\" \".\" \"667\")


DECIMALS> (loop for e from -5 upto 5
                do (print (format-decimal-number
                           (expt 10 e) :round-magnitude -5
                           :decimal-separator \",\"
                           :integer-minimum-width 7
                           :integer-group-separator \" \"
                           :fractional-minimum-width 7
                           :fractional-group-separator \" \")))

\"      0,000 01\" 
\"      0,000 1 \" 
\"      0,001   \" 
\"      0,01    \" 
\"      0,1     \" 
\"      1       \" 
\"     10       \" 
\"    100       \" 
\"  1 000       \" 
\" 10 000       \" 
\"100 000       \" 
NIL


DECIMALS> (loop for m from -3 upto 3
                do (print (format-decimal-number
                           2000/3 :round-magnitude m
                           :integer-minimum-width 4
                           :fractional-minimum-width 4)))

\" 666.667\" 
\" 666.67 \" 
\" 666.7  \" 
\" 667    \" 
\" 670    \" 
\" 700    \" 
\"1000    \" 
NIL
"

  (destructuring-bind (sign integer fractional)
      (decimal-integer-fractional number :round-magnitude round-magnitude
                                  :rounder rounder)

    (setf decimal-separator (princ-to-string decimal-separator)
          sign (princ-to-string
                (case sign
                  (:positive (or positive-sign ""))
                  (:negative (or negative-sign ""))
                  (:zero (or zero-sign ""))))
          integer (divide-into-groups
                   integer
                   :separator (or integer-group-separator "")
                   :group-size integer-group-digits
                   :from-end t)
          fractional (divide-into-groups
                      (let ((diff (- (- (length fractional))
                                     round-magnitude)))
                        (if (and show-trailing-zeros
                                 (plusp diff))
                            (concatenate 'string fractional
                                         (make-string
                                          diff :initial-element #\0))
                            fractional))
                      :separator (or fractional-group-separator "")
                      :group-size fractional-group-digits
                      :from-end nil))

    (values
     (concatenate
      'string
      (string-align (concatenate 'string sign integer)
                    integer-minimum-width
                    :side :right :char integer-pad-char)
      (string-align (if (plusp (length fractional))
                        (concatenate 'string decimal-separator fractional)
                        "")
                    fractional-minimum-width
                    :side :left :char fractional-pad-char))
     (list sign integer decimal-separator fractional))))


(defmacro define-decimal-formatter (name &body keyword-arguments)

  "Define a decimal number formatter function to use with the ~/
directive of CL:FORMAT. The valid format is this:

    (define-decimal-formatter NAME
      (:KEYWORD FORM)
      ...)

NAME is the symbol that names the function. KEYWORD must be a valid
keyword argument for the FORMAT-DECIMAL-NUMBER function (see its
documentation for more information). FORM is evaluated and the value is
used with the KEYWORD argument. Macro's side effect is that global
function NAME is defined and it is suitable for the ~/ directive of
CL:FORMAT function.

Example:

    (define-decimal-formatter my-formatter
      (:round-magnitude -6)
      (:decimal-separator \",\")
      (:integer-group-separator \" \")
      (:integer-minimum-width 4)
      (:fractional-group-separator \" \")
      (:fractional-minimum-width 10)
      (:show-trailing-zeros t))
    => MY-FORMATTER

    (format nil \"~/my-formatter/\" 10/6)
    => \"   1,666 667  \"

    (format nil \"~/my-formatter/\" 100/8)
    => \"  12,500 000  \"

The ~/ directive function call can optionally take up to three arguments
to override the defaults:

    ~round-magnitude,integer-minimum-width,fractional-minimum-width/FUNCTION/

For example:

    (format nil \"~-2,3,4/my-formatter/\" 10/6)
    => \"  1,67 \"
"
  
  (let ((key-arg (gensym)))
    `(let ((,key-arg (list ,@(loop for (keyword value) in keyword-arguments
                                   do (assert (keywordp keyword) (keyword)
                                              "Keyword required.")
                                   collect keyword collect value))))
       
       (defun ,name (stream number &optional colon-p at-sign-p
                     round-magnitude integer-minimum-width
                     fractional-minimum-width)
         (declare (ignore colon-p at-sign-p))

         (let ((args (copy-list ,key-arg)))
           (when round-magnitude
             (setf (getf args :round-magnitude)
                   round-magnitude))
           (when integer-minimum-width
             (setf (getf args :integer-minimum-width)
                   integer-minimum-width))
           (when fractional-minimum-width
             (setf (getf args :fractional-minimum-width)
                   fractional-minimum-width))
           (princ (apply #'format-decimal-number number args) stream))))))


(defun number-string-to-integer (string)
  (handler-case (parse-integer string)
    (parse-error () nil)))


(defun number-string-to-fractional (string)
  (loop for i = 1/10 then (/ i 10)
        for c across string
        for digit = (digit-char-p c)
        if digit sum (* i digit)
        else return nil))


(define-condition decimal-parse-error (parse-error)
  nil
  (:report "Not a valid decimal number string.")
  (:documentation
   "Function PARSE-DECIMAL-NUMBER signals this condition when it
couldn't parse a decimal number from string."))


(defun parse-decimal-number (string &key
                             (decimal-separator #\.)
                             (positive-sign #\+)
                             (negative-sign #\-)
                             (start 0) (end nil))

  "Examine STRING (or its substring from START to END) for a decimal
number. Assume that the decimal number is exact and return it as a
rational number.

Rules for parsing: First all leading and trailing #\\Space characters
are stripped. The resulting string may start with a POSITIVE-SIGN or a
NEGATIVE-SIGN character. The latter causes this function to assume a
negative number. The following characters in the string must include one
or more digit characters and it may include one DECIMAL-SEPARATOR
character which separates integer and fractional parts. All other
characters are illegal. If these rules are not met a DECIMAL-PARSE-ERROR
condition is signaled.

Examples:

    (parse-decimal-number \"0.2\")  => 1/5
    (parse-decimal-number \".2\")   => 1/5
    (parse-decimal-number \"+3.\")  => 3
    (parse-decimal-number \" -7 \") => -7

    (parse-decimal-number \"−12,345\"
                          :decimal-separator #\\, :negative-sign #\\−)
    => -2469/200
"

  (setf string (string-trim " " (subseq string start end)))
  (if (not (plusp (length string)))
      (error 'decimal-parse-error)
      (let ((sign 1))
        (cond ((char= (aref string 0) negative-sign)
               (setf sign -1
                     string (subseq string 1)))
              ((char= (aref string 0) positive-sign)
               (setf string (subseq string 1))))

        (if (and (every (lambda (item)
                          (or (digit-char-p item)
                              (char= item decimal-separator)))
                        string)
                 (some #'digit-char-p string)
                 (<= 0 (count decimal-separator string) 1))

            (let ((pos (position decimal-separator string)))
              (* sign
                 (+ (or (number-string-to-integer (subseq string 0 pos))
                        0)
                    (if pos
                        (number-string-to-fractional (subseq string (1+ pos)))
                        0))))

            (error 'decimal-parse-error)))))

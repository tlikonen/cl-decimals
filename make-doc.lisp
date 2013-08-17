#-sbcl
(error "This is only for SBCL.")

(require :sb-posix)
(require :sb-introspect)

(flet ((probe-load (path &optional (default (user-homedir-pathname)))
         (let ((path (merge-pathnames path default)))
           (when (probe-file path) (load path))))
       (funcallstr (string &rest args)
         (apply (read-from-string string) args)))
  (or (probe-load #p"quicklisp/setup.lisp")
      (probe-load #p".quicklisp/setup.lisp")
      (let ((url "http://beta.quicklisp.org/quicklisp.lisp")
            (init (nth-value 1 (funcallstr "sb-posix:mkstemp"
                                           "/tmp/quicklisp-XXXXXX"))))
        (unwind-protect
             (progn
               (sb-ext:run-program "wget" (list "-O" init "--" url)
                                   :search t :output t)
               (when (probe-load init)
                 (funcallstr "quicklisp-quickstart:install")))
          (delete-file init)))))

(defun symbol-doc-type (symbol)
  (let (docs)
    (flet ((doc (symbol type key)
             (push (list symbol key (documentation symbol type)) docs)))
      (cond ((ignore-errors (macro-function symbol))
             (doc symbol 'function :macro))
            ((ignore-errors (symbol-function symbol))
             (doc symbol 'function :function)))
      (when (ignore-errors (symbol-value symbol))
        (doc symbol 'variable :variable))
      (cond ((subtypep symbol 'condition)
             (doc symbol 'type :condition))
            ((ignore-errors (find-class symbol))
             (doc symbol 'type :class))))
    docs))

(defparameter *head*
  "~
Decimals
========

**A decimal number parser and formatting package for Common Lisp**


Introduction
------------

This Common Lisp package offers functions for parsing and formatting
decimal numbers. Package's main interface are functions
`parse-decimal-number` and `format-decimal-number`. The former is for
parsing strings for decimal numbers and the latter for pretty-printing
them as strings. See _Interface (API)_ for the full documentation of the
public programming interface. Here are some examples.


### Parsing

    DECIMALS> (parse-decimal-number \"0.24\")
    6/25


    DECIMALS> (parse-decimal-number \"−12,345\"
                                    :decimal-separator #\\,
                                    :negative-sign #\\−)
    -2469/200


### Formatting

    DECIMALS> (format-decimal-number -100/6 :round-magnitude -3)
    \"-16.667\"
    (\"-\" \"16\" \".\" \"667\")


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


Author and license
------------------

Author:  Teemu Likonen <<tlikonen@iki.fi>>

License: Public domain

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


Interface (API)
---------------

")

(defun print-doc (package &key (stream *standard-output*) (prefix "### "))
  (format stream *head*)
  (loop :with *package* := (find-package package)
        :with *print-right-margin* := 72
        :with *print-case* := :downcase
        :with symbols := (sort (loop :for symbol
                                     :being :each :external-symbol :in package
                                     :collect symbol)
                               #'string-lessp :key #'symbol-name)

        :for (symbol type doc) :in (mapcan #'symbol-doc-type symbols)
        :if doc :do
        (format stream "~A" prefix)
        (case type
          (:function
           (format stream "Function: `~A`" symbol)
           (let ((ll (sb-introspect:function-lambda-list symbol)))
             (when ll
               (format stream "~%~%The lambda list:~%~%     ~S" ll))))
          (:macro
           (format stream "Macro: `~A`" symbol)
           (let ((ll (sb-introspect:function-lambda-list symbol)))
             (when ll
               (format stream "~%~%The lambda list:~%~%     ~S" ll))))
          (:variable (format stream "Variable: `~A`" symbol))
          (:condition (format stream "Condition: `~A`" symbol))
          (:class (format stream "Class: `~A`" symbol)))
        (format stream "~%~%~A~%~%~%" doc)))


(pushnew (make-pathname :directory (pathname-directory *load-pathname*))
         asdf:*central-registry*)

(with-output-to-string (*standard-output*)
  (ql:quickload "decimals"))

(handler-case (print-doc "DECIMALS")
  (error (c)
    (format *error-output* "~A~%" c)))

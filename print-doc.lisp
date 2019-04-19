;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Creative Commons CC0 (public domain dedication)
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

(defpackage #:print-doc
  (:use #:cl)
  (:export #:print-doc))

(in-package #:print-doc)

(require 'sb-introspect)

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

(defun print-doc (package &key (stream *standard-output*) (prefix "### "))
  (loop
    :with *package* := (find-package package)
    :with *print-right-margin* := 72
    :with *print-case* := :downcase
    :with symbols
      := (sort (mapcan #'symbol-doc-type
                       (loop :for symbol
                               :being :each :external-symbol :in package
                             :collect symbol))
               (lambda (l1 l2)
                 (let ((s1 (symbol-name (first l1)))
                       (s2 (symbol-name (first l2)))
                       (t1 (symbol-name (second l1)))
                       (t2 (symbol-name (second l2))))
                   (or (string-lessp t1 t2)
                       (and (string-equal t1 t2)
                            (string-lessp s1 s2))))))

    :for (symbol type doc) :in symbols

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

Decimals
========

**A decimal number parser and formatting package for Common Lisp**


Author and license
------------------

Author:  Teemu Likonen <<tlikonen@iki.fi>>

License: Public domain

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


Introduction
------------

This package introduces three public functions and one macro. See their
own documentation for complete description. Here's a short introduction.


### Function: `parse-decimal-number (string &key ...)`

Examine _string_ for a decimal number and return it as a rational
number.


*Examples*

    DECIMALS> (parse-decimal-number "12,34" :decimal-separator #\,)
    617/50


*Keyword arguments and their default values*

    :decimal-separator  #\.
    :positive-sign      #\+
    :negative-sign      #\-
    :start              0
    :end                nil


### Function: `format-decimal-number (number &key ...)`

Apply specified decimal number formatting rules to _number_ and return a
formatted string.


*Examples*

    DECIMALS> (format-decimal-number 20000/3 :round-magnitude -3
                                     :decimal-separator ","
                                     :integer-group-separator " "
                                     :integer-minimum-width 7)
    "  6 666,667"
    ("" "6 666" "," "667")


    DECIMALS> (loop for e from -5 upto 5
                    do (print (format-decimal-number
                               (expt 10 e) :round-magnitude -5
                               :decimal-separator ","
                               :integer-minimum-width 7
                               :integer-group-separator " "
                               :fractional-minimum-width 7
                               :fractional-group-separator " ")))

    "      0,000 01"
    "      0,000 1 "
    "      0,001   "
    "      0,01    "
    "      0,1     "
    "      1       "
    "     10       "
    "    100       "
    "  1 000       "
    " 10 000       "
    "100 000       "
    NIL


*Keyword arguments and their default values*

    :round-magnitude             0
    :rounder                     #'round-half-away-from-zero
    :decimal-separator           #\.
    :integer-group-separator     nil
    :integer-group-digits        3
    :integer-minimum-width       0
    :integer-pad-char            #\space
    :fractional-group-separator  nil
    :fractional-group-digits     3
    :fractional-minimum-width    0
    :fractional-pad-char         #\space
    :show-trailing-zeros         nil
    :positive-sign               nil
    :zero-sign                   nil
    :negative-sign               #\-


### Function: `round-half-away-from-zero (number &optional (divisor 1))`

A rounding function similar to `cl:round` except that when _number_ is
exactly half-way between two integers round always away from zero.
`format-decimal-number` uses this rounding function by default.


### Macro: `define-decimal-formatter (name &body keyword-arguments)`

Define a custom decimal number formatter function suitable for the
`~/` directive of `cl:format`.

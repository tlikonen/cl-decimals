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
them as strings. See section _The Programming Interface)_ for the full
documentation of the public programming interface. Here are some
examples.


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


License and Source Code
-----------------------

Author: Teemu Likonen <<tlikonen@iki.fi>>

PGP: [4E10 55DC 84E9 DFF6 13D7 8557 719D 69D3 2453 9450][PGP]

License: [Creative Commons CC0][CC0] (public domain dedication)

The source code repository: <https://github.com/tlikonen/cl-decimals>

[PGP]: http://www.iki.fi/tlikonen/pgp-key.asc
[CC0]: https://creativecommons.org/publicdomain/zero/1.0/legalcode


The Programming Interface
-------------------------

")

(load "print-doc.lisp")
(load "decimals.lisp")
(format t *head*)
(print-doc:print-doc "DECIMALS")

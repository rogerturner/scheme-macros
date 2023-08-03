#! /usr/local/bin/scheme --script
;; SPDX-FileCopyrightText:  © 2023 Roger Turner <r@rogerturner.com>
;; SPDX-License-Identifier: LGPL-3.0-or-later
;; see *Notices* below for License and Contact links

#| Examples from [Extending a Language — Writing Powerful Macros in Scheme]
   (https://github.com/mnieper/scheme-macros)
   Top-level program using the check-examples library  |#

(import
  (chezscheme)
  (check-examples))

#| §4.1 Incrementing a variable |#

(define-syntax incr!
  (syntax-rules ()
    ((incr! x)
     (set! x (+ x 1)))))

(example: (define y 10)
          (incr! y)
          y             => 11 )

;; Example of hygiene:

(example: (let ([set! 2])
            (incr! set!)
            set!)       => 3 )

;; With the syntax-case version of incr! (§6.1), the example: can be embedded

(define-syntax sc-incr!
  (lambda (stx)
    (example: (define y 10)
              (sc-incr! y)
              y             => 11 )
    (syntax-case stx ()
      [(_ x)
       (identifier? #'x)
       #'(set! x (+ x 1))])))


(check-examples)


#| *Notices*

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.

  License: <https://spdx.org/licenses/LGPL-3.0-or-later.html>
  Contact: <https://github.com/rogerturner/Contact/issues/new/choose>  |#

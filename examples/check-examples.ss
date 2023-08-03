;; SPDX-FileCopyrightText:  © 2023 Roger Turner <r@rogerturner.com>
;; SPDX-License-Identifier: LGPL-3.0-or-later
;; see *Notices* below for License and Contact links

#| **Lightweight** check-examples library for Chez Scheme. Usage:
(define (square n)
  (example: (square 5) => 25 )
  (* n n))
...
(check-examples)
|#

#!chezscheme

(library (check-examples)

(export example: check-examples output-of)

(import (chezscheme))

(meta define *examples* (list))

(define-syntax example:
  ;; cons test to *examples*, produce dummy define
  (lambda (x)
    (set! *examples*
      (cons
        (syntax-case x (=>)
          [(example: e)  #'e ]
          [(example: e ... e1 => r)
            #'(equal? (let () e ... e1) r) ])
        *examples*))
    #'(define _)))

(define-syntax check-examples
  ;; produce asserts of tests, count if all pass, clear *examples*
  (lambda (_)
    (let ([examples (reverse! *examples*)])
      (set! *examples* (list))
      (syntax-case _ ()
        [(_) #`(begin
              #,@(map (lambda (ex) #`(assert #,ex))
                   examples)
                 (let ([n #,(length examples)])
                   (for-each display `(,n " example" ,(if (= 1 n) "" "s") " checked\n")))) ]))))

(define-syntax output-of
  ;; for checking output, eg: (example: (output-of (foo ... (display 22))) => "22" )
  (syntax-rules ()
    [(_ e) (with-output-to-string (lambda () e)) ]))

)

#| Notices:

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

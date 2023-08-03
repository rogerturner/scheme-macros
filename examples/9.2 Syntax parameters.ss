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

#| §9.2 Syntax parameters |#

(example:
  (let-syntax ([x (identifier-syntax 'outer)])
    (define-syntax y (identifier-syntax x))
    (list
      (fluid-let-syntax ([x (identifier-syntax 'inner)])
        y)
      y))
  => '(inner outer) )

(example:
  (let-syntax ([x (identifier-syntax 'outer)])
    (define-syntax y (identifier-syntax x))
    (list
     (let-syntax ([x (identifier-syntax 'inner)])
       y)
     y))
  => '(outer outer) )

(define-syntax break
  (lambda (stx)
    (syntax-violation 'break "invalid use outside of loop form" stx)))

(define-syntax loop
  (syntax-rules ()
    [(loop e ...)
     (call/cc
      (lambda (k)
        (fluid-let-syntax
            ([break (syntax-rules () [(break) (k)])])
          (let f ()
            e ... (f)))))]))

(define-syntax incr!
  (lambda (stx)
    (syntax-case stx ()
      [(_ x)
       (identifier? #'x)
       #'(set! x (+ x 1))])))

(example:
  (output-of
    (let ([i 0])
      (loop
        (when (= i 5)
          (break))
        (display i)
        (incr! i))))
  => "01234" )


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

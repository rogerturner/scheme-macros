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

#| §6.2 A fluid let |#

(define-syntax fluid-let1
  (lambda (stx)
    (syntax-case stx ()
      [(_ [(x e)] b1 ... b2)
       (identifier? #'x)
       #'(let ([y e])
           (define swap!
             (lambda ()
               (let ([t x])
                 (set! x y)
                 (set! y t))))
           (dynamic-wind
             swap!
             (lambda ()
               b1 ... b2)
             swap!))])))

(example:
  (output-of
    (let ([x 1])
      (define show
        (lambda ()
          (display x)))
      (show)
      (fluid-let1 ([x 2])
        (show))
      (show)))
  => "121" )

(define-syntax fluid-let
  (lambda (stx)
    (syntax-case stx ()
      [(_ [(x e) ...] b1 ... b2)
       (for-all identifier? #'(x ...))
       (with-syntax
           ([(y ...) (generate-temporaries #'(x ...))])
         #'(let ([y e] ...)
             (define swap!
               (lambda ()
                 (let ([t x])
                   (set! x y)
                   (set! y t))
                 ...))
             (dynamic-wind
               swap!
               (lambda ()
                 b1 ... b2)
               swap!)))])))

(example:
  (output-of
    (let ([a 1] [b 2])
      (define show
        (lambda ()
          (display (list a b))))
      (show)
      (fluid-let ([a 3] [b 4])
        (show))
      (show)))
  => "(1 2)(3 4)(1 2)" )


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

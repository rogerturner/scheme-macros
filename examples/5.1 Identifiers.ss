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

#| §5.1 Identifiers |#

(example: (syntax->datum (syntax x)) => (quote x))

(example: (bound-identifier=? #'x #'x) => #t )
(example: (bound-identifier=? #'x #'y) => #f )

(example: (let ([x 1]
                [y 1])
            (free-identifier=? #'x #'y))
          => #f )

(example: (let ([x 1])
            (define outer-x #'x)
            (let ([x 2])
              (define inner-x #'x)
              (list (bound-identifier=? outer-x inner-x)
                    (free-identifier=? outer-x inner-x))))
          => '(#t #f) )

(example: (let ([x 1])
            (let-syntax
                ([outer-x (identifier-syntax #'x)])
              (define inner-x #'x)
              (list (bound-identifier=? outer-x inner-x)
                    (free-identifier=? outer-x inner-x))))
          => '(#f #t) )

(example: (let ([x 1])
            (define outer-x #'x)
            (let ([x 2])
              (define outer (datum->syntax outer-x 'x))
              (list (bound-identifier=? outer-x outer)
                    (free-identifier=? outer-x outer))))
          => '(#t #t) )

(example: (let-syntax
              ([as-y
                (syntax-rules ()
                  [(as-y x) (datum->syntax #'x 'y)])])
            (bound-identifier=? #'y (as-y x)))
          => #t )


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

#! /usr/local/bin/scheme --script
;; SPDX-FileCopyrightText:  © 2023 Roger Turner <r@rogerturner.com>
;; SPDX-License-Identifier: LGPL-3.0-or-later
;; see *Notices* below for License and Contact links

#| Exercises from [Extending a Language — Writing Powerful Macros in Scheme]
   (https://github.com/mnieper/scheme-macros)
   Top-level program using the check-examples library  |#

(import
  (chezscheme)
  (check-examples))


#| §11 Exercises

1. Write a macro push! such that (push! list-variable expression) prepends
   the value of expression to the list bound to the list-variable.  |#

(example: (let ([xs (list 2 3)])
            (push! xs 1)
            xs) => '(1 2 3) )

(define-syntax push!
  (syntax-rules ()
  [ (push! xs x)
    (set! xs (cons x xs))]))

    #|
2.  Write a macro when-all such that (when-all test-expression ... expression)
    evaluates expression only if all test-expressions evaluate to non-#f. The macro
    should short-circuit the evaluation of the test-expressions as soon as one
    evaluates to #f.  |#

(example: (when-all 0) => 0 )
(example: (when-all #f) => #f )
(example: (when-all 0 #f) => #f )
(example: (when-all #f 0) => (void) )
(example: (when-all 0 1 #f) => #f )
(example: (when-all 0 #f 1) => (void) )
(example: (when-all 0 1 2 #f) => #f )
(example: (when-all 0 1 #f 2) => (void) )
(example: (when-all #f (error) 0) => (void) )

(define-syntax when-all
  (syntax-rules ()
    [(_ expression)
      expression ]
    [(_ t-e expression)
      (when t-e expression) ]
    [(_ t-e0 t-e1 ... expression)
      (when t-e0 (when-all t-e1 ... expression)) ]))

    #|
3.  Write a macro alist such that (alist key1 value1 key2 value2 ... ) expands
    into a literal expression of the form '((key1 value1) (key2 value2) ...).  |#

;; as function (compare with syntax version below):
(define alistf
  (lambda kvs
    ;; produce list of 2 element lists from arguments
    (example: (alistf 'a 1 'b 2) => '((a 1) (b 2)) )
    (let next-kv ([kvs kvs] [result (list)])
      (if (null? kvs)
        (reverse result)
        (next-kv (cddr kvs) (cons (list (car kvs) (cadr kvs)) result))))))

(define-syntax alist
  (lambda (stx)
    (example: (alist) => '() )
    (example: (alist a 1) => '((a 1)) )
    (example: (alist a 1 b 2) => '((a 1) (b 2)) )
    (syntax-case stx ()
    [ (_ kvs ...)
      (let next-kv ([kvs #'(kvs ...)] [result (list)])
        (if (null? kvs)
          (list #'quote (reverse result))
          (next-kv (cddr kvs) (cons (list (car kvs) (cadr kvs)) result)))) ])))

    #|
4.  Let x be a pattern variable that represents a list of syntax objects
    and y a pattern variable that represents a list of lists of syntax objects.
    Find out the result of #'(((x y) ...) ...).  |#

(example: (syntax-case '(_ (x1 x2) ((y11 y12) (y21 y22) (y31 y32))) ()
            [ (_ (x ...) ((y ...) ...))
              #'(((x y) ...) ...) ])
          => '(((x1 y11) (x2 y12))
               ((x1 y21) (x2 y22))
               ((x1 y31) (x2 y32))) )

    #|
5.  Write a macro timestamp such that timestamp expands into a number literal
    counting the number of uses of timestamp.  |#

(define-syntax timestamp
  (let ()
    (define ts-uses 0)
    (lambda (stx)
      (example: timestamp => 1)
      (example: (for-each (lambda _ timestamp) (iota 9)) => 2)
      (example: timestamp => 3)
      (syntax-case stx ()
      [ _
        (begin ;fender
          (set! ts-uses (+ 1 ts-uses))
          #t)
        ts-uses ] ))))


    #|
6.  Rewrite fluid-let as a recursive macro that does not use generate-temporaries.  |#

    #|
7.  Write a procedure symbolic-identifier=? so that (symbolic-identifier=? id1 id2) returns
    #t if and only if the two identifiers id1 and id2 have the same symbolic name  |#

(define (symbolic-identifier=? id1 id2)
  (eq? (syntax->datum id1) (syntax->datum id2)))

(example: (let ([a 1]) (symbolic-identifier=? a |a|)) )
(example: (let ([㎞ 1]) (let ([km 2])
            (symbolic-identifier=? ㎞ km))) => #f )


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

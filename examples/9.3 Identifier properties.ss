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

#| §9.3 Identifier properties |#

(define add1 (lambda (x) (+ 1 x)))
(define key)
(define-property add1 key #'"value")

(example: (add1 2) => 3 )

(example:
  (let-syntax
      ([x
        (lambda (stx)
          (lambda (lookup)
            (syntax-case stx ()
              [(_ key)
               (or (lookup #'add1 #'key)
                   #'"no-value")])))])
    (define other-key)
    (list (x key) (x other-key)))
  => '("value" "no-value") )

#| The public API for our new loop facility consists of the |for| keyword
   and the |define-iterator| defining keyword. Moreover, we define two
   iterator forms, one for going through a list and the other one for
   going through a numeric range. The loop facility is extensible because
   the user can define more iterator forms using |define-iterator|.  |#

(example:
  (output-of
    (for ([x in-list '(a b c)]
          [i in-range 0 10])
      (display (list x i))
      (newline)))
  => "(a 0)\n(b 1)\n(c 2)\n" )

(example:
  (output-of
    (for ([x in-list '(a b c d e)]
          [i in-range 0 3])
      (display (list x i))
      (newline)))
  => "(a 0)\n(b 1)\n(c 2)\n" )

(example:
  (output-of
    (do ( [xs '(a b c) (cdr xs)]
          [i 0 (+ 1 i)])
        ((or (null? xs) (>= i 10)))
      (display (list (car xs) i))
      (newline)))
  => "(a 0)\n(b 1)\n(c 2)\n" )

(example:
  (output-of
    (let next-xi ([xs '(a b c)] [is (iota 10)])
      (cond
        [(or (null? xs) (null? is)) ]
        [else
          (display (list (car xs) (car is)))
          (newline)
          (next-xi (cdr xs) (cdr is)) ])))
  => "(a 0)\n(b 1)\n(c 2)\n" )


(define key)

(define-syntax define-iterator
  (lambda (stx)
    (syntax-case stx ()
      [(define-iterator name parser-expr)
       (identifier? #'name)
       #'(begin
           (define-syntax name
             (lambda (stx)
               (syntax-violation 'name "invalid use of for keyword" stx)))
           (define-property name key
             (let ([parser parser-expr])
               (unless (procedure? parser)
                 (assertion-violation 'define-iterator "invalid parser" parser))
               parser)))])))

(define-syntax for
  (lambda (stx)
    (lambda (lookup)
      (define parse-clause
        (lambda (cl)
          (syntax-case cl ()
            [(formals keyword . arg)
             (identifier? #'keyword)
             (let ([keyword #'keyword])
               (define parser (lookup keyword #'key))
               (unless (procedure? parser)
                 (syntax-violation 'for "invalid for iterator" stx keyword))
               (let-values ([(outer-var* var* loop-var*
                              outer-expr init-expr test-expr loop-expr step-expr)
                             (parser stx cl)])
                 (list outer-var* var* loop-var*
                       outer-expr init-expr test-expr loop-expr step-expr)))])))
      (syntax-case stx ()
        [(_ (clause ...) command ...)
         (with-syntax ([(((outer-var ...) (var ...) (loop-var ...)
                          outer-expr init-expr test-expr loop-expr step-expr) ...)
                        (map parse-clause #'(clause ...))])
           #'(let-values ([(outer-var ...) outer-expr] ...)
               (let-values ([(var ...) init-expr] ...)
                 (let f ([var var] ... ...)
                   (unless (or test-expr ...)
                     (let-values ([(loop-var ...) loop-expr] ...)
                       command ...
                       (let-values ([(var ...) step-expr] ...)
                         (f var ... ...))))))))]))))

(define-iterator in-list
  (lambda (stx cl)
    (syntax-case cl ()
      [(var _ list-expr)
       (identifier? #'var)
       (values #'()           ;outer-var*
               #'(tmp)        ;var*
               #'(var)        ;loop-var*
               #'(values)     ;outer-expr
               #'list-expr    ;init-expr
               #'(null? tmp)  ;test-expr
               #'(car tmp)    ;loop-expr
               #'(cdr tmp)    ;step-expr
               )])))

(define-iterator in-range
  (lambda (stx cl)
    (syntax-case cl ()
      [(var _ start-expr end-expr)
       (identifier? #'var)
       (values #'(end)         ;outer-var*
               #'(i)           ;var*
               #'(var)         ;loop-var*
               #'end-expr      ;outer-expr
               #'start-expr    ;init-expr
               #'(>= i end)    ;test-expr
               #'i             ;loop-expr
               #'(+ i 1)       ;step-expr
               )])))

(define-iterator in-vector
  (lambda (stx cl)
    (syntax-case cl ()
      [(elt _ vec-expr)
       (identifier? #'elt)
       (values #'(vec)         ;outer-var*
               #'(i)           ;var*
               #'(elt)         ;loop-var*
               #'vec-expr      ;outer-expr
               #'0             ;init-expr
               #'(>= i (vector-length vec))  ;test-expr
               #'(vector-ref vec i)          ;loop-expr
               #'(+ i 1)       ;step-expr
               )])))

(example:
  (output-of
    (for ([x in-vector '#(a b c)])
      (for ([i in-vector '#(0 1 2)])
        (display x) (display i))))
  => "a0a1a2b0b1b2c0c1c2" )


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

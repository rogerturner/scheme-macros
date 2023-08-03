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

#| §4.2 A tracing let
   "A form of a named let that prints information about each recursive call."

   The trace-let template is significantly more code than incr! (2 loc -> 32).
   A version could be derived in stages (trace-let0 -> trace-let5) as follows:
   given the pattern:
     (let name ([var expr] ...)
       body1 ... body2)
   the template has to bind name to a procedure with the vars as arguments
   and apply it to the exprs [TSPL4 §5.4]                                  |#

;; === trace-let0: capture trace output for checking ===

(define-syntax trace-let0
  (syntax-rules ()
    [(named-let name ([var expr] ...) body1 ... body2)
      ((letrec ([name (lambda (var ...)
                        body1 ... body2)])
          name)
        expr ...) ]))

(define-syntax value-of
  ;; produce value of e in (example: (value-of e) => r ), discarding output
  (syntax-rules ()
    [(value-of expr)
      (with-output-to-file "/dev/null" (lambda () expr) 'append) ]))

(alias trace-of output-of)  ;; (examples will produce trace on current-output)

(example: (value-of (trace-let0 f () #f)) => #f )
(example: (trace-of (trace-let0 f () #f)) => "" )

;; === trace-let1: add display of args on entry (added code flagged with #;> ) ===

(example: (value-of (trace-let1 f ([a 42]) a))        => 42 )
(example: (trace-of (trace-let1 f ([a 42]) a))        => "|(f 42)\n" )
(example: (trace-of (trace-let1 f ([a 42] [b 43]) a)) => "|(f 42 43)\n" )

(define-syntax trace-let1
  (syntax-rules ()
    [(trace-let1 name ([var expr] ...) body1 ... body2)
      ((letrec ([name (lambda (var ...)
#;>                     (for-each display `("|(" name ,@`(" " ,var) ... ")\n"))
                        body1 ... body2)])
          name)
        expr ...) ]))

;; === trace-let2: add display of result on return ===

(example: (value-of (trace-let2 f ([a 42]) a)) => 42 )
(example: (trace-of (trace-let2 f ([a 42]) a)) => "|(f 42)\n|42\n" )

(define-syntax trace-let2
  (syntax-rules ()
    [(trace-let2 name ([var expr] ...) body1 ... body2)
      ((letrec ([name (lambda (var ...)
                        (for-each display `("|(" name ,@`(" " ,var) ... ")\n"))
                        body1 ...
#;>                     (let ([result body2])
#;>                       (for-each display `("|" ,result "\n"))
#;>                       result))])
          name)
        expr ...) ]))

(define-syntax trace-f
  (syntax-rules (f a)
    [(_ trace-let n)
      (trace-let f ([a n])
        (if (zero? a)  1
            (* a (f (- a 1))))) ]))

(define-syntax with-trace-let2 (identifier-syntax trace-let2))

(example: (value-of (trace-f with-trace-let2 3)) => 6 )

(example: (trace-of (trace-f with-trace-let2 1))
          => "|(f 1)\n|(f 0)\n|1\n|1\n" )

;; === trace-let3: add indentation (note: indent by 2 vs Chez's 1) ===

(define-syntax trace-let3
  (syntax-rules ()
    [(trace-let3 name ([var expr] ...) body1 ... body2)
      ((letrec (
#;>       [depth  0 ]
#;>       [indent (lambda ()
#;>                 (do ( [i 0 (+ i 1)]
#;>                       [s "|" (string-append s " |")])
#;>                     ((>= i depth) s))) ]
          [name   (lambda (var ...)
                    (for-each display `(,(indent) "(" name ,@`(" " ,var) ... ")\n"))
#;>                 (set! depth (+ depth 1))
                    body1 ...
                    (let ([result body2])
#;>                   (set! depth (- depth 1))
                      (for-each display `(,(indent) ,result "\n"))
                      result)) ])
        name)
      expr ...) ]))

(define-syntax with-trace-let3 (identifier-syntax trace-let3))

(example: (value-of (trace-f with-trace-let3 3)) => 6 )

(example:
  (trace-of (trace-f with-trace-let3 3))
  => "|(f 3)\n\
      | |(f 2)\n\
      | | |(f 1)\n\
      | | | |(f 0)\n\
      | | | |1\n\
      | | |1\n\
      | |2\n\
      |6\n" )

;; === trace-let4: refactor to make depth a named let argument ===

(define-syntax trace-let4
  (syntax-rules ()
    [ (trace-let4 name ([var expr] ...) body1 ... body2)
      (let ([indent (lambda (depth)
                      (do ( [i 0 (+ i 1)]
                            [s "|" (string-append s " |")])
                          ((>= i depth) s))) ])
        (let name* ([depth 0] [var expr] ...)  ; named let (with depth)..
          (define (name var ...)               ;   (name v ...) in body..
            (name* (+ depth 1) var ...))       ;   ..will recurse with indent
          (for-each display                    ; arg trace before..
            `(,(indent depth) "(" name ,@`(" " ,var) ... ")\n"))
          body1 ...
          (let ([result body2])
            (for-each display                  ; ..and result trace after
              `(,(indent depth) ,result "\n"))
            result))) ]))

(define-syntax with-trace-let4 (identifier-syntax trace-let4))

(example: (value-of (trace-f with-trace-let4 3)) => 6 )

(example:
  (trace-of (trace-f with-trace-let4 3))
  => (trace-of (trace-f with-trace-let3 3)) )

;; === trace-let5: handle multiple value result ===

(define-syntax trace-let5
  (syntax-rules ()
  [ (trace-let5 name ([var expr] ...) body1 ... body2)
    (let ([indent (lambda (depth)
                    (do ( [i 0 (+ i 1)]
                          [s "|" (string-append s " |")])
                        ((>= i depth) s))) ])
      (let name* ([depth 0] [var expr] ...)
        (define (name var ...)
          (name* (+ depth 1) var ...))
        (for-each display
          `(,(indent depth) "(" name ,@`(" " ,var) ... ")\n"))
        (call-with-values
          (lambda () body1 ... body2)
          (lambda val*
            (for-each display  ;; (does not separate values!)
              `(,(indent depth) ,@val* "\n"))
            (apply values val*))))) ]))

(define (traced-split xs)
  (trace-let5 split ([xs xs])
    (if (or (null? xs) (null? (cdr xs)))
      (values xs '())
      (call-with-values
        (lambda () (split (cddr xs)))
        (lambda (os es)
          (values (cons (car xs) os)
                  (cons (cadr xs) es)))))))

(example:
  (value-of
    (let-values ([(os es) (traced-split '(a b c d))])
      (list os es)))
  => '((a c) (b d)) )

(example:
  (trace-of
    (let-values ([(os es) (traced-split '(a b c d))])
      #f))
  => "|(split (a b c d))\n\
      | |(split (c d))\n\
      | | |(split ())\n\
      | | |()()\n\
      | |(c)(d)\n\
      |(a c)(b d)\n" )


;; === incr! macro that accepts more than one variable to increment: ===

(define-syntax incr!
  (syntax-rules ()
    [(incr! x ...)
      (begin
        (set! x (+ x 1))
        ...) ]))

(example: (define x 10)
          (define y 20)
          (incr! x y)
          (list x y)  => '(11 21) )

(define-syntax incr*!
  (syntax-rules ()
    [(incr*!)
      (values) ]
    [(incr*! x . x*)
      (begin
        (set! x (+ x 1))
        (incr! . x*)) ]))

(example: (define x 100)
          (define y 200)
          (incr*! x y)
          (list x y)  => '(101 201) )


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

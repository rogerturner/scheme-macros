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

#| §6.1 Macro transformers |#

(example:
  (let ([x 41])
    (define-syntax always-42
      (lambda (stx)
        (syntax (+ 1 x))))
    (+ always-42
     (always-42 400))) => 84 )

;; The macro transformers below (f1 etc) produce list-structured SyntaxObjects.
;; For checking in |example:|, unwrap to datums:

(define ($ xs) ;; ListOfSyntaxObjects -> ListOfDatums
  (example: ($ (list (syntax a))) => '(a) )
  (map (lambda (x)
      (if (list? x)  ($ x)
          (syntax->datum x)))
    xs))

(define (f1 stx) ;; Syntax[_ x ...] -> Syntax['(<length xs> x ...)]
  ;; produce syntax object for args of combination stx with length prepended
  (example: ($ (f1 #'(foo a b c))) => '(quote (3 a b c)) )
  (syntax-case stx ()
    [(_ x ...)
     (list #'quote (append (list (length #'(x ...))) #'(x ...)))]))

(define (f2 stx)
  ;; .. use quasisyntax
  (example: ($ (f2 #'(foo a b c))) => '(quote (3 a b c)) )
  (syntax-case stx ()
    [(_ x ...)
     #`(quote (#,(length #'(x ...)) x ...))]))

(define (f3 stx)
  ;; .. use with-syntax
  (example: ($ (f3 #'(foo a b c))) => '(quote (3 a b c)) )
  (syntax-case stx ()
    [(_ x ...)
     (with-syntax ([n (length #'(x ...))])
       #'(quote (n x ...)))]))

(define-syntax quote/length1 f1)
(define-syntax quote/length2 f2)
(define-syntax quote/length3 f3)

(example: (quote/length1 a b c) => '(3 a b c) )
(example: (quote/length2 a b c) => '(3 a b c) )
(example: (quote/length3 a b c) => '(3 a b c) )

#| §6.1.5 Definition of syntax-rules |#

#;
(define-syntax syntax-rules
  (lambda (stx)
    (syntax-case stx ()
      [(_ (lit ...) [(k . p) t] ...)
       (for-all identifier? #'(lit ... k ...))
       #'(lambda (x)
           (syntax-case x (lit ...)
             [(_ . p) #'t] ...))])))

#| "It is instructive to go through the above definition of the syntax-rules
    keyword and see how the earlier definition using syntax-rules expands into
    the later definition using syntax-case" |#

(define-syntax incr!                  ;; "earlier definition using syntax-rules"
  (syntax-rules ()
    [(incr! x)
      (set! x (+ x 1)) ]))

(define-syntax syntax-rules
  (lambda (stx)                       ;; <- "right hand side of define-syntax expects a procedure"
    (syntax-case stx ()               ;; <- syntax-case :: SyntaxObject -> SyntaxObject
      [(_ (lit ...) [(k . p) t] ...)  ;; <- pattern: for (define-syntax incr! etc) match is:
                                      ;;    _           syntax-rules
                                      ;;    (lit ...)   ()
                                      ;;    k           #'incr!
                                      ;;    p           #'x
                                      ;;    t           #'(set! x (+ x 1))
        (for-all identifier?          ;; <- fender
                 (syntax (lit ... k ...)))
        (syntax                       ;; <- template: "procedure to which a syntax-rules expression
                                      ;;               evaluates outputs itself a syntax object"
                                      ;;    (syntax-rules () etc ) expands to:
          (lambda (stx)               ;;    (lambda (stx)
            (syntax-case stx (lit ...);;      (syntax-case stx ()
              [(_ . p)                ;;        [(_ x)
                (syntax t)            ;;          #'(set! x (+ x 1))]))
                ] ...)))              ;;    which is the syntax-case version of incr!
        ])))


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

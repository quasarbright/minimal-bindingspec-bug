#lang racket

(module+ test
  (require rackunit))
(provide (all-defined-out))



(require bindingspec
         (for-syntax syntax/parse))



(define-hosted-syntaxes
  (binding-class var)
  (extension-class pattern-macro
                   #:binding-space my-match)
  (nonterminal clause
               [p:pat e:expr]
               #:binding
               (nest-one p (host e)))
  (nesting-nonterminal pat (body)
               #:allow-extension pattern-macro
               v:var
               #:binding {(bind v) body}
               _
               #:binding {body}))

(define-syntax define-match-syntax
  (syntax-parser
    [(_ name:id rhs:expr)
     #:with spaced-name ((make-interned-syntax-introducer 'my-match) (attribute name) 'add)
     #'(define-syntax spaced-name rhs)]))

(define-syntax define-pattern-syntax
  (syntax-parser
    [(_ (macro-name:id args ...) body ...) #'(define-pattern-syntax macro-name (λ (args ...) body ...))]
    [(_ macro-name:id transformer) #'(define-match-syntax macro-name (pattern-macro transformer))]))

(define-host-interface/expression (my-match target:expr c:clause)
  #:binding (host target)
  (compile-clause #'target #'c))

(begin-for-syntax
  (define (compile-clause target c)
    (syntax-parse c
      [[p body] (compile-pattern target #'p #'body)]))
  (define (compile-pattern target p body)
    (syntax-parse p
      [p
       #:with body body
       #:with target target
       (syntax-parse #'p
         [(~datum _) (compile-host-expr #'body)]
         [var:id
          #:with var^ (compile-binder! #'var)
          #:with body^ (compile-host-expr #'body)
          #:with target^ (compile-host-expr #'target)
          #'(let ([var^ target^]) body^)])]))
    (define (compile-host-expr e)
      (resume-host-expansion e #:reference-compilers ([var compile-reference]))))

(module+ test
  (check-equal? (my-match 1 [_ 2]) 2)
  (check-equal? (my-match 1 [a 2]) 2)
  ; this test passes using raco from cmd line and in drracket
  ; this test passes in another file that requires this one
  ; this file runs when executing with racket from cmd line
  ; this test runs when this file is required in the racket repl at the cmd line or in emacs or in drracket
  ; this test throws an ambiguous binding error when ran with emacs racket-run or racket-test
  ; interestingly, the error points to the binding position, not the reference position, in the source
  #;(check-equal? (my-match 1 [  a a]) 1))

; /Users/mdelmonaco/Documents/GitHub/minimal-bindingspec-bug/main.rkt:72:31: a: identifier's binding is ambiguous
;   in: a
;   context...:
;    #(3747736 module) #(3747743 module main) #(3751972 module)
;    #(3751979 module test) #(3752123 local) #(3752124 intdef)
;    #(3752125 local) #(3752129 intdef) #(3752141 local) #(3752142 intdef)
;    #(3752146 local) #(3752147 intdef) #(3752168 intdef-outside)
;    #(3752169 intdef) #(3752173 macro) #(3752175 local) #(3752177 intdef)
;   matching binding...:
;    local
;    #(3747736 module) #(3747743 module main) #(3751972 module)
;    #(3751979 module test) #(3752123 local) #(3752124 intdef)
;    #(3752125 local) #(3752129 intdef) #(3752141 local) #(3752142 intdef)
;    #(3752146 local) #(3752147 intdef) #(3752168 intdef-outside)
;    #(3752169 intdef)
;   matching binding...:
;    local
;    #(3752173 macro) #(3752175 local)

(define-pattern-syntax m (syntax-parser [(_ p) #'p]))
(module+ test
  ; this test passes when using raco from cmd line and in drracket
  ; this test passes in another file that requires this one
  ; this file runs when using racket from cmd line
  ; when required in the repl in the cmd line, using m yields an error saying (m a) is not a pat
  ; when required in the repl and a macro is defined in the repl, using it [(m a) a] says that a is undefined
  ; when ran in emacs using racket-run or racket-test, we get a similar ambiguous binding error
  ; again, the error points to the binding position, not the reference position
  ; Interestingly, when this test is commented out and the file is ran in emacs with racket-run, using m in the repl works
  ; This test works in the drracket repl too
  #;(check-equal? (my-match 1 [(m a) a]) 1))

; /Users/mdelmonaco/Documents/GitHub/minimal-bindingspec-bug/main.rkt:104:32: a: identifier's binding is ambiguous
;   in: a
;   context...:
;    #(3869327 module) #(3869334 module main) #(3873772 module)
;    #(3873779 module test) #(3873923 local) #(3873924 intdef)
;    #(3873925 local) #(3873929 intdef) #(3873941 local) #(3873942 intdef)
;    #(3873946 local) #(3873947 intdef) #(3873970 intdef-outside)
;    #(3873971 intdef) #(3873974 macro) #(3873976 local) #(3873978 intdef)
;   matching binding...:
;    local
;    #(3869327 module) #(3869334 module main) #(3873772 module)
;    #(3873779 module test) #(3873923 local) #(3873924 intdef)
;    #(3873925 local) #(3873929 intdef) #(3873941 local) #(3873942 intdef)
;    #(3873946 local) #(3873947 intdef) #(3873970 intdef-outside)
;    #(3873971 intdef)
;   matching binding...:
;    local
;    #(3873974 macro) #(3873976 local)

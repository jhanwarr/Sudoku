#lang typed/racket

(: cs151-core-version : String)
(define cs151-core-version "A6.11")
(provide cs151-core-version)

(: cs151-core-date : String)
(define cs151-core-date "January 17, 2020")
(provide cs151-core-date)

(define-syntax cs151-core-define-struct
  (syntax-rules ()
    [(cs151-core-define-struct (name A ...) (fields ...))
     (define-struct (A ...) name (fields ...) #:transparent)]
    [(cs151-core-define-struct (name A ...) (fields ...) #:mutable)
     (define-struct (A ...) name (fields ...) #:transparent #:mutable)]
    [(cs151-core-define-struct name (fields ...) #:mutable)
     (define-struct name (fields ...) #:transparent #:mutable)]
    [(cs151-core-define-struct name (fields ...))
     (define-struct name (fields ...) #:transparent)]))
(provide (rename-out [cs151-core-define-struct define-struct]))
;; NOTE Make sure the CS tutors know about this change to define-struct

;; error reporting
;;
(: cs151-core-error : String -> Nothing)
(define (cs151-core-error msg) (error msg))
(provide (rename-out [cs151-core-error error]))

(: cs151-core-cons : (All (A) (-> A (Listof A) (Listof A))))
(define (cs151-core-cons hd tl) (cons hd tl))
(provide (rename-out [cs151-core-cons cons]))

(: cs151-core-first : (All (A) (Listof A) -> A))
(define (cs151-core-first xs) (first xs))
(provide (rename-out [cs151-core-first first]))

(: cs151-core-map : (All (A B) (-> (-> A B) (Listof A) (Listof B))))
(define (cs151-core-map f xs) (map f xs))
(provide (rename-out [cs151-core-map map]))

(: cs151-core-filter : (All (A) (-> (-> A Boolean) (Listof A) (Listof A))))
(define (cs151-core-filter f xs) (filter f xs))
(provide (rename-out [cs151-core-filter filter]))

(: cs151-core-foldl : (All (A B) (-> (-> A B B) B (Listof A) B)))
(define (cs151-core-foldl f acc xs) (foldl f acc xs))
(provide (rename-out [cs151-core-foldl foldl]))

(: cs151-core-foldr : (All (A B) (-> (-> A B B) B (Listof A) B)))
(define (cs151-core-foldr f acc xs) (foldr f acc xs))
(provide (rename-out [cs151-core-foldr foldr]))

(: cs151-core-partition :
   (All (A) (-> (-> A Boolean) (Listof A) (values (Listof A) (Listof A)))))
(define (cs151-core-partition f xs) (partition f xs))
(provide (rename-out [cs151-core-partition partition]))

(: cs151-core-andmap : (All (A) (-> (-> A Boolean) (Listof A) Boolean)))
(define (cs151-core-andmap f xs) (andmap f xs))
(provide (rename-out [cs151-core-andmap andmap]))

(: cs151-core-ormap : (All (A) (-> (-> A Boolean) (Listof A) Boolean)))
(define (cs151-core-ormap f xs) (ormap f xs))
(provide (rename-out [cs151-core-ormap ormap]))

(: cs151-core-vector-map : (All (A B) (A -> B) (Vectorof A) -> (Vectorof B)))
(define (cs151-core-vector-map f v) (vector-map f v))
(provide (rename-out [cs151-core-vector-map vector-map]))

(: cs151-core-vector-filter : (All (A) (A -> Boolean) (Vectorof A) -> (Vectorof A)))
(define (cs151-core-vector-filter f v) (vector-filter f v))
(provide (rename-out [cs151-core-vector-filter vector-filter]))

(: cs151-core-vector-count : (All (A) (A -> Boolean) (Vectorof A) -> Integer))
(define (cs151-core-vector-count pred v) (vector-count pred v))
(provide (rename-out [cs151-core-vector-count vector-count]))

(: cs151-core-sqrt : Real -> Real)
(define (cs151-core-sqrt x)
  (if (not (negative? x))
      (sqrt x)
      (error
       (string-append "sqrt expects a nonnegative real; given "
                      (number->string x)))))
(provide (rename-out [cs151-core-sqrt sqrt]))

(provide (rename-out [sqrt full-sqrt]))

;; forbidden builtin functions
;;

(: forbidden-function : String -> String)
(define (forbidden-function f)
  (string-append "cs151-core: " f
                 ": You may not use the built-in function \"" f
                 "\" in this course; it might render certain programming exercises trivial. "
		 "You must write your own such function."))

(: cs151-core-argmax : (All (A) (A -> Real) (Listof A) -> A))
(define (cs151-core-argmax f xs)
  (error (forbidden-function "argmax")))
(provide (rename-out [cs151-core-argmax argmax]))

(: cs151-core-argmin : (All (A) (A -> Real) (Listof A) -> A))
(define (cs151-core-argmin f xs)
  (error (forbidden-function "argmin")))
(provide (rename-out [cs151-core-argmin argmin]))

(: cs151-core-apply : (All (a b) (a * -> b) (Listof a) -> b))
(define (cs151-core-apply f xs)
  (error (forbidden-function "apply")))
(provide (rename-out [cs151-core-apply apply]))

(: cs151-core-vector-argmin : (All (X) (X -> Real) (Vectorof X) -> X))
(define (cs151-core-vector-argmin f xs)
  (error (forbidden-function "vector-argmin")))
(provide (rename-out [cs151-core-vector-argmin vector-argmin]))

(: cs151-core-vector-argmax : (All (X) (X -> Real) (Vectorof X) -> X))
(define (cs151-core-vector-argmax f xs)
  (error (forbidden-function "vector-argmax")))
(provide (rename-out [cs151-core-vector-argmax vector-argmax]))

(: cs151-core-object-name : Any -> Any)
(define (cs151-core-object-name x)
  (error (string-append "cs151-core: object-name: "
    "You may not use the built-in function object-name in this course; "
    "you must adopt an approach that does not require it.")))
(provide (rename-out [cs151-core-object-name object-name]))

(: cs151-core-list-set : (All (A) (Listof A) Integer A -> (Listof A)))
(define (cs151-core-list-set xs i y)
  (error (forbidden-function "list-set")))
(provide (rename-out [cs151-core-list-set list-set]))

(: cs151-core-list-update : (All (A) (Listof A) Integer (-> A A) ->
  (Listof A)))
(define (cs151-core-list-update xs i fn)
  (error (forbidden-function "list-update")))
(provide (rename-out [cs151-core-list-update list-update]))

(: cs151-core-append* : (All (a) (-> (Listof (Listof a)) (Listof a))))
(define (cs151-core-append* lst)
  (error (forbidden-function "append*")))
(provide (rename-out [cs151-core-append* append*]))

(: cs151-core-remove : (All (a) (-> Any (Listof a) (Listof a))))
(define (cs151-core-remove a lst)
  (error (forbidden-function "remove")))
(provide (rename-out [cs151-core-remove remove]))

(: cs151-core-remq : (All (a) (-> Any (Listof a) (Listof a))))
(define (cs151-core-remq a lst)
  (error (forbidden-function "remove")))
(provide (rename-out [cs151-core-remq remq]))

(: cs151-core-remove* : (All (a b) (->* ((Listof a) (Listof b)) ((-> a b Boolean)) (Listof b))))
(define (cs151-core-remove* lst1 lst2 [f equal?])
  (error (forbidden-function "remove*")))
(provide (rename-out [cs151-core-remove* remove*]))

(: cs151-core-remove-duplicates : (All (a b)
      (case->
       (->* ((Listof a)) ((-> a a Any) #:key (U (-> a a) False)) (Listof a))
       (->* ((Listof a)) ((-> b b Any) #:key (U (-> a b) False)) (Listof a)))))
(define (cs151-core-remove-duplicates l [=? equal?] #:key [key #f])
  (error (forbidden-function "remove-duplicates")))
(provide (rename-out [cs151-core-remove-duplicates remove-duplicates]))

(: cs151-core-take : (All (a) (-> (Listof a) Integer (Listof a))))
(define (cs151-core-take lst i)
  (error (forbidden-function "take")))
(provide (rename-out [cs151-core-take take]))

(: cs151-core-drop : (All (a) (-> (Listof a) Integer (Listof a))))
(define (cs151-core-drop lst i)
  (error (forbidden-function "drop")))
(provide (rename-out [cs151-core-drop drop]))

(: cs151-core-member : (All (a b)
      (case->
       (-> Any (Listof a) (U (Pairof a (Listof a)) False))
       (-> b (Listof a) (-> b a Any) (U (Pairof a (Listof a)) False)))))
(define cs151-core-member
  (case-lambda
    [(a lst) (error (forbidden-function "member"))]
    [(a lst b) (error (forbidden-function "member"))]))
(provide (rename-out [cs151-core-member member]))

(: cs151-core-list-tail : (All (a) (-> (Listof a) Integer (Listof a))))
(define (cs151-core-list-tail lst i)
  (error (forbidden-function "list-tail")))
(provide (rename-out [cs151-core-list-tail list-tail]))

(: cs151-core-vector-append : (All (a) (-> (Vectorof a) * (Vectorof a))))
(define (cs151-core-vector-append . lst)
  (error (string-append "cs151-core: vector-append: "
    "You may not use the built-in function vector-append in this course; "
    "efficiency dictates that you create a single vector of the "
    "needed length from the start and fill it in, rather than "
    "concatenating smaller vectors.")))
(provide (rename-out [cs151-core-vector-append vector-append]))

(: cs151-core-permutations : (All (a) (-> (Listof a) (Listof (Listof a)))))
(define (cs151-core-permutations lst)
  (error (forbidden-function "permutations")))
(provide (rename-out [cs151-core-permutations permutations]))

(: cs151-core-combinations : (All (a) (-> (Listof a) Nonnegative-Integer (Listof (Listof a)))))
(define (cs151-core-combinations lst n)
  (error (forbidden-function "combinations")))
(provide (rename-out [cs151-core-combinations combinations]))

(: cs151-core-string-split : (-> String (U Regexp String) (Listof String)))
(define (cs151-core-string-split str delim)
  (error (forbidden-function "string-split")))
(provide (rename-out [cs151-core-string-split string-split]))

(: cs151-core-shuffle : (All (a) (Listof a) -> (Listof a)))
(define (cs151-core-shuffle xs)
  (error (forbidden-function "shuffle")))
(provide (rename-out [cs151-core-shuffle shuffle]))

(: forbidden-equality : (-> String String))
(define (forbidden-equality f)
  (string-append "cs151-core: " f
                 ": You may not use the built-in function " f
                 " in this course; you must use a type-specific "
                 "comparison function, such as = for numeric types, "
                 "string=? for Strings, etc."))

(: cs151-core-equal? : (-> Any Any Boolean))
(define (cs151-core-equal? a b)
  (error (forbidden-equality "equal?")))
(provide (rename-out [cs151-core-equal? equal?]))

(: cs151-core-eqv? : (-> Any Any Boolean))
(define (cs151-core-eqv? a b)
  (error (forbidden-equality "eqv?")))
(provide (rename-out [cs151-core-eqv? eqv?]))

(: cs151-core-eq? : (-> Any Any Boolean))
(define (cs151-core-eq? a b)
  (error (forbidden-equality "eq?")))
(provide (rename-out [cs151-core-eq? eq?]))

(define-syntax cs151-core-set!
  (syntax-rules ()
    [(cs151-core-set! var val)
     (error
        "you may not mutate the value of a variable binding in this course")]))
(provide (rename-out [cs151-core-set! set!]))

;; conveniences

(provide (rename-out [string-append cat]))

(provide (rename-out [number->string num$]))
(provide (rename-out [symbol->string sym$]))

(provide (rename-out [Number  Num]))
(provide (rename-out [Integer Int]))
(provide (rename-out [Boolean Bool]))
(provide (rename-out [Symbol  Sym]))

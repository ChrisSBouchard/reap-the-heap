#lang racket
(provide (all-defined-out))

;; Any -> Boolean
;; Is x a well-formed expression?
(define (expr? x)
  (match x
    [(? integer? i) #t]
    [(? boolean? b) #t]
    [(? char? c) #t]
    [(? string? s) #t]
    [(? empty? e) #t]
    [(? symbol? s) (valid-variable? s)]
    [`(,(? prim1?) ,x) (expr? x)]
    [`(,(? prim2?) ,x ,y) (and (expr? x) (expr? y))]
    [`(if ,x ,y ,z)
     (and (expr? x)
          (expr? y)
          (expr? z))]
    [`(cond (,xs ,ys) ... (else ,z))
     (and (andmap expr? xs)
          (andmap expr? ys)
          (expr? z))]
    [`(let ((,vs ,es) ...) ,e)
     (and (andmap valid-variable? vs)
          (andmap expr? es)
          (expr? e))]
    [`(let* ((,vs ,es) ...) ,e)
     (and (andmap valid-variable? vs)
          (andmap expr? e)
          (expr? e))]
    [_ #f]))


;; Symbol -> Boolean
(define (valid-variable? s)
  (and (symbol? s) (not (memq s '(- if cond let add1 sub1 abs zero? integer->char char->integer
                                    char? integer? boolean? string? box? empty? cons? box unbox car cdr string-length
                                    make-string string-ref = < <= char=? boolean=? + cons)))))






;; Expr -> Boolean
;; Is e a closed expression?
(define (closed? e)
  (closed-env? e '()))

(define (closed-env? e env)
  (match e
    [(? integer? i) #t]
    [(? boolean? b) #t]
    [(? char? c) #t]
    [(? string? s) #t]
    [(? empty? e) #t]
    [(? symbol? s) (and (valid-variable? s) (not (not (memq s env))))]
    [`(if ,x ,y ,z)
     (and (closed-env? x env)
          (closed-env? y env)
          (closed-env? z env))]
    [`(,(? prim1?) ,x) (closed-env? x env)]
    [`(,(? prim2?) ,x ,y) (and (closed-env? x env) (closed-env? y env))]
    [`(cond (,xs ,ys) ... (else ,z))
     (and (closed-clauses? xs env)
          (closed-clauses? ys env)
          (closed-env? z env))]
    [`(let ((,vs ,es) ...) ,e)
     (and (check-bindings vs es env)
          (closed-env? e (add-vars vs env)))]
    [`(let* ((,vs ,es) ...) ,e)
     (and (check-*bindings vs es env)
          (closed-env? e (add-vars vs env)))]
    [_ #f]))

(define (check-bindings vs es env)
  (match vs
    ['() #t]
    [(cons v vs) (match es
                   ['() #t] ;should never get here becauses binded lists must be the same length
                   [(cons e es) (and (valid-variable? v)
                                     (closed-env? e env)
                                     (check-bindings vs es env))])]))

(define (check-*bindings vs es env)
  (match vs
    ['{} #t]
    [(cons v vs) (match es
                   ['() #t]
                   [(cons e es) (and (valid-variable? v)
                                     (closed-env? e env)
                                     (check-*bindings vs es (add-vars (list v) env)))])]))           


;; (Listof (Bindings)) (Listof (Variables)) -> (Listof (Variables))
(define (add-vars vs env)
  (match vs
    ['() env]
    [(cons v vs) (add-vars vs (if (not (memq v env)) (append (list v) env) env))]))

(define (closed-clauses? cs env)
  (match cs
    ['() #t]
    [(cons e0 cs) (and (closed-env? e0 env)
                       (closed-clauses? cs env))]))

;; Any -> Boolean
;; Is x a unary primitive?
(define (prim1? x)
  (and (symbol? x)
       (memq x '(add1 sub1 abs - integer->char char->integer
                      car cdr length                      
                      char? integer? boolean? zero? string? box? empty? cons? box unbox car cdr string-length))))

;; Any -> Boolean
;; Is x a binary primitive?
(define (prim2? x)
  (and (symbol? x)
       (memq x '(+
                 cons
                 - make-string string-ref = < <= char=? boolean=? + cons))))





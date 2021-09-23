#lang racket

; da ne izgubimo funkcij `numerator` in `denominator` zaradi "naših" makrojev 
(require (rename-in racket (numerator qnumerator)
                    (denominator qdenominator)))
(provide false true zz qq .. empty s
         if-then-else
         is-zz? is-qq? is-bool? is-seq? is-proper-seq? is-empty? is-set?
         add mul leq? rounding =? right left ~ all? any?
         vars valof fun proc closure call
         gt? inv numerator denominator filtering folding mapping
         fri)

;podatkovni tipi
(struct true () #:transparent)
(struct false () #:transparent)
(struct zz (n) #:transparent)
(struct qq (e1 e2) #:transparent)
(struct .. (e1 e2) #:transparent)
(struct empty () #:transparent)
(struct s (es) #:transparent)

;nadzor toka
(struct if-then-else (condition e1 e2) #:transparent)
(struct is-zz? (e1) #:transparent)
(struct is-qq? (e1) #:transparent)
(struct is-bool? (e1) #:transparent)
(struct is-seq? (e1) #:transparent)
(struct is-proper-seq? (e1) #:transparent)
(struct is-empty? (e1) #:transparent)
(struct is-set? (e1) #:transparent)
(struct add (e1 e2) #:transparent)
(struct mul (e1 e2) #:transparent)
(struct leq? (e1 e2) #:transparent)
(struct rounding (e1) #:transparent)
(struct =? (e1 e2) #:transparent)
(struct left (e1) #:transparent)
(struct right (e1) #:transparent)
(struct ~ (e1) #:transparent)
(struct all? (e1) #:transparent)
(struct any? (e1) #:transparent)

;spremenljivke
(struct vars (s e1 e2) #:transparent)
(struct valof (s) #:transparent)

;funkcije
(struct closure (env f) #:transparent)
(struct call (e args) #:transparent)
(struct fun (name farg body) #:transparent)
(struct proc (name body) #:transparent)

;interpreter
(define (fri e env)
  (letrec ([fri2 (lambda (e env)
  (cond [(true? e) e]
        [(false? e) e]
        [(zz? e) e]
        [(qq? e)
         (qq (zz(qnumerator(/(zz-n(fri2(qq-e1 e)env))(zz-n(fri2(qq-e2 e)env))))) (zz(qdenominator(/(zz-n(fri2(qq-e1 e)env))(zz-n(fri2(qq-e2 e)env))))))]
        [(..? e) (.. (fri2(..-e1 e)env) (fri2(..-e2 e)env))]
        [(empty? e) e]
        [(s? e) (s(for/set ([i (s-es e)]) (fri2 i env)))]

        [(proc? e) e]

        [(fun? e) (closure env e)] ;SE PROCEDURE IN OPTIMIZACIJO OVOJNIC

        [(call? e) (let ([o (fri2 (call-e e) env)])
                     (if (closure? o)
                         (fri2 (fun-body(closure-f o)) (append env (list(cons(fun-name(closure-f o)) o))));(append (append (closure-env o) (apply map cons (list (fun-farg(closure-f o)) (call-args e)))) (list(cons(fun-name(closure-f o)) o))))
                         (fri2 (proc-body o) (append env (list(cons(proc-name o) o))))))]

        [(vars? e)
         (if (and (list? (vars-s e)) (list? (vars-e1 e)))
             (let ([v1 (for/list ([i (vars-e1 e)])(fri2 i env))])
                       (fri2 (vars-e2 e) 
                                          (append (apply map cons (list (vars-s e) v1)) env )))
                                          
                       (fri2 (vars-e2 e) (append (list(cons (vars-s e) (fri2(vars-e1 e)env))) env )))]

        [(valof? e)
         (cdr(assoc (valof-s e) env))]
        
        
        [(if-then-else? e)
         (if (equal? (false) (fri2(if-then-else-condition e) env)) (fri2(if-then-else-e2 e) env) (fri2(if-then-else-e1 e) env))]

        [(is-zz?? e)
         (if (zz? (fri2(is-zz?-e1 e)env)) (true) (false))]

        [(is-qq?? e)
         (if (qq? (fri2(is-qq?-e1 e)env)) (true) (false))]

        [(is-bool?? e)
         (if (true? (fri2(is-bool?-e1 e)env)) (true) (if (false? (fri2(is-bool?-e1 e)env)) (true) (false)))]

        [(is-seq?? e)
         (if (..? (fri2(is-seq?-e1 e)env)) (true) (false))]

        [(is-proper-seq?? e)
         (let ([v1 (fri2 (is-proper-seq?-e1 e) env)])
         (if (empty? (..-e2 v1)) (true) (if (..?(..-e2 v1)) (fri2(is-proper-seq? (..-e2 v1))env) (false))))];(if (empty? (..-e2(..-e2(is-proper-seq?-e1 e)))) (true) (false)) (false)))] ;nevem ce samo za dva al za vec
         
        [(is-empty?? e)
         (if (empty? (fri2(is-empty?-e1 e)env)) (true) (false))] ;nevem če prou

        [(is-set?? e)
         (if (s? (fri2(is-set?-e1 e)env)) (true) (false))]

        [(add? e)
         (let ([v1 (fri2 (add-e1 e) env)]
               [v2 (fri2 (add-e2 e) env)])
           (cond [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                  (if (and (false? v1) (false? v2)) (false) (true))]
                 [(and (zz? v1) (zz? v2))
                  (zz (+ (zz-n v1) (zz-n v2)))]
                 [(or(and (qq? v1) (qq? v2)) (and (zz? v1) (qq? v2)) (and (qq? v1) (zz? v2)))
                  (cond
                    [(and (qq? v1) (qq? v2)) (qq (zz(qnumerator(+ (/ (zz-n(qq-e1 v1))(zz-n(qq-e2 v1))) (/ (zz-n(qq-e1 v2))(zz-n(qq-e2 v2)))))) (zz(qdenominator(+ (/ (zz-n(qq-e1 v1))(zz-n(qq-e2 v1))) (/ (zz-n(qq-e1 v2))(zz-n(qq-e2 v2)))))))]
                    [(and (zz? v1) (qq? v2)) (qq (zz(qnumerator(+ (zz-n v1) (/ (zz-n(qq-e1 v2))(zz-n(qq-e2 v2)))))) (zz(qdenominator(+ (zz-n v1) (/ (zz-n(qq-e1 v2))(zz-n(qq-e2 v2)))))))]
                    [(and (qq? v1) (zz? v2)) (qq (zz(qnumerator(+ (/ (zz-n(qq-e1 v1))(zz-n(qq-e2 v1)))(zz-n v2)))) (zz(qdenominator(+ (/ (zz-n(qq-e1 v1))(zz-n(qq-e2 v1)))(zz-n v2)))))]
                    )]
                 [(and (..? v1) (..? v2))
                  (if (empty?(..-e2 v1)) (..(..-e1 v1)v2) (..(..-e1 v1)(fri2(add (..-e2 v1) v2)env)))]
                 [(and (s? v1) (s? v2))
                  (s (set-union (s-es v1) (s-es v2)))]
                 [(and (empty? v1)(empty? v2)) (empty)]
           )
         )]

        [(mul? e)
         (let ([v1 (fri2 (mul-e1 e) env)]
               [v2 (fri2 (mul-e2 e) env)])
           (cond [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                  (if (and (true? v1) (true? v2)) (true) (false))]
                 [(and (zz? v1) (zz? v2))
                  (zz (* (zz-n v1) (zz-n v2)))]
                 [(or(and (qq? v1) (qq? v2)) (and (zz? v1) (qq? v2)) (and (qq? v1) (zz? v2)))
                  (cond
                    [(and (qq? v1) (qq? v2)) (qq (zz(qnumerator(* (/ (zz-n(qq-e1 v1))(zz-n(qq-e2 v1))) (/ (zz-n(qq-e1 v2))(zz-n(qq-e2 v2)))))) (zz(qdenominator(* (/ (zz-n(qq-e1 v1))(zz-n(qq-e2 v1))) (/ (zz-n(qq-e1 v2))(zz-n(qq-e2 v2)))))))]
                    [(and (zz? v1) (qq? v2)) (qq (zz(qnumerator(* (zz-n v1) (/ (zz-n(qq-e1 v2))(zz-n(qq-e2 v2)))))) (zz(qdenominator(* (zz-n v1) (/ (zz-n(qq-e1 v2))(zz-n(qq-e2 v2)))))))]
                    [(and (qq? v1) (zz? v2)) (qq (zz(qnumerator(* (/ (zz-n(qq-e1 v1))(zz-n(qq-e2 v1)))(zz-n v2)))) (zz(qdenominator(* (/ (zz-n(qq-e1 v1))(zz-n(qq-e2 v1)))(zz-n v2)))))]
                    )]
                 [(and (s? v1) (s? v2))
                  (s(for/set ([i (list->set(cartesian-product (set->list(s-es v1)) (set->list(s-es v2))))]) (.. (list-ref i 0) (list-ref i 1))))];(s(list->set(cartesian-product (set->list(s-es v1)) (set->list(s-es v2)))))]
           )
         )]

        [(leq?? e)
         (letrec ([v1 (fri2 (leq?-e1 e) env)]
               [v2 (fri2 (leq?-e2 e) env)]
               [stetje (lambda (zap st)
                         (cond [(..? (..-e2 zap)) (stetje (..-e2 zap) (+ st 1))] [else st]))])
           (cond [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                  (if (and (true? v1) (false? v2)) (false) (true))]
                 [(or(and (qq? v1) (qq? v2)) (and (zz? v1) (qq? v2)) (and (qq? v1) (zz? v2))(and (zz? v1) (zz? v2)))
                  (cond
                    [(and (qq? v1) (qq? v2)) (if (<= (/ (zz-n(qq-e1 v1))(zz-n(qq-e2 v1)))(/ (zz-n(qq-e1 v2))(zz-n(qq-e2 v2)))) (true) (false))]
                    [(and (zz? v1) (qq? v2)) (if (<= (zz-n v1)(/ (zz-n(qq-e1 v2))(zz-n(qq-e2 v2)))) (true) (false))]
                    [(and (qq? v1) (zz? v2)) (if (<= (/ (zz-n(qq-e1 v1))(zz-n(qq-e2 v1)))(zz-n v2)) (true) (false))]
                    [(and (zz? v1) (zz? v2)) (if (<= (zz-n v1)(zz-n v2)) (true) (false))]
                    )]
                 [(and (..? v1) (..? v2))
                  (if (<= (stetje v1 0)(stetje v2 0)) (true) (false))] ;more stej kokrat ..
                 [(and (s? v1) (s? v2))
                  (if (subset? (s-es v1) (s-es v2)) (true) (false))]
           ))]

        [(rounding? e)
         (let ([v1 (fri2 (rounding-e1 e) env)])
         (cond [(qq? v1) (zz(round(/(zz-n(qq-e1 v1))(zz-n(qq-e2 v1)))))]
               [(zz? v1) v1]
           ))] ;nevem za zz če prou

        [(=?? e)
         (if (equal? (fri2(=?-e1 e) env) (fri2(=?-e2 e) env)) (true) (false))]

        [(left? e)
         (let ([v1 (fri2 (left-e1 e) env)])
         (cond [(qq? v1) (qq-e1 v1)]
               [(..? v1)
                  (fri2 (..-e1 v1) env)]
               [(s? v1) (fri2 (set-first(s-es v1)))]
           ))]

        [(right? e)
         (let ([v1 (fri2 (right-e1 e) env)])
         (cond [(qq? v1) (qq-e2 v1)]
               [(..? v1)
                  (fri2 (..-e2 v1) env)]
               [(s? v1) (fri2 (s(set-rest(s-es v1))))]
           ))]

        [(~? e)
         (let ([v1 (fri2 (~-e1 e) env)])
         (cond [(true? v1)(false)]
               [(false? v1)(true)]
               [(zz? v1)(zz (- (zz-n v1)))]
               [(qq? v1)(qq (zz(- (zz-n(qq-e1 v1))))(qq-e2 v1))]
           ))]

        [(all?? e)
         (let ([v1 (fri2 (all?-e1 e) env)])
         (cond
           [(..? v1)
                  (if (or(equal?(..-e1 v1)(false)) (equal?(if (..? (..-e2 v1))(fri2(all?(..-e2 v1))env)(..-e2 v1))(false))) (false) (true))]
           [(s? v1)(if (set-member? (s-es v1) (false)) (false) (true))]
           [(empty? v1)(true)]
           ))]

        [(any?? e)
         (let ([v1 (fri2 (any?-e1 e) env)])
         (cond
           [(..? v1)
                  (if (and(equal?(..-e1 v1)(false)) (or(equal?(if (..? (..-e2 v1))(fri2(any?(..-e2 v1))env)(..-e2 v1))(empty))(equal?(if (..? (..-e2 v1))(fri2(any?(..-e2 v1))env)(..-e2 v1))(false)))) (false) (true))]
           [(s? v1)(if (or(equal? (s-es v1) (set (false)))(equal? (s-es v1) (set))) (false) (true))]
           [(empty? v1)(false)]
           ))]
))])
    (fri2 e null)))

;makro sistemi
(define (numerator e1)
  (left e1))

(define (denominator e1)
  (right e1))

(define(gt? e1 e2)
  (leq? e1 e2))

(define(inv e1)
  (cond
    [(zz? e1)(qq(zz 1) e1)]
    [(qq? e1)(qq(right e1)(left e1))]
    [(..? e1)(e1)];nedela
    [(empty? e1)(empty)]
    ))

(define(mapping f seq)
  (f))

(define(filtering f seq)
  (f))

(define(folding f init seq)
  (true))



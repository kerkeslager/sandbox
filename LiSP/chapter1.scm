; this allows (begin), which is almost certainly undesirable behavior
(define (eprogn exps env trace)
  (if (pair? exps)
      (if (pair? (cdr exps))
          (begin (evaluate (car exps) env trace)
                 (eprogn (cdr exps) env trace))
          (evaluate (car exps env trace)))
      '()))

; Complication is to avoid excess recursion (exercise 1.2)
(define (evlis exps env trace)
  (define (evlis-internal exps env trace)
    (let ((current-result (evaluate (car exps) env trace)))
      (cons current-result (if (pair? (cdr exps))
                               (evlis (cdr exps) env trace)
                               '()))))
  (if (pair? exps)
      (evlis-internal exps env trace)
      '()))

(define (lookup id env)
  (if (pair? env)
      (if (eq? (caar env) id)
          (cdar env)
          (lookup id (cdr env)))
      (wrong "No such binding" id)))

(define (update! id env value)
  (if (pair? env)
      (if (eq? (caar env) id)
          (begin (set-cdr! (car env) value)
                 value)
          (update! id (cdr env) value))
      (wrong "No such binding" id)))

(define (extend env variables values)
  (cond ((pair? variables) (if (pair? values)
                               (cons (cons (car variables) (car values))
                                     (extend env (cdr variables) (cdr values)))
                               (wrong "Too many variables")))
        ((null? variables) (if (null? values)
                               env
                               (wrong "Too many values")))
        ; I'm not sure I like this, it creates an inconsistent
        ; interface for extend
        ((symbol? variables?) (cons (cons variables values) env))))

(define (invoke fn args)
  (if (procedure? fn)
      (fn args)
      (wrong "Not a function" fn)))

; variables are lexically scoped
(define (make-function variables body env trace)
  (lambda (values) (eprogn body (extend env variables values) trace)))

(define env.init '())

(define env.global env.init)

(define-syntax def-initial
  (syntax-rules ()
    ((def-initial name) (begin (set! env.global (cons (cons 'name 'void) env.global))
                               'name))
    ((def-initial name value) (begin (set! env.global (cons (cons 'name value) env.global))
                                     'name))))

(define-syntax def-primitive
  (syntax-rules ()
    ((def-primitive name value arity) (def-initial name (lambda (values) (if (= arity (length values))
                                                                        (apply value values) ; the real apply of Scheme
                                                                        (wrong "Incorrect arity" (list 'name values))))))))


; Implementing the exercise suggested in section 1.8
(define exit-sentinel (gensym 'exit-sentinel-))
(define (exit) exit-sentinel)

(def-initial true #t)
(def-initial false #f)
(def-initial nil '())

(def-primitive cons cons 2)
(def-primitive car car 1)
(def-primitive cdr cdr 1)
(def-primitive set-car! set-car! 2)
(def-primitive set-cdr! set-cdr! 2)
(def-primitive + + 2)
(def-primitive - - 2)
(def-primitive * * 2)
(def-primitive / / 2)
(def-primitive eq? eq? 2)
(def-primitive < < 2)
(def-primitive > > 2)
(def-primitive <= <= 2)
(def-primitive >= >= 2)
(def-primitive exit exit 0)

(define (atom? e)
  (not (pair? e)))

(define (display-args args)
  (if (pair? args)
      (begin (display " ")
             (display (car args))
             (display-args (cdr args)))))

; `trace` is there to implement exercise 1.1
; Pass #t to output trace information
(define (evaluate e env trace)
  (if (atom? e)
      (cond ((symbol? e) (lookup e env))
            ((or (number? e) (string? e) (char? e) (boolean? e) (vector? e)) e)
            (else (wrong "Cannot evaluate" e)))
      (case (car e)
        ((quote) (cadr e))
        ((if) (if (evaluate (cadr e) env trace)
                  (evaluate (caddr e) env trace)
                  (evaluate (cadddr e) env trace)))
        ((begin) (eprogn (cdr e) env trace))
        ((set!) (update! (cadr e) env (evaluate (caddr e) env trace)))
        ((lambda) (make-function (cadr e) (cddr e) env trace))
        (else
         (let ((fn (evaluate (car e) env trace))
               (args (evlis (cdr e) env trace)))
           (begin (if trace
                      (begin (display "(")
                             (display (car e))
                             (display-args args)
                             (display ")\n")))
                  (let ((result (invoke fn args)))
                    (begin (display result)
                           (display "\n")
                           result))))))))

(define (chapter1-scheme trace)
  (define (toplevel)
    (let ((result (evaluate (read) env.global trace)))
      (if (not (and (symbol? result) (equal? result exit-sentinel)))
          (begin (display result)
                 (display "\n")
                 (toplevel)))))
  (toplevel))

#lang racket
(require "./Genetic-Algorithm.rkt")

(define Maximum-point-of-a-function-by-GA ;in region [0,1]
  (lambda (function)
    ;;return: the point that gives the Maximum-value and the Maximum-value
    (let* ([count 0];;to count the times of calling fitness
           [crossover
            (lambda (father mother)
              (let* ([alpha (random)]
                     [beta (- 1 alpha)])
                (+ (* alpha father)
                   (* beta mother))))]
           [mutation
            (lambda (x)
              (let [(mutation-value (- (random) 0.5))]
                (+ x
                   (* 2 mutation-value (min x (- 1 x))))))]; new-x is still in [0, 1]
           [fitness
            (lambda (x)
              (set! count (+ 1 count))
              (exp (function x)))]
           [population 500]
           [initialization-generation (map (lambda (i) (random)) (make-list population 0))]
           [fitness_threshold 1e-8])
      (let ([result (Genetic-Algorithm initialization-generation population fitness fitness_threshold crossover mutation)])
        (display "The number of times of calling fitness is: ")
        (display count)
        (newline)
        (display "'(maximum-point . maximum-value) is: ")
        (newline)
        (cons result (function result))))))

;;test-1
;;function: f(x) = sin(pi*x)
;;the maximum  is 1 at 1/2.
(Maximum-point-of-a-function-by-GA (lambda (x) (sin (* pi x))))

;;test-2
;;function: f(x)= -x * (x-1/3)^2 * (x-1)
;;the maximum is 0.03498052356790557 at x= (11+sqrt(73))/24 ~ 0.8143334893882305.
;;another extreme-point is at x = (11+sqrt(73))/24 ~ 0.10.
(Maximum-point-of-a-function-by-GA (lambda (x) (* x (expt (- x 1/3) 2) (- 1 x))))

;;test-3
;;function: f(x) = 9*x + 10*sin(45*x) + 7*cos(36*x)
(Maximum-point-of-a-function-by-GA (lambda (x)
                                     (+ (* x 9.0)
                                        (* 10 (sin (* x 45)))
                                        (* 7 (cos (* x 36))))))

#lang racket
(provide Genetic-Algorithm)

;;; Genetic Algorithm
;;
;;;Input:
;;initial-generation: a list of individuals
;;population: the number of a generation
;;fitness: a positive-function whose maximum is finite(need not be known)
;;fitness_threshold: control the value |fitness - maximum_fitness|/maximum_fitness
;;crossover: a function of 2 individuals
;;mutation: a function of 1 individual

;;;Output: the best individual whose fitness-value is largest.

;;;The idea of Genetic Algorithm
;; start from a arbitary generation
;;1)use the fitness function to select survied-individuals, called fathers ans mothers
;;2)then crossover the selected father and mother to get childs
;;3)mutate the childs: thus a 'better' generation is generated
;;loop above procedure until approach the fitness_threshold
;;Here I improved some:
;;a)childs are replaced by the best of the three: father,mother,child
;;b)after mutation, if the individual is not better, do not use the result of mutation

;;;some functions are defined below:
;;1)
;;aux-probability-of-surving: the pdf(unnormed) to select one individual from a generation
;;select-one-individual-with-fitness
;;2)
;;best-individual-with-fitness-under-crossover: choose the best from father, mother, child
;;3)
;;new-generation-with-fitness: from a generation to get a new generation(selection, crossover, mutation)
;;the-best-individual-of-generation

;; main-function: Genetic-Algorithm:
;;;iterate the generation by: new-generation-with-fitness
;;;to end the iteration, find the best-individual-of-a-generation and control it by fitness_threshold

(define (aux-probability-of-surving generation-with-fitness)
  ;;the pdf(unnormed) to select the individuals of the generation
  ;;generation-with-fitness is a pair of two lists: (generation . fitness-of-the-generation)
  (define fitness-of-the-generation (cdr generation-with-fitness))
  (let iter ([now (car fitness-of-the-generation)]
             [rest (cdr fitness-of-the-generation)])
         (if (null? rest)
             (list now)
             (cons now
                   (iter (+ now (car rest)) (cdr rest))))))

(define (select-one-individual-with-fitness
         generation-with-fitness
         population)
  ;;by the probability to select one inidividual
  ;;return: (individual . the-fitness-of-the-individual)
  (define aux-probability (aux-probability-of-surving generation-with-fitness))
  (define sum-of-aux (list-ref aux-probability (- population 1)))
  (let [(pdf (* (random) sum-of-aux))]
      (let iter [(i 0)]
        (if (<= pdf (list-ref aux-probability i))
            (cons (list-ref (car generation-with-fitness) i)
                  (list-ref (cdr generation-with-fitness) i))
            (iter (+ i 1))))))

(define (best-individual-with-fitness-under-crossover
         father-with-fitness mother-with-fitness fitness crossover)
  ;;choose the best individual in father, mother, and the-individual-under-crossover
  ;;return: (best-individual . the-fitness-of-the-individual)
  (let* ([father (car father-with-fitness)]
         [fitness-of-father (cdr father-with-fitness)]
         [mother (car mother-with-fitness)]
         [fitness-of-mother (cdr mother-with-fitness)]
         [individual (crossover father mother)]
         [fitness-of-the-individual (fitness individual)])
    (cond [(>= fitness-of-the-individual
               (max fitness-of-father fitness-of-mother))
           (cons individual
                 fitness-of-the-individual)]
          [(>= fitness-of-father
               (max fitness-of-the-individual fitness-of-mother))
           father-with-fitness]
          [else
           mother-with-fitness])))

(define (new-generation-with-fitness
         generation-with-fitness population fitness crossover mutation)
  ;;generate a new-generation from a generation through: selection -> crossover -> mutation
  ;;return: (new-generation . fitness-of-the-generation)
  (define generation (car generation-with-fitness))
  (define fitness-of-the-generation (cdr generation-with-fitness))
  (define (individual-of-next-generation-with-fitness)
    (let ([father-with-fitness (select-one-individual-with-fitness
                                generation-with-fitness population)]
          [mother-with-fitness (select-one-individual-with-fitness
                                generation-with-fitness population)])
      (let* ([after-crossover (best-individual-with-fitness-under-crossover
                               father-with-fitness
                               mother-with-fitness
                               fitness
                               crossover)]
             [child (mutation (car after-crossover))]
             [fitness-of-child (fitness child)])
        (if (>= fitness-of-child (cdr after-crossover));only accept improved case
            (cons child fitness-of-child)
            after-crossover))))
  (let ([next-generation (map (lambda (i) (individual-of-next-generation-with-fitness))
                              (make-list population 0))])
    (cons (map car next-generation) (map cdr next-generation))))


(define (the-best-individual-of-generation generation-with-fitness)
  ;;return the best-individual (with its fitness) of a generation
  (define the-generation (car generation-with-fitness))
  (define fitness-of-the-generation (cdr generation-with-fitness))
  (let aux ([rest-generation (cdr the-generation)]
            [rest-fitness (cdr fitness-of-the-generation)]
            [tentative-individual (car the-generation)]
            [tentative-fitness (car fitness-of-the-generation)])
    (cond [(null? rest-fitness)
           (cons tentative-individual tentative-fitness)]
          [(<= (car rest-fitness) tentative-fitness)
           (aux (cdr rest-generation)
                (cdr rest-fitness)
                tentative-individual
                tentative-fitness)]
          [else (aux (cdr rest-generation)
                     (cdr rest-fitness)
                     (car rest-generation)
                     (car rest-fitness))])))

;;main function
(define (Genetic-Algorithm initial-generation
                           population
                           fitness
                           fitness_threshold
                           crossover
                           mutation)
  ;;return an individual whose fitness approach the fitness_threshold
  (define initial-generation-with-fitness
    (cons initial-generation
          (map fitness initial-generation)))
  (let iter ([generation-with-fitness
              (new-generation-with-fitness initial-generation-with-fitness
                                           population
                                           fitness
                                           crossover
                                           mutation)]
             [current-best-individual-with-fitness
              (the-best-individual-of-generation initial-generation-with-fitness)])
    (display "Fitness-value: ");;to view the evolution of generations
    (display (cdr current-best-individual-with-fitness))
    (newline)
    (define the-best-individual-with-fitness
      (the-best-individual-of-generation generation-with-fitness))
    (let ([tol (abs (/ (- (cdr the-best-individual-with-fitness)
                          (cdr current-best-individual-with-fitness))
                       (cdr the-best-individual-with-fitness)))])
      (if (and (> tol 0)
               (<= tol fitness_threshold))
          (car the-best-individual-with-fitness)
          (iter (new-generation-with-fitness generation-with-fitness population
                                             fitness crossover mutation)
                the-best-individual-with-fitness)))))


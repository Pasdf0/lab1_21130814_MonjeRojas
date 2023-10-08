#lang Racket

(provide (all-defined-out))

(define InvalidOption null)
(define EmptyList (list ))

;--------------------Funciones Generales

(define all-strings? (lambda (lista)
        (andmap string? lista)))

(define all-lowercase (lambda (lista)
        (map string-downcase lista)))

(define epi? (lambda (n) (or (exact-positive-integer? n) (= n 0))))

;--------------------Constructor

(define option (lambda (Code Message ChatbotCodeLink InitialFlowCodeLink . Keywords)
        (if (and (epi? Code)(string? Message)(epi? ChatbotCodeLink)(epi? InitialFlowCodeLink))
            (list Code Message ChatbotCodeLink InitialFlowCodeLink (if (all-strings? Keywords)
                                                                       (all-lowercase Keywords) null))
            InvalidOption)))

;--------------------Pertenencia

(define option? (lambda (op)                  
        (and (list? op)(= (length op) 5)(epi? (car op))(string? (cadr op))(epi? (caddr op))
             (epi? (cadddr op))(all-strings? (car (cddddr op))))))

(define all-option? (lambda (list-op)
        (andmap option? list-op)))

;--------------------Selectores

(define Sel-op-code (lambda (op)
        (if (option? op)
            (list-ref op 0)
            null)))

(define Sel-op-msg (lambda (op)
        (if (option? op)
            (list-ref op 1)
            null)))

(define Sel-op-cbcodelink (lambda (op)
        (if (option? op)
            (list-ref op 2)
            null)))

(define Sel-op-fwcodelink (lambda (op)
        (if (option? op)
            (list-ref op 3)
            null)))

(define Sel-op-keywords (lambda (op)
        (if (option? op)
            (list-ref op 4)
            null)))

;--------------------Modificadores

(define dup-list-op (lambda (list-op)
        (define compare-code (lambda (op1 op2)
                (= (Sel-op-code op1) (Sel-op-code op2))))
        (remove-duplicates list-op compare-code)))


(define change-op-code (lambda (op n)
        (if (and (option? op) (integer? n))
            (option n (Sel-op-msg op) (Sel-op-cbcodelink op)
                    (Sel-op-fwcodelink op) (Sel-op-keywords op))
            InvalidOption)))
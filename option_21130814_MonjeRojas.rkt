#lang Racket

(provide (all-defined-out))

;############### TDA Option ###############

(define InvalidOption null)
(define EmptyList (list ))

;--------------------Funciones Generales

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define all-strings? (lambda (lista)
        (andmap string? lista)))

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define all-lowercase (lambda (lista)
        (map string-downcase lista)))

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define epi? (lambda (n)
        (or (exact-positive-integer? n) (= n 0))))

;--------------------Constructor

; Funcionalidad 2 Constructor Option

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define option (lambda (Code Message ChatbotCodeLink InitialFlowCodeLink . Keywords)
        (if (and (epi? Code)(string? Message)(epi? ChatbotCodeLink)(epi? InitialFlowCodeLink))
            (list Code Message ChatbotCodeLink InitialFlowCodeLink (if (all-strings? Keywords)
                                                                       (all-lowercase Keywords) null))
            InvalidOption)))

;--------------------Pertenencia

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define option? (lambda (op)                  
        (and (list? op)(= (length op) 5)(epi? (car op))(string? (cadr op))(epi? (caddr op))
             (epi? (cadddr op))(all-strings? (car (cddddr op))))))

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define all-option? (lambda (list-op)
        (andmap option? list-op)))

;--------------------Selectores

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define Sel-op-code (lambda (op)
        (if (option? op)
            (list-ref op 0)
            null)))

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define Sel-op-msg (lambda (op)
        (if (option? op)
            (list-ref op 1)
            null)))

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define Sel-op-cbcodelink (lambda (op)
        (if (option? op)
            (list-ref op 2)
            null)))

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define Sel-op-fwcodelink (lambda (op)
        (if (option? op)
            (list-ref op 3)
            null)))

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define Sel-op-keywords (lambda (op)
        (if (option? op)
            (list-ref op 4)
            null)))

;--------------------Modificadores

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define dup-list-op (lambda (list-op)
        (define compare-code (lambda (op1 op2)
                (= (Sel-op-code op1) (Sel-op-code op2))))
        (remove-duplicates list-op compare-code)))

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define change-op-code (lambda (op n)
        (if (and (option? op) (integer? n))
            (option n (Sel-op-msg op) (Sel-op-cbcodelink op)
                    (Sel-op-fwcodelink op) (Sel-op-keywords op))
            InvalidOption)))
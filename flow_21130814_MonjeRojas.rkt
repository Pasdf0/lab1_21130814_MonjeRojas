#lang Racket

(require "option_21130814_MonjeRojas.rkt")
(provide (all-defined-out))

;############### TDA Flow ###############

(define InvalidFlow null)

;--------------------Constructor

; Funcionalidad 3 Constructor Flow

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define flow (lambda (Flow-ID msg . Options)
        (if (and (epi? Flow-ID)(string? msg))
            (list Flow-ID msg (if (all-option? Options)
                                   (dup-list-op Options) null))
            InvalidFlow)))

;--------------------Pertenencia

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define flow? (lambda (fw)
        (and (list? fw)(= (length fw) 3)(epi? (car fw))
             (string? (cadr fw))(all-option? (caddr fw)))))

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define all-flow? (lambda (list-fw)
        (andmap flow? list-fw)))

;--------------------Selectores

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define Sel-fw-id (lambda (fw)
        (if (flow? fw)
            (list-ref fw 0)
            null)))

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define Sel-fw-msg (lambda (fw)
        (if (flow? fw)
            (list-ref fw 1)
            null)))

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define Sel-fw-op (lambda (fw)
        (if (flow? fw)
            (list-ref fw 2)
            null)))

;--------------------Modificadores

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define dup-list-fw (lambda (list-fw)
        (define compare-fw-id (lambda (fw1 fw2)
                (= (Sel-fw-id fw1) (Sel-fw-id fw2))))
        (remove-duplicates list-fw compare-fw-id)))

; Funcionalidad 4 Flow add Option

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define change-fw-op (lambda (fw list-op)
        (define delete-fw-op (lambda (fw)
                (remove (Sel-fw-op fw) fw)))
        (if (and (flow? fw)(all-option? list-op))
            (append (delete-fw-op fw) list-op)
            InvalidFlow)))

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define flow-add-option (lambda (fw op)
        (if (and (flow? fw)(option? op))
            (change-fw-op fw (dup-list-op (append (Sel-fw-op fw) (list op))))
            InvalidFlow)))
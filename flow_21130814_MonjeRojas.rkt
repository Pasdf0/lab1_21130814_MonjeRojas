#lang Racket

(require "option_21130814_MonjeRojas.rkt")
(provide (all-defined-out))

;############### TDA Flow ###############

;REPRESENTACIÓN TDA
;flow: Flow-ID(positive integer)x msg(string) x Options(list)

(define InvalidFlow null) ;Se define Flow no aceptado para retorno


;--------------------Pertenencia

;Dominio: Any
;Recorrido: Boolean
;Recursividad: No aplica
;Descripción: Comprueba que la entrada sea un flow.
(define flow? (lambda (fw)
        (and (list? fw)(= (length fw) 3)(epi? (car fw))
             (string? (cadr fw))(all-option? (caddr fw)))))

;Dominio: List
;Recorrido: Boolean
;Recursividad: No aplica
;Descripción: Comprueba que cada elemento de una lista sea un flow.
(define all-flow? (lambda (list-fw)
        (andmap flow? list-fw)))

;--------------------Selectores

;Dominio: Flow
;Recorrido: id(positive integer)
;Recursividad: No aplica
;Descripción: Retorna la id de un flow.
(define Sel-fw-id (lambda (fw)
        (if (flow? fw)
            (list-ref fw 0)
            null)))

;Dominio: Flow
;Recorrido: msg(string)
;Recursividad: No aplica
;Descripción: Retorna el msg de un flow.
(define Sel-fw-msg (lambda (fw)
        (if (flow? fw)
            (list-ref fw 1)
            null)))

;Dominio: Flow
;Recorrido: List of Options(list)
;Recursividad: No aplica
;Descripción: Retorna la lista de Options de un flow.
(define Sel-fw-op (lambda (fw)
        (if (flow? fw)
            (list-ref fw 2)
            null)))

;--------------------Modificadores

;Dominio: List of Flows(list)
;Recorrido: List of Flows(list)
;Recursividad: No aplica
;Descripción: Elimina flow duplicados en una lista a partir de su id.
(define dup-list-fw (lambda (list-fw)
        (define compare-fw-id (lambda (fw1 fw2)
                (= (Sel-fw-id fw1) (Sel-fw-id fw2))))
        (remove-duplicates list-fw compare-fw-id)))

;Dominio: Flow x List of Options(list)
;Recorrido: Flow
;Recursividad: No aplica
;Descripción: Reemplaza la lista de options de un flow por la indicada.
(define change-fw-op (lambda (fw list-op)
        (define delete-fw-op (lambda (fw)
                (remove (Sel-fw-op fw) fw)))
        (if (and (flow? fw)(all-option? list-op))
            (append (delete-fw-op fw) (list list-op))
            InvalidFlow)))
#lang Racket

(provide (all-defined-out))

;############### TDA Option ###############

;REPRESENTACIÓN TDA
;option: Code(positive integer) x Message(string) x ChatbotCodeLink(positive integer) x
;        InitialFlowCodeLink(positive integer) x Keywords(list)

(define InvalidOption null) ;Se define Option no aceptada para retorno
(define EmptyList (list )) ;Se define lista vacía de uso general

;--------------------Funciones Generales

;Dominio: List
;Recorrido: Boolean
;Recursividad: No aplica
;Descripción: Comprueba que todos los elementos de una lista sean strings.
(define all-strings? (lambda (lista)
        (andmap string? lista)))

;Dominio: List
;Recorrido: List
;Recursividad: No aplica
;Descripción: Cambia todos los string de una lista a minúscula.
(define all-lowercase (lambda (lista)
        (map string-downcase lista)))

;Dominio: Any
;Recorrido: Boolean
;Recursividad: No aplica
;Descripción: Se define epi como un entero exacto positivo o el valor 0,
;             su uso principal es comprobar que un dato entregado sirve como id.
;             Esta redefinición se usa en la mayoría del proyecto.
(define epi? (lambda (n)
        (or (exact-positive-integer? n) (equal? n 0))))

;Dominio: Lista de strings
;Recorrido: string
;Recursividad: No aplica
;Descripción: Junta los strings de una lista agregando un salto de linea entre ellos, sirve para
;             formatear las opciones almacenadas en el chathistory.
(define str-append-cola (lambda (lista)
        (map (lambda (arg) (string-append arg "\n")) lista)))

;--------------------Pertenencia

;Dominio: Any
;Recorrido: Boolean
;Recursividad: No aplica
;Descripción: Verifica que el argumento entregado sea un option.
(define option? (lambda (op)                  
        (and (list? op)(= (length op) 5)(epi? (car op))(string? (cadr op))(epi? (caddr op))
             (epi? (cadddr op))(all-strings? (car (cddddr op))))))

;Dominio: List
;Recorrido: Boolean
;Recursividad: No aplica
;Descripción: Comprueba que todos los elementos de una lista sean option.
(define all-option? (lambda (list-op)
        (andmap option? list-op)))

;--------------------Selectores

;Dominio: Option
;Recorrido: Code(positive integer)
;Recursividad: No aplica
;Descripción: Retorna el code de un option.
(define Sel-op-code (lambda (op)
        (if (option? op)
            (list-ref op 0)
            null)))

;Dominio: Option
;Recorrido: msg(string)
;Recursividad: No aplica
;Descripción: Retorna el Message de un option.
(define Sel-op-msg (lambda (op)
        (if (option? op)
            (list-ref op 1)
            null)))

;Dominio: Option
;Recorrido: ChatbotCodeLink(positive integer)
;Recursividad: No aplica
;Descripción: Retorna el ChatbotCodeLink de un option.
(define Sel-op-cbcodelink (lambda (op)
        (if (option? op)
            (list-ref op 2)
            null)))

;Dominio: Option
;Recorrido: FlowCodeLink(positive integer)
;Recursividad: No aplica
;Descripción: Retorna el FlowCodeLink de un option.
(define Sel-op-fwcodelink (lambda (op)
        (if (option? op)
            (list-ref op 3)
            null)))

;Dominio: Option
;Recorrido: List
;Recursividad: No aplica
;Descripción: Retorna las Keywords de un option.
(define Sel-op-keywords (lambda (op)
        (if (option? op)
            (list-ref op 4)
            null)))

;--------------------Modificadores

;Dominio: List of Options(list)
;Recorrido: List of Options(list)
;Recursividad: No aplica
;Descripción: Elimina duplicados en una lista de options a partir de sus ids.
(define dup-list-op (lambda (list-op)
        (define compare-code (lambda (op1 op2)
                (= (Sel-op-code op1) (Sel-op-code op2))))
        (remove-duplicates list-op compare-code)))
#lang Racket

(require "option_21130814_MonjeRojas.rkt")
(require "chathistory_21130814_MonjeRojas.rkt")
(provide (all-defined-out))

;############### TDA User ###############

;REPRESENTACIÓN TDA
;user: name(string) x List of ChatHistory(list)

(define InvalidUser null) ;Se define un usuario no aceptado para retorno
(define NoUser "no-user") ;Se define un usuario para sesión no activa de system

;--------------------Constructor

;Dominio: name(string)
;Recorrido: User
;Recursividad: No aplica
;Descripción: Constructor User a partir de un string.
(define user (lambda (name)
        (if (string? name)
            (list name EmptyList)
            InvalidUser)))

;--------------------Pertenencia

;Dominio: Any
;Recorrido: Boolean
;Recursividad: No aplica
;Descripción: Comprueba que el argumento entregado sea un User.
(define user? (lambda (usr)
        (or (and (list? usr)(= (length usr) 2)(string? (car usr))(all-chathistory? (cadr usr)))
            (equal? NoUser usr))))
            
;Dominio: List
;Recorrido: Boolean
;Recursividad: No aplica
;Descripción: Comprueba que todos los elementos de una lista sean Users.
(define all-user? (lambda (list-usr)
        (andmap user? list-usr)))

;--------------------Selectores

;Dominio: User
;Recorrido: name(string)
;Recursividad: No aplica
;Descripción: Selector de User, retorna su nombre
(define Sel-usr-name (lambda (usr)
        (if (user? usr)
            (list-ref usr 0)
            null)))

;Dominio: User
;Recorrido: UserHistory
;Recursividad: No aplica
;Descripción: Selector de User, retorna sus chathistory.
(define Sel-usr-his (lambda (usr)
        (if (user? usr)
            (list-ref usr 1)
            null)))

;--------------------Modificadores

;Dominio: List of Users (list)
;Recorrido: List of Users (list)
;Recursividad: No aplica
;Descripción: Elimina users duplicados en una lista de users en base al nombre.
(define dup-list-usr (lambda (list-usr)
        (define compare-usr-name (lambda (usr1 usr2)
                (equal? (Sel-usr-name usr1) (Sel-usr-name usr2))))
        (remove-duplicates list-usr compare-usr-name)))

;Dominio: User
;Recorrido: string
;Recursividad: No aplica
;Descripción: Formatea un chathistory juntando los strings y añadiendo saltos de linea, retorna un string
;             listo para display
(define format-sys (lambda (usr)
        (define format-aux (lambda (chathis)
                (string-append (Sel-usr-name usr) ": " (car chathis) "\n"
                (cadr chathis) ": " (caddr chathis) "\n"
                (apply string-append (str-append-cola (cdddr chathis))) "\n\n")))
        (apply string-append (map format-aux (Sel-usr-his usr)))))
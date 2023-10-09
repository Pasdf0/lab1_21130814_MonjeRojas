#lang Racket

(require "option_21130814_MonjeRojas.rkt")
(require "flow_21130814_MonjeRojas.rkt")
(provide (all-defined-out))

;############### TDA Chatbot ###############

;REPRESENTACIÓN TDA
;chatbot: Chatbot-ID(positive integer) x name(string) x welcome-msg(string) x startFlowID(positive integer) x Flows(list)

(define InvalidChatbot null) ;Se define chatbot no aceptado para retorno


;--------------------Pertenencia

;Dominio: Any
;Recorrido: Boolean
;Recursividad: No aplica
;Descripción: Comprueba que el argumento entregado sea un chatbot.
(define chatbot? (lambda (cb)
        (and (list? cb)(= (length cb) 5)(epi? (car cb))(string? (cadr cb))
             (string? (caddr cb))(epi? (cadddr cb))(all-flow? (car (cddddr cb))))))

;Dominio: List
;Recorrido: Boolean
;Recursividad: No aplica
;Descripción: Comprueba que todos los elementos de una lista sean chatbots.
(define all-chatbot? (lambda (list-cb)
        (andmap chatbot? list-cb)))

;--------------------Selectores

;Dominio: Chatbot
;Recorrido: id (positive integer)
;Recursividad: No aplica
;Descripción: Selector de chatbot para su id.
(define Sel-cb-id (lambda (cb)
        (if (chatbot? cb)
            (list-ref cb 0)
            null)))

;Dominio: Chatbot
;Recorrido: name (string)
;Recursividad: No aplica
;Descripción: Selector de chatbot para su nombre.
(define Sel-cb-name (lambda (cb)
        (if (chatbot? cb)
            (list-ref cb 1)
            null)))

;Dominio: Chatbot
;Recorrido: welcome-msg (string)
;Recursividad: No aplica
;Descripción: Selector de chatbot para su mensaje de bienvenida.
(define Sel-cb-welmsg (lambda (cb)
        (if (chatbot? cb)
            (list-ref cb 2)
            null)))

;Dominio: Chatbot
;Recorrido: startfwid (positive integer)
;Recursividad: No aplica
;Descripción: Selector de chatbot para la id de su flow actual.
(define Sel-cb-startfwid (lambda (cb)
        (if (chatbot? cb)
            (list-ref cb 3)
            null)))

;Dominio: Chatbot
;Recorrido: Flows (list)
;Recursividad: No aplica
;Descripción: Selector de chatbot para obtener sus flows.
(define Sel-cb-fw (lambda (cb)
        (if (chatbot? cb)
            (list-ref cb 4)
            null)))

;--------------------Modificadores

;Dominio: List of Chatbots (list)
;Recorrido: List of Chatbots (list)
;Recursividad: No aplica
;Descripción: Elimina chatbot duplicados a partir de las ids.
(define dup-list-cb (lambda (list-cb)
        (define compare-cb-id (lambda (cb1 cb2)
                (= (Sel-cb-id cb1) (Sel-cb-id cb2))))
        (remove-duplicates list-cb compare-cb-id)))

;Dominio: Chatbot x id(positive integer)
;Recorrido: Flow
;Recursividad: No aplica
;Descripción: Busca un flow dentro de un chatbot a partir de su id.
(define find-fw (lambda (cb id)
        (findf (lambda (arg) (= id (car arg))) (Sel-cb-fw cb))))

;Dominio: Chatbot x id(positive integer)
;Recorrido: Chatbot
;Recursividad: No aplica
;Descripción: Cambia el startflowid de un chatbot.
(define change-cb-startfwid (lambda (cb id)
        (if (and (chatbot? cb)(epi? id))
            (cons (Sel-cb-id cb) (cons (Sel-cb-name cb) (cons (Sel-cb-welmsg cb)
            (cons id (cddddr cb)))))
            InvalidChatbot)))

;Dominio: Lista de chatbots (list) x Chatbot x id (positive integer)
;Recorrido: Lista de chatbots (list)
;Recursividad: No aplica
;Descripción: Utiliza change-cb-startfwid para cambiar la startflowid de un chatbot y
;             retornar la lista de chatbots completa, su funcion principal es
;             actualizar un system para las funciones system-talk.
(define change-list-cb-startfwid (lambda (list-cb cb id)
        (if (and (all-chatbot? list-cb)(chatbot? cb)(epi? id))
            (dup-list-cb (cons (change-cb-startfwid cb id) list-cb))
            InvalidChatbot)))

;Dominio: Chatbot x Lista de Flows (list)
;Recorrido: Chatbot
;Recursividad: No aplica
;Descripción: Reemplaza la lista de flows de un chatbot.
(define change-cb-fw (lambda (cb list-fw)
        (define delete-cb-fw (lambda (cb)
                (remove (Sel-cb-fw cb) cb)))
        (if (and (chatbot? cb)(all-flow? list-fw))
            (append (delete-cb-fw cb) (list list-fw))
            InvalidChatbot)))
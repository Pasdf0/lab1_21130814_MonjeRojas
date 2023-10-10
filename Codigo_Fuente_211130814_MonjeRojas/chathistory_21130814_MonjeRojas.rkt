#lang Racket

(require "option_21130814_MonjeRojas.rkt")
(require "flow_21130814_MonjeRojas.rkt")
(require "chatbot_21130814_MonjeRojas.rkt")
(provide (all-defined-out))

;############### TDA Chathistory ###############

;REPRESENTACIÓN TDA
;chathistory: msg(string) x chatbotname(string) x flowmsg(string) x option-msgs(all-string/for each option in flow)

(define InvalidChatHistory null) ;Se define chathistory no aceptado para retorno

;--------------------Constructor

;Dominio: msg(string) x Chatbot x Flow
;Recorrido: ChatHistory
;Recursividad: No aplica
;Descripción: Crea un chathistory a partir del msg de entrada y la información
;             contenida en el chatbot y el flow entregado.
(define chathistory (lambda (msg cb fw)
        (if (and (string? msg)(chatbot? cb)(flow? fw))
            (append (list msg (Sel-cb-name cb) (Sel-fw-msg fw))
            (map Sel-op-msg (Sel-fw-op fw)))
            InvalidChatHistory)))

;--------------------Pertenencia

;Dominio: List
;Recorrido: Boolean
;Recursividad: No aplica
;Descripción: Comprueba que el argumento entregado sea un chathistory
(define chathistory? (lambda (chathis)
        (or (all-strings? chathis)(null? chathis))))

;Dominio: List
;Recorrido: Boolean
;Recursividad: No aplica
;Descripción: Comprueba que cada elemento de una lista sea un chathistory.
(define all-chathistory? (lambda (list-chathis)
        (or (andmap chathistory? list-chathis)(null? list-chathis))))
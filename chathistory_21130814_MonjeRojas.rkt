#lang Racket

(require "option_21130814_MonjeRojas.rkt")
(require "flow_21130814_MonjeRojas.rkt")
(require "chatbot_21130814_MonjeRojas.rkt")
(provide (all-defined-out))

;############### TDA Chathistory ###############

(define InvalidChatHistory null)

;--------------------Constructor

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define chathistory (lambda (msg cb fw)
        (if (and (string? msg)(chatbot? cb)(flow? fw))
            (append (list msg (Sel-cb-name cb) (Sel-fw-msg fw))
            (map Sel-op-msg (Sel-fw-op fw)))
            InvalidChatHistory)))

;--------------------Pertenencia

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define chathistory? (lambda (chathis)
        (or (all-strings? chathis)(null? chathis))))

;Dominio:
;Recorrido:
;Recursividad: 
;Descripción: 
(define all-chathistory? (lambda (list-chathis)
        (or (andmap chathistory? list-chathis)(null? list-chathis))))
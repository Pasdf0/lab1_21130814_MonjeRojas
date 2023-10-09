#lang Racket

(require "option_21130814_MonjeRojas.rkt")
;(require "flow_21130814_MonjeRojas.rkt")
;(require "chatbot_21130814_MonjeRojas.rkt")
;(require "system_21130814_MonjeRojas.rkt")
(require "chathistory_21130814_MonjeRojas.rkt")
(provide (all-defined-out))

(define InvalidUser null)
(define NoUser "no-user")

;--------------------Constructor

(define user (lambda (name)
        (if (string? name)
            (list name EmptyList)
            InvalidUser)))

;--------------------Pertenencia

(define user? (lambda (usr)
        (or (and (list? usr)(= (length usr) 2)(string? (car usr))(all-chathistory? (cadr usr)))
            (equal? NoUser usr))))
            

(define all-user? (lambda (list-usr)
        (andmap user? list-usr)))

;--------------------Selectores

(define Sel-usr-name (lambda (usr)
        (if (user? usr)
            (list-ref usr 0)
            null)))

(define Sel-usr-his (lambda (usr)
        (if (user? usr)
            (list-ref usr 1)
            null)))

;--------------------Modificadores

(define dup-list-usr (lambda (list-usr)
        (define compare-usr-name (lambda (usr1 usr2)
                (equal? (Sel-usr-name usr1) (Sel-usr-name usr2))))
        (remove-duplicates list-usr compare-usr-name)))
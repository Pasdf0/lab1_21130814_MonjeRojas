#lang Racket

(require "option_21130814_MonjeRojas.rkt")
(require "flow_21130814_MonjeRojas.rkt")
;(require "user_21130814_MonjeRojas.rkt")
;(require "chathistory_21130814_MonjeRojas.rkt")
(provide (all-defined-out))

(define InvalidChatbot null)

;--------------------Constructor

(define chatbot (lambda (Chatbot-ID name welcome-msg startFlowID . Flows)
        (if (and (epi? Chatbot-ID)(string? name)(string? welcome-msg)(epi? startFlowID))
            (list Chatbot-ID name welcome-msg startFlowID (if (all-flow? Flows)
                                                              (dup-list-fw Flows) null))
            InvalidChatbot)))

;--------------------Pertenencia

(define chatbot? (lambda (cb)
        (and (list? cb)(= (length cb) 5)(epi? (car cb))(string? (cadr cb))
             (string? (caddr cb))(epi? (cadddr cb))(all-flow? (car (cddddr cb))))))

(define all-chatbot? (lambda (list-cb)
        (andmap chatbot? list-cb)))

;--------------------Selectores

(define Sel-cb-id (lambda (cb)
        (if (chatbot? cb)
            (list-ref cb 0)
            null)))

(define Sel-cb-name (lambda (cb)
        (if (chatbot? cb)
            (list-ref cb 1)
            null)))

(define Sel-cb-welmsg (lambda (cb)
        (if (chatbot? cb)
            (list-ref cb 2)
            null)))

(define Sel-cb-startfwid (lambda (cb)
        (if (chatbot? cb)
            (list-ref cb 3)
            null)))

(define Sel-cb-fw (lambda (cb)
        (if (chatbot? cb)
            (list-ref cb 4)
            null)))

;--------------------Modificadores

(define dup-list-cb (lambda (list-cb)
        (define compare-cb-id (lambda (cb1 cb2)
                (= (Sel-cb-id cb1) (Sel-cb-id cb2))))
        (remove-duplicates list-cb compare-cb-id)))


(define find-fw (lambda (cb id)
        (findf (lambda (arg) (= id (car arg))) (Sel-cb-fw cb))))

(define change-cb-fw (lambda (cb list-fw)
        (define delete-cb-fw (lambda (cb)
                (remove (Sel-cb-fw cb) cb)))
        (if (and (chatbot? cb)(all-flow? list-fw))
            (append (delete-cb-fw cb) list-fw)
            InvalidChatbot)))

(define change-cb-startfwid (lambda (cb id)
        (if (and (chatbot? cb)(epi? id))
            (cons (Sel-cb-id cb) (cons (Sel-cb-name cb) (cons (Sel-cb-welmsg cb)
            (cons id (cddddr cb)))))
            InvalidChatbot)))

(define change-list-cb-startfwid (lambda (list-cb cb id)
        (if (and (all-chatbot? list-cb)(chatbot? cb)(epi? id))
            (dup-list-cb (cons (change-cb-startfwid cb id) list-cb))
            InvalidChatbot)))

(define chatbot-add-flow (lambda (cb fw)
        (define cb-add-aux (lambda (list-fw fw aux)
                (if (null? list-fw)
                    (append aux (list fw))
                    (if (= (Sel-fw-id fw) (Sel-fw-id (car list-fw)))
                        (append aux list-fw)
                        (cb-add-aux (cdr list-fw) fw (append aux (list (car list-fw))))))))    
        (if (and (chatbot? cb)(flow? fw))
            (change-cb-fw cb (cb-add-aux cb fw))
            InvalidChatbot)))
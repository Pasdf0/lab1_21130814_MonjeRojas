#lang Racket

(require "option_21130814_MonjeRojas.rkt")
(require "flow_21130814_MonjeRojas.rkt")
(require "chatbot_21130814_MonjeRojas.rkt")
(require "user_21130814_MonjeRojas.rkt")
(require "chathistory_21130814_MonjeRojas.rkt")
(provide (all-defined-out))

;############### TDA System ###############

(define InvalidSystem null)

;--------------------Constructor

(define system (lambda (name InitialChatbotCodeLink . Chatbots)
        (if (and (string? name)(epi? InitialChatbotCodeLink))
            (list name InitialChatbotCodeLink (if (all-chatbot? Chatbots)
                                                  (dup-list-cb Chatbots) null) NoUser EmptyList)
            InvalidSystem)))

;--------------------Pertenencia

(define system? (lambda (sys)
        (and (list? sys)(= (length sys) 5)(string? (car sys))
             (epi? (cadr sys))(all-chatbot? (caddr sys))(user? (cadddr sys))
             (all-user? (car (cddddr sys))))))

;--------------------Selectores

(define Sel-sys-name (lambda (sys)
        (if (system? sys)
            (list-ref sys 0)
            null)))

(define Sel-sys-startcbcode (lambda (sys)
        (if (system? sys)
            (list-ref sys 1)
            null)))

(define Sel-sys-cb (lambda (sys)
        (if (system? sys)
            (list-ref sys 2)
            null)))

(define Sel-sys-usr (lambda (sys)
        (if (system? sys)
            (list-ref sys 3)
            null)))

(define Sel-sys-list-usr (lambda (sys)
        (if (system? sys)
            (list-ref sys 4)
            null)))


;--------------------Modificadores

  ;System add chatbot
(define change-sys-cb (lambda (sys list-cb)
        (define delete-sys-cb (lambda (sys)
                (remove (Sel-sys-cb sys) sys)))
        (if (and (system? sys)(all-chatbot? list-cb))
            (cons (Sel-sys-name sys) (cons (Sel-sys-startcbcode sys) (cons list-cb (cdddr sys))))
            InvalidSystem)))

(define system-add-chatbot (lambda (sys cb)
        (if (and (system? sys)(chatbot? cb))
            (change-sys-cb sys (dup-list-cb (append (Sel-sys-cb sys) (list cb))))
            InvalidSystem)))

  ;System add User
(define change-sys-usr-list (lambda (sys list-usr)
        (define delete-sys-list-usr (lambda (usr)
                (remove (Sel-sys-list-usr sys) sys)))
        (if (and (system? sys)(all-user? list-usr))
            (append (delete-sys-list-usr sys) (list list-usr))
            InvalidUser)))

(define system-add-user (lambda (sys usr)
        (if (and (system? sys)(string? usr))
            (change-sys-usr-list sys (dup-list-usr (append (Sel-sys-list-usr sys) (list (user usr)))))
            InvalidUser)))

  ;System-login-logout
(define usr-is-in (lambda (usr list-usr)
        (list? (findf (lambda (arg) (equal? (Sel-usr-name usr) (Sel-usr-name arg))) list-usr))))

(define change-sys-usr (lambda (sys usr)
        (if (and (system? sys)(user? usr))
            (cons (Sel-sys-name sys) (cons (Sel-sys-startcbcode sys)
            (cons (Sel-sys-cb sys) (cons usr (cddddr sys)))))
            InvalidSystem)))

(define find-usr (lambda (usr list-usr)
        (if (and (user? usr) (all-user? list-usr))
            (findf (lambda (arg) (equal? (Sel-usr-name usr) (Sel-usr-name arg))) list-usr)
            InvalidUser)))

(define refresh-sys-usr (lambda (sys)
       (define change-list-usr (lambda (list-usr usr)
               (if (and (all-user? list-usr)(user? usr))
                   (dup-list-usr (cons usr list-usr))
                   InvalidUser)))
       (append (remove (Sel-sys-list-usr sys) sys) (list (change-list-usr (Sel-sys-list-usr sys) (Sel-sys-usr sys))))))
            

  ;System Login
(define system-login (lambda (sys usr)
        (if (and (system? sys)(string? usr))
            (if (and (equal? NoUser (Sel-sys-usr sys))(usr-is-in (user usr) (Sel-sys-list-usr sys)))
                (change-sys-usr sys (user usr))
                sys)
            InvalidSystem)))

  ;System Logout
(define system-logout (lambda (sys)
        (if (system? sys)
            (change-sys-usr (refresh-sys-usr sys) NoUser)
            InvalidSystem)))

  ;System talk

(define find-cb (lambda (sys id)
        (findf (lambda (arg) (= id (car arg))) (Sel-sys-cb sys))))

(define msg-in-op? (lambda (op msg)
        (if (string? (findf (lambda (arg) (equal? (string-downcase msg) arg)) (Sel-op-keywords op)))
            #t
            (if (not (boolean? (string->number msg)))
                (= (string->number msg) (Sel-op-code op))
                #f))))

(define msg-in-fw? (lambda (fw msg)
        (list? (member #t (map (lambda (arg) (msg-in-op? arg msg)) (Sel-fw-op fw))))))

(define msg-in-sys? (lambda (sys msg)
        (msg-in-fw? (get-current-fw sys) msg)))

(define get-current-fw (lambda (sys)
        (find-fw (find-cb sys (Sel-sys-startcbcode sys))
                 (Sel-cb-startfwid (find-cb sys (Sel-sys-startcbcode sys))))))

(define usr-add-log (lambda (usr chathis)
        (if (and (user? usr) (chathistory? chathis))
            (cons (Sel-usr-name usr) (cons (append (Sel-usr-his usr) (list chathis)) null))
            InvalidUser)))

(define sys-update-usr (lambda (sys msg)
        (cons (Sel-sys-name sys) (cons (Sel-sys-startcbcode sys) (cons (Sel-sys-cb sys)
        (cons (usr-add-log (Sel-sys-usr sys) (chathistory msg (find-cb sys (Sel-sys-startcbcode sys))
        (get-current-fw sys))) (cddddr sys)))))))

(define sys-update-cbcode (lambda (sys newcbcode)
        (cons (Sel-sys-name sys) (cons newcbcode (cddr sys)))))

(define sys-update-cb-fwid (lambda (sys newfwid)
        (cons (Sel-sys-name sys) (cons (Sel-sys-startcbcode sys) (cons
        (change-list-cb-startfwid (Sel-sys-cb sys) (find-cb sys (Sel-sys-startcbcode sys)) newfwid)
        (cdddr sys))))))
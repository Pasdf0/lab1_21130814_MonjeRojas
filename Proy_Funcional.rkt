#lang Racket
(provide (all-defined-out))

(define InvalidOption null)
(define InvalidFlow null)
(define InvalidChatbot null)
(define InvalidSystem null)
(define InvalidUserHistory null)
(define InvalidUser null)
(define NoUser "no-user")
(define EmptyList (list ))

(define epi? (lambda (n) (or (exact-positive-integer? n) (= n 0))))
;Se redefine la función para verificar ids en caso de requerir cambios

(define list-dup? (lambda (lista)
        (not (false? (check-duplicates lista)))))

(define all-lowercase (lambda (lista)
        (map string-downcase lista)))

(define (myRandom Xn)
        (modulo (+ (* 1103515245 Xn) 12345) 2147483648)
)

;----------------Constructores TDA

;Option
(define option (lambda (Code Message ChatbotCodeLink InitialFlowCodeLink . Keywords)
        (if (and (epi? Code)(string? Message)(epi? ChatbotCodeLink)(epi? InitialFlowCodeLink))
            (list Code Message ChatbotCodeLink InitialFlowCodeLink (if (all-strings? Keywords)
                                                                       (all-lowercase Keywords) null))
            InvalidOption)))

;Flow
(define flow (lambda (Flow-ID msg . Options)
        (if (and (epi? Flow-ID)(string? msg))
            (list Flow-ID msg (if (all-option? Options)
                                   (dup-list-op Options) null))
            InvalidFlow)))

;Chatbot
(define chatbot (lambda (Chatbot-ID name welcome-msg startFlowID . Flows)
        (if (and (epi? Chatbot-ID)(string? name)(string? welcome-msg)(epi? startFlowID))
            (list Chatbot-ID name welcome-msg startFlowID (if (all-flow? Flows)
                                                              (dup-list-fw Flows) null))
            InvalidChatbot)))

;System
(define system (lambda (name InitialChatbotCodeLink . Chatbots)
        (if (and (string? name)(epi? InitialChatbotCodeLink))
            (list name InitialChatbotCodeLink (if (all-chatbot? Chatbots)
                                                  (dup-list-cb Chatbots) null) NoUser EmptyList)
            InvalidSystem)))

;UserHistory
(define userhistory EmptyList)

;User
(define user (lambda (name)
        (if (string? name)
            (list name EmptyList)
            InvalidUser)))

;---------------Funciones de pertenencia TDA

;Option
(define option? (lambda (op)                  
        (and (list? op)(= (length op) 5)(epi? (car op))(string? (cadr op))(epi? (caddr op))
             (epi? (cadddr op))(all-strings? (car (cddddr op))))))

(define all-option? (lambda (list-op)
        (if (null? list-op)
            #t
            (if (option? (car list-op))
                (all-option? (cdr list-op))
                #f))))

;Flow
(define flow? (lambda (fw)
        (and (list? fw)(= (length fw) 3)(epi? (car fw))
             (string? (cadr fw))(all-option? (caddr fw)))))

(define all-flow? (lambda (list-fw)
        (if (null? list-fw)
            #t
            (if (flow? (car list-fw))
                (all-flow? (cdr list-fw))
                #f))))

;Chatbot
(define chatbot? (lambda (cb)
        (and (list? cb)(= (length cb) 5)(epi? (car cb))(string? (cadr cb))
             (string? (caddr cb))(epi? (cadddr cb))(all-flow? (car (cddddr cb))))))

(define all-chatbot? (lambda (list-cb)
        (if (null? list-cb)
            #t
            (if (chatbot? (car list-cb))
                (all-chatbot? (cdr list-cb))
                #f))))

;System
(define system? (lambda (sys)
        (and (list? sys)(= (length sys) 5)(string? (car sys))
             (epi? (cadr sys))(all-chatbot? (caddr sys))(user? (cadddr sys))
             (all-user? (car (cddddr sys))))))

;UserHistory
(define userhistory? (lambda (usrhys)
        (or (and (list? usrhys)(all-strings? usrhys))(null? usrhys))))

;User
(define user? (lambda (usr)
        (or (and (list? usr)(= (length usr) 2)(string? (car usr))(userhistory? (cadr usr)))
            (equal? NoUser usr))))
            

(define all-user? (lambda (list-usr)
        (if (null? list-usr)
            #t
            (if (user? (car list-usr))
                (all-user? (cdr list-usr))
                #f))))
;Adicionales

(define all-strings? (lambda (lista)
        (if (null? lista)
            #t
            (if (string? (car lista))
                (all-strings? (cdr lista))
                #f))))

;---------------Selectores TDA

;Option
(define Sel-op-code (lambda (op)
        (if (option? op)
            (list-ref op 0)
            null)))

(define Sel-op-msg (lambda (op)
        (if (option? op)
            (list-ref op 1)
            null)))

(define Sel-op-cbcodelink (lambda (op)
        (if (option? op)
            (list-ref op 2)
            null)))

(define Sel-op-fwcodelink (lambda (op)
        (if (option? op)
            (list-ref op 3)
            null)))

(define Sel-op-keywords (lambda (op)
        (if (option? op)
            (list-ref op 4)
            null)))

;Flow
(define Sel-fw-id (lambda (fw)
        (if (flow? fw)
            (list-ref fw 0)
            null)))

(define Sel-fw-msg (lambda (fw)
        (if (flow? fw)
            (list-ref fw 1)
            null)))

(define Sel-fw-op (lambda (fw)
        (if (flow? fw)
            (list-ref fw 2)
            null)))

;Chatbot
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

;System
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

;User
(define Sel-usr-name (lambda (usr)
        (if (user? usr)
            (list-ref usr 0)
            null)))

(define Sel-usr-his (lambda (usr)
        (if (user? usr)
            (list-ref usr 1)
            null)))

;--------------------Duplicidad

(define dup-list-op (lambda (list-op)
        (define compare-code (lambda (op1 op2)
                (= (Sel-op-code op1) (Sel-op-code op2))))
        (remove-duplicates list-op compare-code)))

(define dup-list-fw (lambda (list-fw)
        (define compare-fw-id (lambda (fw1 fw2)
                (= (Sel-fw-id fw1) (Sel-fw-id fw2))))
        (remove-duplicates list-fw compare-fw-id)))

(define dup-list-cb (lambda (list-cb)
        (define compare-cb-id (lambda (cb1 cb2)
                (= (Sel-cb-id cb1) (Sel-cb-id cb2))))
        (remove-duplicates list-cb compare-cb-id)))

(define dup-list-usr (lambda (list-usr)
        (define compare-usr-name (lambda (usr1 usr2)
                (equal? (Sel-usr-name usr1) (Sel-usr-name usr2))))
        (remove-duplicates list-usr compare-usr-name)))


;-------------------Modificadores

;Option
(define change-op-code (lambda (op n)
        (if (and (option? op) (integer? n))
            (option n (Sel-op-msg op) (Sel-op-cbcodelink op)
                    (Sel-op-fwcodelink op) (Sel-op-keywords op))
            InvalidOption)))

;Flow
(define change-fw-op (lambda (fw list-op)
        (define delete-fw-op (lambda (fw)
                (remove (Sel-fw-op fw) fw)))
        (if (and (flow? fw)(all-option? list-op))
            (append (delete-fw-op fw) list-op)
            InvalidFlow)))

(define flow-add-option (lambda (fw op)
        (if (and (flow? fw)(option? op))
            (change-fw-op fw (dup-list-op (append (Sel-fw-op fw) (list op))))
            InvalidFlow)))

;Chatbot
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
            (dup-list-cb (cons (change-cb-startfwid cb) list-cb))
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

;System

  ;System add Chatbot
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

  ;System talk rec
;(define find-op (lambda (fw code)
;        (findf (lambda (arg) (= code (car arg))) (Sel-fw-op fw))))
#|
(define find-fw-rec (lambda (cb id)
        (define fw-rec-aux (lambda (list-fw id)
                (if (null? list-fw)
                    InvalidFlow
                    (if (equal? id (caar list-fw))
                        (car list-fw)
                        (fw-rec-aux (cdr list-fw) id)))))
        (fw-rec-aux (Sel-cb-fw cb) id)))
|#
(define find-fw (lambda (cb id)
        (findf (lambda (arg) (= id (car arg))) (Sel-cb-fw cb))))
#|
(define find-cb-rec (lambda (sys id)
        (define cb-rec-aux (lambda (list-cb id)
                (if (null? list-cb)
                    InvalidChatbot
                    (if (equal? id (caar list-cb))
                        (car list-cb)
                        (cb-rec-aux (cdr list-cb) id)))))
        (cb-rec-aux (Sel-sys-cb sys) id)))
|#
(define find-cb (lambda (sys id)
        (findf (lambda (arg) (= id (car arg))) (Sel-sys-cb sys))))

(define msg-in-op? (lambda (op msg)
        (if (string? (findf (lambda (arg) (equal? (string-downcase msg) arg)) (Sel-op-keywords op)))
            #t
            (if (not (boolean? (string->number msg)))
                (= (string->number msg) (Sel-op-code op))
                #f))))

(define msg-in-fw? (lambda (fw msg)
        (list? (member #t
               (map (lambda (arg) (msg-in-op? arg msg)) (Sel-fw-op fw))))))

(define msg-in-fw-norec (lambda (fw msg)
        (findf (lambda (arg) (msg-in-op? arg msg)) (Sel-fw-op fw))))
         
(define msg-in-fw-rec (lambda (fw msg)
        (define msg-fw-aux (lambda (list-op msg)
                (if (null? list-op)
                    #f
                    (if (msg-in-op? (car list-op) msg)
                        (car list-op)
                        (msg-fw-aux (cdr list-op) msg)))))
        (msg-fw-aux (Sel-fw-op fw) msg)))

(define get-current-fw (lambda (sys)
        (find-fw (find-cb sys (Sel-sys-startcbcode sys))
                 (Sel-cb-startfwid (find-cb sys (Sel-sys-startcbcode sys))))))

(define msg-in-sys? (lambda (sys msg)
        (msg-in-fw? (get-current-fw sys) msg)))

(define msg-in-sys-rec (lambda (sys msg)
        (msg-in-fw-rec (get-current-fw sys) msg)))

(define msg-in-sys-norec (lambda (sys msg)
        (msg-in-fw-norec (get-current-fw sys) msg)))

(define usr-add-log (lambda (usr msg)
        (if (and (user? usr) (string? msg))
            (cons (Sel-usr-name usr) (cons (append (Sel-usr-his usr) (list msg)) null))
            InvalidUser)))

(define sys-update-aux (lambda (sys newcbcode msg)
        (cons (Sel-sys-name sys) (cons newcbcode (cons (Sel-sys-cb sys)
        (cons (usr-add-log (Sel-sys-usr sys) msg) (cddddr sys)))))))

(define sys-cb-update (lambda (sys newfwid)
        (cons (Sel-sys-name sys) (cons (Sel-sys-startcbcode sys) (cons
        (change-list-cb-startfwid (Sel-sys-cb sys) (find-cb sys (Sel-sys-startcbcode sys)) newfwid)
        (cdddr sys))))))

(define sys-update-rec (lambda (sys msg)
        (sys-cb-update (sys-update-aux sys (Sel-op-cbcodelink (msg-in-sys-rec sys msg)) msg)
                       (Sel-op-fwcodelink (msg-in-sys-rec sys msg)))))

(define sys-update-norec (lambda (sys msg)
        (sys-cb-update (sys-update-aux sys (Sel-op-cbcodelink (msg-in-sys-rec sys msg)) msg)
                       (Sel-op-fwcodelink (msg-in-sys-norec sys msg)))))
                                                               
(define system-talk-rec (lambda (sys msg)
        (if (and (system? sys)(string? msg)(not (equal? (Sel-sys-usr sys) NoUser)))
            (if (msg-in-sys? sys msg)
                (sys-update-rec sys msg)
                (sys-update-aux sys (Sel-sys-startcbcode sys) msg))
            InvalidSystem)))

(define system-talk-norec (lambda (sys msg)
        (if (and (system? sys)(string? msg)(not (equal? (Sel-sys-usr sys) NoUser)))
            (if (msg-in-sys? sys msg)
                (sys-update-norec sys msg)
                (sys-update-aux sys (Sel-sys-startcbcode sys) msg))
            InvalidSystem)))
            


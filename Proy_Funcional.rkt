#lang Racket
(provide (all-defined-out))

(define InvalidOption null)
(define InvalidFlow null)
(define InvalidChatbot null)
(define InvalidSystem null)

(define epi? (lambda (n) (or (exact-positive-integer? n) (= n 0))))
;Se redefine la función para verificar ids en caso de requerir cambios

(define list-dup? (lambda (lista)
        (not (false? (check-duplicates lista)))))

(define (myRandom Xn)
        (modulo (+ (* 1103515245 Xn) 12345) 2147483648)
)

;----------------Constructores TDA

;Option
(define option (lambda (Code Message ChatbotCodeLink InitialFlowCodeLink . Keywords)
        (if (and (epi? Code)(string? Message)(epi? ChatbotCodeLink)(epi? InitialFlowCodeLink))
            (list Code Message ChatbotCodeLink InitialFlowCodeLink (if (all-strings? Keywords)
                                                                       Keywords null))
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
                                                  (dup-list-cb Chatbots) null))
            InvalidSystem)))

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
            (if (chatbot? (cdr list-cb))
                (all-chatbot? (cdr list-cb))
                #f))))

;System
(define system? (lambda (sys)
        (and (list? sys)(= (length sys) 3)(string? (car sys))
             (epi? (cadr sys))(all-chatbot? (caddr sys)))))

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

(define Sel-op-initialfwcodelink (lambda (op)
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

;------------------Duplicidad
;Sin Utilidad por ahora
;(define flow-dup-op? (lambda (fw)
;        (if (flow? fw)
;            (list-dup? (map Sel-op-code (Sel-fw-op fw))) 
;            null)))

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


;-------------------Modificadores

;Option
(define change-op-code (lambda (op n)
        (if (and (option? op) (integer? n))
            (option n (Sel-op-msg op) (Sel-op-cbcodelink op)
                    (Sel-op-initialfwcodelink op) (Sel-op-keywords op))
            InvalidOption)))

;Flow
(define change-fw-op (lambda (fw list-op)
        (define delete-fw-op (lambda (fw)
                (if (flow? fw)
                    (remove (Sel-fw-op fw) fw)
                    InvalidFlow)))
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
                (if (chatbot? cb)
                    (remove (Sel-cb-fw cb) cb)
                    InvalidChatbot)))
        (if (and (chatbot? cb)(all-flow? list-fw))
            (append (delete-cb-fw cb) list-fw)
            InvalidChatbot)))

(define chatbot-add-flow (lambda (cb fw)
        (if (and (chatbot? cb)(flow? fw))
            (change-cb-fw cb (dup-list-fw (append (Sel-cb-fw cb) (list fw))))
            InvalidChatbot)))


;-----------------------------------------------------------

;testing
(define op1 (option 1 "msg1" 1 1 "Key1-1" "Key1-2" "Key1-3"))
(define op2 (option 2 "msg2" 2 2 "Key2-1" "Key2-2" "Key2-3"))
(define op3 (option 3 "msg3" 3 3 "Key3-1" 25 "Key3-3"))
(define op4 (option 3 "msg4" 4 4 "Key4-1" "Key4-2" "Key4-3"))
(define fw1 (flow 1 "TestFlow1" op1 op2 op3))
;(change-op-code op3 4)
(define n1 (myRandom 1))
(define n2 (myRandom n1))
;(dup-list-op (Sel-fw-op fw1))
(define test (dup-list-op (append (Sel-fw-op fw1) (list op4))))
;(flow-add-option fw1 op4)



#lang Racket

(define InvalidOption null)
(define InvalidFlow null)
(define InvalidChatbot null)

(define list-dup? (lambda (lista)
        (not (false? (check-duplicates lista)))))

;----------------Constructores TDA

;Option
(define option (lambda (Code Message ChatbotCodeLink InitialFlowCodeLink . Keywords)
        (if (and (exact-positive-integer? Code)(string? Message)(exact-positive-integer? ChatbotCodeLink)
                 (exact-positive-integer? InitialFlowCodeLink))
            (list Code Message ChatbotCodeLink InitialFlowCodeLink (if (all-strings? Keywords)
                                                                       Keywords null))
            InvalidOption)))

;Flow
(define flow (lambda (Flow-ID msg . Options)
        (if (and (exact-positive-integer? Flow-ID)(string? msg))
            (list Flow-ID msg (if (all-option? Options)
                                   Options null))
            InvalidFlow)))

;Chatbot
(define chatbot (lambda (Chatbot-ID name welcome-msg startFlowID . Flows)
        (if (and (exact-positive-integer? Chatbot-ID)(string? name)
                 (string? welcome-msg)(exact-positive-integer? startFlowID))
            (list Chatbot-ID name welcome-msg startFlowID (if (all-flow? Flows)
                                                              Flows null))
            InvalidChatbot)))

;---------------Funciones de pertenencia TDA

;Option
(define option? (lambda (op)                  
        (and (list? op)(= (length op) 5)(exact-positive-integer? (car op))(string? (cadr op))
             (exact-positive-integer? (caddr op))(exact-positive-integer? (cadddr op))
             (all-strings? (car (cddddr op))))))

(define all-option? (lambda (lista)
        (if (null? lista)
            #t
            (if (option? (car lista))
                (all-option? (cdr lista))
                #f))))

;Flow
(define flow? (lambda (fl)
        (and (list? fl)(= (length fl) 3)(exact-positive-integer? (car fl))
             (string? (cadr fl))(all-option? (caddr fl)))))

(define all-flow? (lambda (lista)
        (if (null? lista)
            #t
            (if (flow? (car lista))
                (all-flow? (cdr lista))
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
(define Selector-op-code (lambda (op)
        (if (option? op)
            (list-ref op 0)
            null)))

(define Selector-op-msg (lambda (op)
        (if (option? op)
            (list-ref op 1)
            null)))

(define Selector-op-chatbotcodelink (lambda (op)
        (if (option? op)
            (list-ref op 2)
            null)))

(define Selector-op-initialflowcodelink (lambda (op)
        (if (option? op)
            (list-ref op 3)
            null)))

(define Selector-op-keywords (lambda (op)
        (if (option? op)
            (list-ref op 4)
            null)))

;Flow
(define Selector-fw-ID (lambda (fw)
        (if (flow? fw)
            (list-ref fw 0)
            null)))

(define Selector-fw-msg (lambda (fw)
        (if (flow? fw)
            (list-ref fw 1)
            null)))

(define Selector-fw-op (lambda (fw)
        (if (flow? fw)
            (list-ref fw 2)
            null)))

;------------------Duplicidad
(define flow-dup-op? (lambda (fw)
        (if (flow? fw)
            (list-dup? (map Selector-op-code (Selector-fw-op fw))) 
            null)))

;(define Dup-flow-op (lambda (fw)
;        (if (flow-op-codes fw)

;Option
;(define Dup-list-op (lambda (lista)
;        (define Dup-op-aux (lambda (L aux n)
;                (for ())))))

;Flow
;(define Dup-flow (fw))

;-----------------------------------------------------------

;testing
(define op1 (option 1 "msg1" 1 1 "Key1-1" "Key1-2" "Key1-3"))
(define op2 (option 2 "msg2" 2 2 "Key2-1" "Key2-2" "Key2-3"))
(define op3 (option 3 "msg3" 3 3 "Key3-1" 25 "Key3-3"))
(define op4 (option 3 "msg4" 4 4 "Key4-1" "Key4-2" "Key4-3"))
(define fw1 (flow 1 "TestFlow1" op1 op2 op3 op4))
(flow-dup-op? fw1)
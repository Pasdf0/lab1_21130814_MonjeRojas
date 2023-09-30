#lang Racket

(define InvalidOption null)
(define InvalidFlow null)

;----------------Constructores TDA

;Option
(define option (lambda (Code Message ChatbotCodeLink InitialFlowCodeLink . Keywords)
        (if (and (exact-positive-integer? Code)(string? Message)(exact-positive-integer? ChatbotCodeLink)
                 (exact-positive-integer? InitialFlowCodeLink))
            (list Code Message ChatbotCodeLink InitialFlowCodeLink
                  (if (all-strings? Keywords)
                          Keywords
                          null))
            InvalidOption)))

;Flow
(define flow (lambda (Flow-ID name-msg . Options)
        (if (and (exact-positive-integer? Flow-ID)(string? name-msg))
            (list Flow-ID name-msg (if (all-options? Options)
                                       Options
                                       null))
            InvalidFlow)))


;---------------Funciones de pertenencia TDA

;Option
(define option? (lambda (op)                  
        (and (list? op)(= (length op) 5)(exact-positive-integer? (car op))(string? (cadr op))
             (exact-positive-integer? (caddr op))(exact-positive-integer? (cadddr op))
             (all-strings? (car (cddddr op))))))

;Flow
(define flow? (lambda (fl)
        (and (list? fl)(= (length fl) 3)(exact-positive-integer? (car fl))
             (string? (cadr fl))(all-options? (caddr fl)))))

;Adicionales

(define all-strings? (lambda (lista)
        (if (null? lista)
            #t
            (if (string? (car lista))
                (all-strings? (cdr lista))
                #f))))

(define all-options? (lambda (lista)
        (if (null? lista)
            #t
            (if (option? (car lista))
                (all-options? (cdr lista))
                #f))))

;---------------Selectores TDA

;Option
(define Selector-Op-Code (lambda (op)
        (if (option? op)
            (list-ref op 0)
            null)))

(define Selector-Op-Message (lambda (op)
        (if (option? op)
            (list-ref op 1)
            null)))

(define Selector-Op-ChatbotCodeLink (lambda (op)
        (if (option? op)
            (list-ref op 2)
            null)))

(define Selector-Op-InitialFlowCodeLink (lambda (op)
        (if (option? op)
            (list-ref op 3)
            null)))

(define Selector-Op-Keywords (lambda (op)
        (if (option? op)
            (list-ref op 4)
            null)))

;Flow

;------------------Modificadores de Duplicidad

;-----------------------------------------------------------

;testing
(define op1 (option 1 "msg1" 1 1 "Key1-1" "Key1-2" "Key1-3"))
(define op2 (option 2 "msg2" 2 2 "Key2-1" "Key2-2" "Key2-3"))
(define op3 (option 3 "msg3" 3 3 "Key3-1" 25 "Key3-3"))
(define op4 (option 3 "msg4" 4 4 "Key4-1" "Key4-2" "Key4-3"))
(define fw1 (flow 1 "TestFlow1" op1 op2 op3 op4))
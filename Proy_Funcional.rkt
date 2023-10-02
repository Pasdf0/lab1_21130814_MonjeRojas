#lang Racket

(define InvalidOption null)
(define InvalidFlow null)
(define InvalidChatbot null)

(define list-dup? (lambda (lista)
        (not (false? (check-duplicates lista)))))

(define (myRandom Xn)
        (modulo (+ (* 1103515245 Xn) 12345) 2147483648)
)

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
                                   (dup-list-op Options) null))
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
             (or (all-strings? (car (cddddr op)))(null? (car (cddddr op)))))))

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




;-------------------Modificadores

;Option
(define change-op-code (lambda (op n)
        (if (and (option? op) (integer? n))
            (option n (Sel-op-msg op) (Sel-op-cbcodelink op)
                    (Sel-op-initialfwcodelink op) (Sel-op-keywords op))
            InvalidOption)))

;Flow
(define delete-fw-op (lambda (fw)
        (if (flow? fw)
            (remove (Sel-fw-op fw) fw)
            InvalidFlow)))

(define change-fw-op (lambda (fw list-op)
        (if (and (flow? fw)(all-option? list-op))
            (append (delete-fw-op fw) list-op)
            InvalidFlow)))

(define flow-add-option (lambda (fw op)
        (if (and (flow? fw)(option? op))
            (change-fw-op fw (dup-list-op (append (Sel-fw-op fw) (list op))))
            InvalidFlow)))

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
(flow-add-option fw1 op4)

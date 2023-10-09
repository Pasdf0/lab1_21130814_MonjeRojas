#lang Racket

(require "option_21130814_MonjeRojas.rkt")
;(require "user_21130814_MonjeRojas.rkt")
;(require "chathistory_21130814_MonjeRojas.rkt")
(provide (all-defined-out))

(define InvalidFlow null)

;--------------------Constructor

(define flow (lambda (Flow-ID msg . Options)
        (if (and (epi? Flow-ID)(string? msg))
            (list Flow-ID msg (if (all-option? Options)
                                   (dup-list-op Options) null))
            InvalidFlow)))

;--------------------Pertenencia

(define flow? (lambda (fw)
        (and (list? fw)(= (length fw) 3)(epi? (car fw))
             (string? (cadr fw))(all-option? (caddr fw)))))

(define all-flow? (lambda (list-fw)
        (andmap flow? list-fw)))

;--------------------Selectores

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

;--------------------Modificadores

(define dup-list-fw (lambda (list-fw)
        (define compare-fw-id (lambda (fw1 fw2)
                (= (Sel-fw-id fw1) (Sel-fw-id fw2))))
        (remove-duplicates list-fw compare-fw-id)))


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
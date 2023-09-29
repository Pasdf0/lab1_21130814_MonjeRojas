#lang Racket

(define SelectorLista (lambda (n L) (if (= n 0)
                                        (car L)
                                        (SelectorLista (- n 1) (cdr L)))))

(define InvalidOption null)

(define all-strings? (lambda (lista)
        (if (null? lista)
            #t
            (if (string? (car lista))
                (all-strings? (cdr lista))
                #f))))
            

;----------------Constructores TDA

;Option
(define Option (lambda (Code Message ChatbotCodeLink InitialFlowCodeLink . Keywords)
        (if (and (integer? Code)(string? Message)(integer? ChatbotCodeLink)
                 (integer? InitialFlowCodeLink))
            (list Code Message ChatbotCodeLink InitialFlowCodeLink
                  (if (and (not (null? Keywords)) (all-strings? Keywords))
                          Keywords
                          null))
            InvalidOption)))

;testing
(display (Option 1 "Test" 1 1 "test2" "test3" "test4"))
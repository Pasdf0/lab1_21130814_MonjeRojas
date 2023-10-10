#lang Racket

(require "option_21130814_MonjeRojas.rkt")
(require "flow_21130814_MonjeRojas.rkt")
(require "chatbot_21130814_MonjeRojas.rkt")
(require "system_21130814_MonjeRojas.rkt")
(require "user_21130814_MonjeRojas.rkt")
(require "chathistory_21130814_MonjeRojas.rkt")
(provide (all-defined-out))

;############### Main ###############

;No implementada
;(define (myRandom Xn)
;        (modulo (+ (* 1103515245 Xn) 12345) 2147483648)
;) 


;        -------------------------------------------------------------
;         ################ Requerimientos Funcionales ################
;        -------------------------------------------------------------

;---------------------------------------------
; Requerimiento Funcional 2 Constructor Option
;---------------------------------------------

;Dominio: Code(positive integer) x Message(string) x ChatbotCodeLink(positive integer) x
;         InitialFlowCodeLink(positive integer) x Keywords(list)
;Recorrido: Option
;Recursividad: No aplica
;Descripción: Crea un option a partir de las entradas, verificando que estas sean del tipo requerido.
(define option (lambda (Code Message ChatbotCodeLink InitialFlowCodeLink . Keywords)
        (if (and (epi? Code)(string? Message)(epi? ChatbotCodeLink)(epi? InitialFlowCodeLink))
            (list Code Message ChatbotCodeLink InitialFlowCodeLink (if (all-strings? Keywords)
                                                                       (all-lowercase Keywords) null))
            InvalidOption)))

;-------------------------------------------
; Requerimiento Funcional 3 Constructor Flow
;-------------------------------------------

;Dominio: Flow-ID(positive integer)x msg(string) x Options (List of Options)
;Recorrido: Flow
;Recursividad: No aplica
;Descripción: Construye un flow tomando las entradas, verificando que sus tipos
;             de datos corresponden y agregandolos a una lista.
(define flow (lambda (Flow-ID msg . Options)
        (if (and (epi? Flow-ID)(string? msg))
            (list Flow-ID msg (if (all-option? Options)
                                   (dup-list-op Options) null))
            InvalidFlow)))

;------------------------------------------
; Requerimiento Funcional 4 Flow add Option
;------------------------------------------

;Dominio: Flow x Option
;Recorrido: Flow
;Recursividad: No aplica
;Descripción: Agrega un option a un flow, es eliminado en caso de duplicidad.
(define flow-add-option (lambda (fw op)
        (if (and (flow? fw)(option? op))
            (change-fw-op fw (dup-list-op (append (Sel-fw-op fw) (list op))))
            InvalidFlow)))

;----------------------------------------------
; Requerimiento Funcional 5 Constructor Chatbot
;----------------------------------------------

;Dominio: Chatbot-ID(positive integer) x name(string) x welcome-msg(string) x startFlowID(positive integer) x Flows(List of flows)
;Recorrido: Chatbot
;Recursividad: No aplica
;Descripción: Constructor de chatbot, comprueba que los tipos de datos de las entradas correspondan con lo que
;             se requiere, si lo hacen, se crea un chatbot con estas.
(define chatbot (lambda (Chatbot-ID name welcome-msg startFlowID . Flows)
        (if (and (epi? Chatbot-ID)(string? name)(string? welcome-msg)(epi? startFlowID))
            (list Chatbot-ID name welcome-msg startFlowID (if (all-flow? Flows)
                                                              (dup-list-fw Flows) null))
            InvalidChatbot)))

;-------------------------------------------
; Requerimiento Funcional 6 Chatbot add flow
;-------------------------------------------

;Dominio: Chatbot x Flow
;Recorrido: Chatbot
;Recursividad: Cola
;Descripción: Usa cb-add-aux para añadir un flow a la lista de flows usando recursion de cola
;             luego reemplaza la lista de flows obtenida en el chatbot indicado.
(define chatbot-add-flow (lambda (cb fw)
        (define cb-add-aux (lambda (list-fw fw aux)
                (if (null? list-fw)
                    (append aux (list fw))
                    (if (= (Sel-fw-id fw) (Sel-fw-id (car list-fw)))
                        (append aux list-fw)
                        (cb-add-aux (cdr list-fw) fw (append aux (list (car list-fw))))))))    
        (if (and (chatbot? cb)(flow? fw))
            (change-cb-fw cb (cb-add-aux (Sel-cb-fw cb) fw (list )))
            InvalidChatbot)))

;---------------------------------------------
; Requerimiento Funcional 7 Constructor System
;---------------------------------------------

;Dominio: name(string) x InitialChatbotCodeLink(positive integer) x Chatbots(list)
;Recorrido: System
;Recursividad: No aplica
;Descripción: Crea un system verificando que los argumentos de entrada sean los requeridos.
(define system (lambda (name InitialChatbotCodeLink . Chatbots)
        (if (and (string? name)(epi? InitialChatbotCodeLink))
            (list name InitialChatbotCodeLink (if (all-chatbot? Chatbots)
                                                  (dup-list-cb Chatbots) null) NoUser EmptyList)
            InvalidSystem)))

;---------------------------------------------
; Requerimiento Funcional 8 System add chatbot
;---------------------------------------------

;Dominio: System x Chatbot
;Recorrido: System
;Recursividad: No aplica
;Descripción: Añade un chatbot al system y en caso de duplicidad lo elimina.
(define system-add-chatbot (lambda (sys cb)
        (if (and (system? sys)(chatbot? cb))
            (change-sys-cb sys (dup-list-cb (append (Sel-sys-cb sys) (list cb))))
            InvalidSystem)))

;------------------------------------------
; Requerimiento Funcional 9 System add User
;------------------------------------------

;Dominio: System x User
;Recorrido: System
;Recursividad: No aplica
;Descripción: Agrega un user al system y en caso de duplicidad lo elimina.
(define system-add-user (lambda (sys usr)
        (if (and (system? sys)(string? usr))
            (change-sys-usr-list sys (dup-list-usr (append (Sel-sys-list-usr sys) (list (user usr)))))
            InvalidUser)))

;----------------------------------------
; Requerimiento Funcional 10 System Login
;----------------------------------------

;Dominio: System x usr-name(string)
;Recorrido: System
;Recursividad: No aplica
;Descripción: Si no hay una sesión activa y el string ingresado corresponde a un user ingresado en el system,
;             se crea una sesión activa con el usuario escogido.
(define system-login (lambda (sys usr-name)
        (if (and (system? sys)(string? usr-name))
            (if (and (equal? NoUser (Sel-sys-usr sys))(usr-is-in? (user usr-name) (Sel-sys-list-usr sys)))
                (change-sys-usr sys (user usr-name))
                sys)
            InvalidSystem)))

;-----------------------------------------
; Requerimiento Funcional 11 System Logout
;-----------------------------------------

;Dominio: System
;Recorrido: System
;Recursividad: No aplica
;Descripción: Reemplaza el user en la sesión activa por NoUser, además actualiza el user de la sesión
;             cerrada en la lista de users.
(define system-logout (lambda (sys)
        (if (system? sys)
            (if (equal? (Sel-sys-usr sys) NoUser)
                sys
                (change-sys-usr (refresh-sys-usr sys) NoUser))
            InvalidSystem)))

;-------------------------------------------
; Requerimiento Funcional 12 System talk rec
;-------------------------------------------

;Dominio: System x msg(string)
;Recorrido: System
;Recursividad: Propia de sys-update-rec (Cola)
;Descripción: Permite interactuar con un system ingresando un string, este puede ser una keyword o un id.
;             En caso de que el msg se encuentre en el estado actual del system se actualiza este último
;             al nuevo estado, en caso contrario, se agrega el estado actual al chathistory del usuario
;             actual.
(define system-talk-rec (lambda (sys msg)
        (if (and (system? sys)(string? msg)(not (equal? (Sel-sys-usr sys) NoUser)))
            (if (msg-in-sys? sys msg)
                (sys-update-rec sys msg)
                (sys-update-usr sys msg))
            InvalidSystem)))

;---------------------------------------------
; Requerimiento Funcional 13 System talk norec
;---------------------------------------------

;Dominio: System x msg
;Recorrido: System
;Recursividad: No aplica
;Descripción: Permite interactuar con un system ingresando un msg, siempre que exista un usuario
;             Logueado. Similar a system-talk-rec pero sin utilizar recursividad.
(define system-talk-norec (lambda (sys msg)
        (if (and (system? sys)(string? msg)(not (equal? (Sel-sys-usr sys) NoUser)))
            (if (msg-in-sys? sys msg)
                (sys-update-norec sys msg)
                (sys-update-usr sys msg))
            InvalidSystem)))

;--------------------------------------------
; Requerimiento Funcional 14 System synthesis
;--------------------------------------------

;Dominio: System x usr-name(string)
;Recorrido: string
;Recursividad: No aplica
;Descripción: Usando la función format-sys obtiene un string con el chathistory del usuario solicitado
;             formateado para display.
(define system-synthesis (lambda (sys usr-name)
        (if (and (system? sys)(string? usr-name))
            (if (usr-is-in? (user usr-name) (Sel-sys-list-usr sys))
                (format-sys (if (equal? (Sel-usr-name (Sel-sys-usr sys)) usr-name)
                                (Sel-sys-usr sys)
                                (find-usr (user usr-name) (Sel-sys-list-usr sys))))
                InvalidUser)
            InvalidUser)))

;-------------------------------------------
; Requerimiento Funcional 15 System simulate
;-------------------------------------------

;(define system-simulate (lambda (sys maxInter seed)
        

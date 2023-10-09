#lang Racket

(require "option_21130814_MonjeRojas.rkt")
(require "flow_21130814_MonjeRojas.rkt")
(require "chatbot_21130814_MonjeRojas.rkt")
(require "user_21130814_MonjeRojas.rkt")
(require "chathistory_21130814_MonjeRojas.rkt")
(provide (all-defined-out))

;############### TDA System ###############

;REPRESENTACIÓN TDA
;system: name(string) x InitialChatbotCodeLink(positive integer) x Chatbots(list) x User x List of Users(list)

(define InvalidSystem null) ;Se define system no aceptado para retorno


;--------------------Pertenencia

;Dominio: Any
;Recorrido: Boolean
;Recursividad: No aplica
;Descripción: Comprueba que el argumento entregado sea un system.
(define system? (lambda (sys)
        (and (list? sys)(= (length sys) 5)(string? (car sys))
             (epi? (cadr sys))(all-chatbot? (caddr sys))(user? (cadddr sys))
             (all-user? (car (cddddr sys))))))

;--------------------Selectores

;Dominio: System
;Recorrido: string
;Recursividad: No aplica
;Descripción: Retorna el nombre de un system.
(define Sel-sys-name (lambda (sys)
        (if (system? sys)
            (list-ref sys 0)
            null)))

;Dominio: System
;Recorrido: StartChatbotCode(positive integer)
;Recursividad: No aplica
;Descripción: Retorna el StartChatbotCode de un system.
(define Sel-sys-startcbcode (lambda (sys)
        (if (system? sys)
            (list-ref sys 1)
            null)))

;Dominio: System
;Recorrido: List of Chatbots(list)
;Recursividad: No aplica
;Descripción: Retorna los chatbots de un system.
(define Sel-sys-cb (lambda (sys)
        (if (system? sys)
            (list-ref sys 2)
            null)))

;Dominio: System
;Recorrido: User
;Recursividad: No aplica
;Descripción: Retorna la sesión actual de usuario de un system.
(define Sel-sys-usr (lambda (sys)
        (if (system? sys)
            (list-ref sys 3)
            null)))

;Dominio: System
;Recorrido: List of Users(list)
;Recursividad: No aplica
;Descripción: Retorna la lista de usuarios registrados de un system.
(define Sel-sys-list-usr (lambda (sys)
        (if (system? sys)
            (list-ref sys 4)
            null)))


;--------------------Modificadores

;Dominio: System x List of Chatbots(list)
;Recorrido: System
;Recursividad: No aplica
;Descripción: Reemplaza la lista de chatbots de un system por la indicada.
(define change-sys-cb (lambda (sys list-cb)
        (define delete-sys-cb (lambda (sys)
                (remove (Sel-sys-cb sys) sys)))
        (if (and (system? sys)(all-chatbot? list-cb))
            (cons (Sel-sys-name sys) (cons (Sel-sys-startcbcode sys) (cons list-cb (cdddr sys))))
            InvalidSystem)))

;Dominio: System x List of Users(list)
;Recorrido: System
;Recursividad: No aplica
;Descripción: Reemplaza la lista de Users de un system por la indicada.
(define change-sys-usr-list (lambda (sys list-usr)
        (define delete-sys-list-usr (lambda (usr)
                (remove (Sel-sys-list-usr sys) sys)))
        (if (and (system? sys)(all-user? list-usr))
            (append (delete-sys-list-usr sys) (list list-usr))
            InvalidUser)))


  ; Funciones para System-login-logout

;Dominio: User x List of Users(list)
;Recorrido: Boolean
;Recursividad: No aplica
;Descripción: verifica si un user esta en una lista de users usando su nombre.
(define usr-is-in? (lambda (usr list-usr)
        (list? (findf (lambda (arg) (equal? (Sel-usr-name usr) (Sel-usr-name arg))) list-usr))))

;Dominio: System x User
;Recorrido: System
;Recursividad: No aplica
;Descripción: Reemplaza el user en la sesión activa del system por el indicado.
(define change-sys-usr (lambda (sys usr)
        (if (and (system? sys)(user? usr))
            (cons (Sel-sys-name sys) (cons (Sel-sys-startcbcode sys)
            (cons (Sel-sys-cb sys) (cons usr (cddddr sys)))))
            InvalidSystem)))

;Dominio: User x List of Users(list)
;Recorrido: User
;Recursividad: No aplica
;Descripción: Busca y retorna un user con el mismo nombre que el ingresado en una lista de users.
(define find-usr (lambda (usr list-usr)
        (if (and (user? usr) (all-user? list-usr))
            (findf (lambda (arg) (equal? (Sel-usr-name usr) (Sel-usr-name arg))) list-usr)
            InvalidUser)))

;Dominio: System
;Recorrido: System
;Recursividad: No aplica
;Descripción: Actualiza los datos de un user almacenados en la lista de users del system a partir de los que
;             están en la sesión activa del mismo.
(define refresh-sys-usr (lambda (sys)
       (define change-list-usr (lambda (list-usr usr)
               (if (and (all-user? list-usr)(user? usr))
                   (dup-list-usr (cons usr list-usr))
                   InvalidUser)))
       (append (remove (Sel-sys-list-usr sys) sys) (list (change-list-usr (Sel-sys-list-usr sys) (Sel-sys-usr sys))))))


  ; Funciones para System talk rec y norec

;Dominio: System x id(positive integer)
;Recorrido: Chatbot
;Recursividad: No aplica
;Descripción: Busca y retorna un chatbot dentro de un system a partir de su id.
(define find-cb (lambda (sys id)
        (findf (lambda (arg) (= id (car arg))) (Sel-sys-cb sys))))

;Dominio: Option x msg(string)
;Recorrido: Boolean
;Recursividad: No aplica
;Descripción: Verifica si un string corresponde o, a la id del option, o a alguna de las keywords del mismo.
(define msg-in-op? (lambda (op msg)
        (if (string? (findf (lambda (arg) (equal? (string-downcase msg) arg)) (Sel-op-keywords op)))
            #t
            (if (not (boolean? (string->number msg)))
                (= (string->number msg) (Sel-op-code op))
                #f))))

;Dominio: Flow x msg(string)
;Recorrido: Boolean
;Recursividad: No aplica
;Descripción: Utiliza msg-in-op? para comprobar si un msg se encuentra en algun option del flow.
(define msg-in-fw? (lambda (fw msg)
        (list? (member #t (map (lambda (arg) (msg-in-op? arg msg)) (Sel-fw-op fw))))))

;Dominio: System x msg(string)
;Recorrido: Boolean
;Recursividad: No aplica
;Descripción: Utiliza msg-in-fw? para comprobar si el mensaje entregado es una respuesta válida para
;             el chatbot, flow y options actuales del system.
(define msg-in-sys? (lambda (sys msg)
        (msg-in-fw? (get-current-fw sys) msg)))

;Dominio: System
;Recorrido: Flow
;Recursividad: No aplica
;Descripción: Obtiene el flow actual del chatbot con el que se está hablandom, es decir,
;             el chatbot del estado actual.
(define get-current-fw (lambda (sys)
        (find-fw (find-cb sys (Sel-sys-startcbcode sys))
                 (Sel-cb-startfwid (find-cb sys (Sel-sys-startcbcode sys))))))

;Dominio: User x ChatHistory
;Recorrido: User
;Recursividad: No aplica
;Descripción: Añade un chathistory a la lista de chathistory contenida en un user.
(define usr-add-log (lambda (usr chathis)
        (if (and (user? usr) (chathistory? chathis))
            (cons (Sel-usr-name usr) (cons (append (Sel-usr-his usr) (list chathis)) null))
            InvalidUser)))

;Dominio: System
;Recorrido: msg(string)
;Recursividad: No aplica
;Descripción: Actualiza el historial de un user a partir de un msg.
(define sys-update-usr (lambda (sys msg)
        (cons (Sel-sys-name sys) (cons (Sel-sys-startcbcode sys) (cons (Sel-sys-cb sys)
        (cons (usr-add-log (Sel-sys-usr sys) (chathistory msg (find-cb sys (Sel-sys-startcbcode sys))
        (get-current-fw sys))) (cddddr sys)))))))

;Dominio: System x newChatbotCode(positive integer)
;Recorrido: System
;Recursividad: No aplica
;Descripción: Actualiza el startChatbotCode de un system, se utiliza para conversar con un system.
(define sys-update-cbcode (lambda (sys newcbcode)
        (cons (Sel-sys-name sys) (cons newcbcode (cddr sys)))))

;Dominio: System x newFlowID(positive integer)
;Recorrido: System
;Recursividad: No aplica
;Descripción: Actualiza el flowid del chatbot actual de un system, se utiliza para cambia de flow
;             según se necesite al conversar con un system.
(define sys-update-cb-fwid (lambda (sys newfwid)
        (cons (Sel-sys-name sys) (cons (Sel-sys-startcbcode sys) (cons
        (change-list-cb-startfwid (Sel-sys-cb sys) (find-cb sys (Sel-sys-startcbcode sys)) newfwid)
        (cdddr sys))))))


  ;Funciones para system talk rec

;Dominio: Flow x msg(string)
;Recorrido: Option
;Recursividad: Cola
;Descripción: Busca si un msg esta en un flow, revisando cada option, sus keywords e ids,
;             De ser encontrado se retorna la option que contiene el msg.
;             Se utiliza funcion msg-in-op? contenida en TDA-System.
(define msg-in-fw-rec (lambda (fw msg)
        (define msg-fw-aux (lambda (list-op msg)
                (if (null? list-op)
                    #f
                    (if (msg-in-op? (car list-op) msg)
                        (car list-op)
                        (msg-fw-aux (cdr list-op) msg)))))
        (msg-fw-aux (Sel-fw-op fw) msg)))

;Dominio: System x msg(string)
;Recorrido: Option
;Recursividad: Propia de msg-in-fw-rec (Cola)
;Descripción: Encapsula función msg-in-fw-rec para realizar búsqueda en system
(define msg-in-sys-rec (lambda (sys msg)
        (msg-in-fw-rec (get-current-fw sys) msg)))

;Dominio: System x msg(string)
;Recorrido: System
;Recursividad: Propia de msg-in-sys-rec y msg-in-fw-rec (Cola)
;Descripción: Funcion que actualiza el start-chatbot-code de un system, el chatbot-initialflowid del
;             chatbot que actualizó la función anterior y finalmente agrega un registro al chathistory del
;             user que está logueado en el system. Esto a partir del option obtenido en msg-in-sys-rec.
(define sys-update-rec (lambda (sys msg)
        (sys-update-usr (sys-update-cb-fwid (sys-update-cbcode sys
        (Sel-op-cbcodelink (msg-in-sys-rec sys msg))) (Sel-op-fwcodelink (msg-in-sys-rec sys msg))) msg)))


  ;Funciones para system talk norec

;Dominio: Flow x msg(string)
;Recorrido: Option
;Recursividad: No aplica
;Descripción: Misma funcion que msg-in-fw-rec pero sin recursividad, busca un option a partir de un msg.
(define msg-in-fw-norec (lambda (fw msg)
        (findf (lambda (arg) (msg-in-op? arg msg)) (Sel-fw-op fw))))

;Dominio: System x msg(string)
;Recorrido: Option
;Recursividad: No aplica
;Descripción: Encapsula msg-in-fw-norec, busca option en un system a partir de un string.
(define msg-in-sys-norec (lambda (sys msg)
        (msg-in-fw-norec (get-current-fw sys) msg)))

;Dominio: System x msg(string)
;Recorrido: System
;Recursividad: No aplica
;Descripción: Actualiza el start-chatbot-code de un system, el chatbot-initialflowid del
;             chatbot y el chathistory del usuario logueado. Similar a system-update-rec pero
;             sin utilizar recursividad.
(define sys-update-norec (lambda (sys msg)
        (sys-update-usr (sys-update-cb-fwid (sys-update-cbcode sys
        (Sel-op-cbcodelink (msg-in-sys-rec sys msg))) (Sel-op-fwcodelink (msg-in-sys-norec sys msg))) msg)))

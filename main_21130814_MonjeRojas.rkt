#lang Racket

(require "option_21130814_MonjeRojas.rkt")
(require "flow_21130814_MonjeRojas.rkt")
(require "chatbot_21130814_MonjeRojas.rkt")
(require "system_21130814_MonjeRojas.rkt")
(require "user_21130814_MonjeRojas.rkt")
(require "chathistory_21130814_MonjeRojas.rkt")
(provide (all-defined-out))

;############### Main ###############

(define (myRandom Xn)
        (modulo (+ (* 1103515245 Xn) 12345) 2147483648)
)

; Funcionalidad 12 System talk rec

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

; Funcionalidad 13 System talk norec

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

; Funcionalidad 14 System synthesis

;Dominio: Lista de strings
;Recorrido: string
;Recursividad: No aplica
;Descripción: Junta los strings de una lista agregando un salto de linea entre ellos, sirve para
;             formatear las opciones almacenadas en el chathistory.
(define str-append-cola (lambda (lista)
        (map (lambda (arg) (string-append arg "\n")) lista)))

;Dominio: User
;Recorrido: string
;Recursividad: No aplica
;Descripción: Formatea un chathistory juntando los strings y añadiendo saltos de linea, retorna un string
;             listo para display
(define format-sys (lambda (usr)
        (define format-aux (lambda (chathis)
                (string-append (Sel-usr-name usr) ": " (car chathis) "\n"
                (cadr chathis) ": " (caddr chathis) "\n"
                (apply string-append (str-append-cola (cdddr chathis))) "\n\n")))
        (apply string-append (map format-aux (Sel-usr-his usr)))))

;Dominio: System x usr-name(string)
;Recorrido: string
;Recursividad: No aplica
;Descripción: Usando la función format-sys obtiene un string con el chathistory del usuario solicitado
;             formateado para display.
(define system-synthesis (lambda (sys usr-name)
        (if (and (system? sys)(string? usr-name))
            (if (usr-is-in (user usr-name) (Sel-sys-list-usr sys))
                (format-sys (if (equal? (Sel-usr-name (Sel-sys-usr sys)) usr-name)
                                (Sel-sys-usr sys)
                                (find-usr (user usr-name) (Sel-sys-list-usr sys))))
                InvalidUser)
            InvalidUser)))

; Funcionalidad 15 System simulate

;(define system-simulate (lambda (sys maxInter seed)
        

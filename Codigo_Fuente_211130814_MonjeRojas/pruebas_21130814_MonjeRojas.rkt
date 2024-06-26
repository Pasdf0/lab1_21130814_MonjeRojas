#lang Racket

(require "main_21130814_MonjeRojas.rkt")
(require "option_21130814_MonjeRojas.rkt")
(require "flow_21130814_MonjeRojas.rkt")
(require "chatbot_21130814_MonjeRojas.rkt")
(require "system_21130814_MonjeRojas.rkt")
(require "user_21130814_MonjeRojas.rkt")
(require "chathistory_21130814_MonjeRojas.rkt")

;############### Script de pruebas ###############

;Chabot0
(define op1 (option 1 "1) Viajar" 1 1 "viajar" "turistear" "conocer"))
(define op2 (option 2 "2) Estudiar" 2 1 "estudiar" "aprender" "perfeccionarme"))
(define f10 (flow 1 "Flujo Principal Chatbot 1\nBienvenido\n¿Qué te gustaría hacer?" op1 op2 op2 op2 op2 op1))
;solo añade una ocurrencia de op2 y op1
(define f11 (flow-add-option f10 op1)) ;se intenta añadir opción duplicada
(define cb0 (chatbot 0 "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f10 f10 f10 f10))
;solo añade una ocurrencia de f10
;Chatbot1
(define op3 (option 1 "1) New York, USA" 1 2 "USA" "Estados Unidos" "New York"))
(define op4 (option 2 "2) París, Francia" 1 1 "Paris" "Eiffel"))
(define op5 (option 3 "3) Torres del Paine, Chile" 1 1 "Chile" "Torres" "Paine" "Torres Paine" "Torres del Paine"))
(define op6 (option 4 "4) Volver" 0 1 "Regresar" "Salir" "Volver"))
;Opciones segundo flujo Chatbot1
(define op7 (option 1 "1) Central Park" 1 2 "Central" "Park" "Central Park"))
(define op8 (option 2 "2) Museos" 1 2 "Museo"))
(define op9 (option 3 "3) Ningún otro atractivo" 1 3 "Museo"))
(define op10 (option 4 "4) Cambiar destino" 1 1 "Cambiar" "Volver" "Salir"))
(define op11 (option 1 "1) Solo" 1 3 "Solo"))
(define op12 (option 2 "2) En pareja" 1 3 "Pareja"))
(define op13 (option 3 "3) En familia" 1 3 "Familia"))
(define op14 (option 4 "4) Agregar más atractivos" 1 2 "Volver" "Atractivos"))
(define op15 (option 5 "5) En realidad quiero otro destino" 1 1 "Cambiar destino"))
(define f20 (flow 1 "Flujo 1 Chatbot1\n¿Dónde te Gustaría ir?" op3 op4 op5 op6))
(define f21 (flow 2 "Flujo 2 Chatbot1\n¿Qué atractivos te gustaría visitar?" op7 op8 op9 op10))
(define f22 (flow 3 "Flujo 3 Chatbot1\n¿Vas solo o acompañado?" op11 op12 op13 op14 op15))
(define cb1 (chatbot 1 "Agencia Viajes" "Bienvenido\n¿Dónde quieres viajar?" 1 f20 f21 f22))
;Chatbot2
(define op16 (option 1 "1) Carrera Técnica" 2 1 "Técnica"))
(define op17 (option 2 "2) Postgrado" 2 1 "Doctorado" "Magister" "Postgrado"))
(define op18 (option 3 "3) Volver" 0 1 "Volver" "Salir" "Regresar"))
(define f30 (flow 1 "Flujo 1 Chatbot2\n¿Qué te gustaría estudiar?" op16 op17 op18))
(define cb2 (chatbot 2 "Orientador Académico" "Bienvenido\n¿Qué te gustaría estudiar?" 1 f30))
;Sistema
(define s0 (system "Chatbots Paradigmas" 0 cb0 cb0 cb0 cb1 cb2))
(define s1 (system-add-chatbot s0 cb0)) ;igual a s0
(define s2 (system-add-user s1 "user1"))
(define s3 (system-add-user s2 "user2"))
(define s4 (system-add-user s3 "user2"))
(define s5 (system-add-user s4 "user3"))
(define s6 (system-login s5 "user8"))
(define s7 (system-login s6 "user1"))
(define s8 (system-login s7 "user2"))
(define s9 (system-logout s8))
(define s10 (system-login s9 "user2"))
;las siguientes interacciones deben funcionar de igual manera con system-talk-rec o system-talk-norec
(define s11 (system-talk-rec s10 "hola"))
(define s12 (system-talk-rec s11 "1"))
(define s13 (system-talk-rec s12 "1"))
(define s14 (system-talk-rec s13 "Museo"))
(define s15 (system-talk-rec s14 "1"))
(define s16 (system-talk-rec s15 "3"))
(define s17 (system-talk-rec s16 "5"))
(display (system-synthesis s17 "user2"))
;(system-simulate s0 5 32131) ;No se implementó

;Ejemplos complementarios
;Se añaden 3 ejemplos por requerimiento funcional

(define op01 (option 1 "1) test" 1 1 "test01" "test02" "test03"))
(define op02 (option 2 "2) test" 1 1 "test04" 5 "test06"))
(define op03 (option 5 "3) test" 1 1 "" "" ""))
(define f01 (flow 1 1090382 op6 op7 op8 op9))
(define f02 (flow 1 "Flow test" op01 op1 op2 op2))
(define f03 (flow 1 "Empty Flow" ))
(define f12 (flow-add-option f10 op18))
(define f13 (flow-add-option f12 op17)) ;se elimina por duplicidad
(define f31 (flow-add-option f30 op15))
(define cb01 (chatbot 1 "InvalidChatbot" 26 1 f20 f20 f20 f20 f20 f20))
(define cb02 (chatbot 2 "DuplicateChatbot" "test msg" 1 f30 f20 f10))
(define cb03 (chatbot 3 "EmptyChatbot" "test msg 2" 25 ))
(define cb3 (chatbot-add-flow cb0 f21))
(define cb4 (chatbot-add-flow cb3 f22))
(define cb5 (chatbot-add-flow cb2 f20)) ;se elimina por duplicidad
(define s001 (system "Chatbot test" 1 cb0 cb3 cb5 cb5)) ;eliminará cb3 y un cb5 por duplicidad
(define s002 (system "Chatbot test2" 25)) ;se crea sin chatbots
(define s003 (system "Chatbot testfail" "not-id" cb0 cb1 cb2 cb3)) ;Retorna InvalidSystem
(define s004 (system-add-chatbot s001 cb1)) 
(define s005 (system-add-chatbot s002 cb5)) ;se agrega chatbot a system sin chatbots
(define s006 (system-add-chatbot s0 cb5)) ;no se agrega por duplicidad de id
(define s01 (system-add-user s0 123))
(define s02 (system-add-user s0 NoUser))
(define s03 (system-add-user s4 "tests03"))
(define s04 (system-login s0 NoUser))
(define s05 (system-login s0 "user1"))
(define s06 (system-login s0 1))
(define s07 (system-logout 1))
(define s08 (system-logout s0)) ;Se hace logout en un system sin user logueado
(define s09 (system-logout s7)) ;Se hace logout en un system sin chathistory en user activo
(define s010 (system-talk-rec s07 "1"))
(define s011 (system-talk-rec s17 "hola"))
(define s012 (system-talk-rec s17 "1"))
(define s11-norec (system-talk-norec s10 "hola"))
(define s12-norec (system-talk-norec s11-norec "1"))
(define s13-norec (system-talk-norec s12-norec "1"))
(define s14-norec (system-talk-norec s13-norec "Museo"))
(define s15-norec (system-talk-norec s14-norec "1"))
(define s16-norec (system-talk-norec s15-norec "3"))
(define s17-norec (system-talk-norec s16-norec "5"))

(display "\nSystem-synthesis norec: \n\n")
(display (system-synthesis s17-norec "user2")) ;Ejemplo system-synthesis con talk-norec
(display (system-synthesis s10 "user1")) ;Ejemplo user sin historial, no se muestra nada.
(system-synthesis s17 "user2")
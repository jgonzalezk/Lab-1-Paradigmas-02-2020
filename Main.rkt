#lang racket

(require"TDAusuario.rkt")
(require"TDAPregunta.rkt")
(require"TDArespuestas.rkt")
(require"TDAstack.rkt")


;Archivo Main

;Funcion Register
;Dominio: stack X string X string
;Recorrido: stack
;Descripcion: Funcion que registra a un usuario y devuelve el nuevo stack con el usuario registrado, si el usuario ya estaba registrado desde antes se devuelve el stack sin modificar
;Es por esto que al verificar se utiliza la recursion de cola (la funcion usuarioYaExiste?)
(define (register stack username password)
  (if (and (stack? stack) (string? username) (string? password) (usuarioYaExiste? (getUsuarios stack) username))
      stack;Se retorna el mismo stack debido a que el usuario ya se encontraba registrado, o no era valido
      (setUsuariosStack stack (addUsuario (getUsuarios stack) (usuario username password 300)));Debiese ser 0 el puntaje incial de cada usuario pero para probar las funciones se dejó en 300 puntos.
      )
  )

;Funcion Login
;Funcion de auxiliar que recibe el nuevo stack con el usuario activo
(define (operationAux stack operation)
  (operation stack)
  )

;Dominio: stack X string X string X operation
;Recorrido: stack
;Descripcion:Funcion que logea al usuario si los datos ingresados son correctos, en caso que no sea correcto no exite usuario activo, luego se procede a ejecutar la operacion ingresada.
;Recursion: de cola (se utiliza recursion de cola en IniciarSesion? para recorrer y retornar un boolean si el usuario es correcto o no).
(define login(lambda (stack username password operation)
               (if (and (stack? stack)(IniciarSesion? (getUsuarios stack) username password));Si se cumple entonces el usuario se logeo exitosamente
                   (operationAux (pasarUsuarioActivo  stack username password) operation);Pasar al usuario a activo y realizar la funcion ingresada
                   (operation stack) )));Llamar a la funcion sin el usuario activo


;Funcion ask
;Dominio: stack X date X string X list
;Recorrido: stack
;Descripcion: Funcion que agrega una pregunta a la lista de preguntas y retorna el stack actualizado, esto si los datos fueron correctos al igual que el login.
(define ask (lambda (stack)
              (lambda (Fecha)
                (lambda (Pregunta tags)
                  (if (and (date? Fecha) (string? Pregunta) (or (null? tags) (etiquetas? tags)) (not (null? (getActivoS stack))))
                      (list (getUsuarios stack) (agregarPregunta (getPreguntas stack) (pregunta (+ (getIdAnterior stack) 1) Pregunta Fecha 0 0 tags 0 0 0 (getNombreActivo stack) respuestasVacia 0)) usuarioActivoVacio);Stack actualizado
                      (list (getUsuarios stack) (getPreguntas stack) usuarioActivoVacio);Si los parametros no son correctos o el login no fue correcto entonces se regresa el mismo stack
                      )
                  )
                )
              )
  )

;Funcion reward
;Dominio: stack X int X int
;Recorrido:stack
;Descripcion: Funcion que agrega recompensa a alguna preguna siempre y cuando el usuario tenga puntos suficientes, se retorna el stack actualizado si los parametros son correctos.
;Recursion: natural (debido a las funciones auxiliares, estas estan comentadas debidamente).
(define reward (lambda (stack)
                 (lambda (IDPregunta)
                   (lambda (recompensa)
                     (if (and (integer? IDPregunta) (integer? recompensa) (> recompensa 0) (not(null? (getActivoS stack))) (not (null? (auxGetQuestion (getPreguntas stack) IDPregunta))))
                         (if (>= (getPuntosActivo stack) recompensa)
                             (list (cambiarPuntosUsuario (getUsuarios stack) (getNombreActivo stack) (getPasswordActivo stack) (getPuntosActivo stack) recompensa)
                                   (cambiarPuntosPregunta (getPreguntas stack)  IDPregunta recompensa) usuarioActivoVacio)
                             (list (getUsuarios stack) (getPreguntas stack) usuarioActivoVacio))
                         (list (getUsuarios stack) (getPreguntas stack) usuarioActivoVacio))
                     )
                   )
                 )
  )
                             
;Funcion answer
;Dominio: stack X date X int X string X list
;Recorrido: stack
;Descripcion: Funcion que agrega una respuesta a la pregunta deseada, esto siempre y cuando los parametros sean correctos, si es asi se retorna el stack actualizado.
;Recursion: natural (debido a la funcion auxiliar, esta esta comentada debidamente).
(define answer (lambda (stack)
                 (lambda (Fecha)
                   (lambda (IDpregunta)
                     (lambda (Respuesta tags)
                       (if (and (date? Fecha) (string? Respuesta) (or(null? tags) (etiquetas? tags)) (not (null? (getActivoS stack))))
                           (list (getUsuarios stack) (AgregarRespuestaNueva stack (getPreguntas stack) Fecha IDpregunta Respuesta tags) usuarioActivoVacio)
                           (list (getUsuarios stack) (getPreguntas stack) usuarioActivoVacio))
                       )
                     )
                   )
                 )
  )
                       
;Funcion accept
;Dominio: stack X int X int
;Recorrido:stack
;Descripcion: Esta funcion acepta la respuesta de la pregunta ingresada, siempre y cuando el que quiere aceptar la respuesta es el autor de la pregunta y tambien los parametros son correctos, si es asi
;se retorna el stack actualizado.
;Recursion: natural (funciones auxiliares debidamente comentadas).
(define accept(lambda (stack)
                (lambda (IDPregunta)
                  (lambda (IDRespuesta)
                    (if (and (integer? IDPregunta) (integer? IDRespuesta) (not (null? (getActivoS stack))) (ExistePregunta? (getPreguntas stack) (getNombreActivo stack) IDPregunta IDRespuesta))
                        (list (usuariosActRecompensa (getUsuarios stack) (conseguirNombreResp (getPreguntas stack) IDPregunta IDRespuesta) (conseguirRecompensaP (getPreguntas stack) IDPregunta))
                              (preguntasActEstado (getPreguntas stack) IDPregunta IDRespuesta)
                              usuarioActivoVacio)
                        (list (getUsuarios stack) (getPreguntas stack) usuarioActivoVacio))
                    )
                  )
                )
  )

;Funcion stack->string
;Dominio:stack
;Recorrido: string
;Descripcion: Funcion que transforma todo el stack a un string para luego ser utilizado con la funcion display, esto para tener una forma mas clara del foro.
;Recursion: principalmente natural (estan comentadas las funciones que se utilizan).
(define stack->string(lambda (stack)
                       (if (not (null? (getActivoS stack)));Si el usuario se logeo
                           (string-append "\n\nPreguntas y respuestas:\n" (stringPreguntas (getPreguntas stack))
                                          "\n\nUsuario activo:\n" (stringUsuarioActivo stack))
                           
                           (string-append "Usuarios registrados:\n\n" (stringUsuarios (getUsuarios stack))
                                          "\n\nPreguntas y respuestas:\n" (stringPreguntas (getPreguntas stack))
                                          "\n\nUsuario activo:\n" );No hay usuario activo
                           )
                       )
  )
;--------------------------------------------------------------------------------------------------------------------------
;Funcion vote
;Dominio: stack X funcion X ID X boolean
;Recorrido: stack
;Descripcion: Funcion que agrega un voto posito o negativo a la pregunta o a la respuesta segun lo que se haya ingresado, se retorna el stack actualizado si los parametros son correctos.
;Recursion: Principalmente natural (las funciones auxiliares estan comentadas).
(define vote(lambda (stack)
              (lambda (parametro1)
                (lambda (parametro2)
                  (lambda (voto)
                    (if (not (null? (getActivoS stack)))
                        (if (not (null? (length ((parametro1 (getPreguntas stack)) parametro2))));Si el retorno es null la pregunta y/o respuesta no existe
                            (if(= 7 (length ((parametro1 (getPreguntas stack)) parametro2)));Si el largo es 7 es porque retorna una respuesta
                               (list (getUsuarios stack) (agregarVotoRespuesta (getPreguntas stack) ((parametro1 (getPreguntas stack)) -4000) ((parametro1 (getPreguntas stack)) parametro2) voto) usuarioActivoVacio)
                               (list (getUsuarios stack) (agregarVotoPregunta (getPreguntas stack) ((parametro1 (getPreguntas stack)) parametro2) voto) usuarioActivoVacio)
                               )
                            (list (getUsuarios stack) (getPreguntas stack) usuarioActivoVacio))
                        (list (getUsuarios stack) (getPreguntas stack) usuarioActivoVacio))
                    )
                  )
                )
              )
  )




                    
;-------------------------------- FUNCIONES PARA stack->string ----------------------------------------------------------
;Dominio:usuario
;Recorrido: string
;Descripcion: Funcion que concatena todos los datos de un usuario como un string
(define (stringUsuario user)
  (string-append "Usuario: " (getUsuario user) "\n" "Password: " (getPassword user) "\n" "Puntos: " (number->string (getPoints user)))
  )

;Dominio: listaUsuarios
;Recorrido: string
;Descripcion: Funcion que concatena a todos los usuarios registrados
;Recursion: natural debido a que quedan operaciones pendientes.
(define stringUsuarios (lambda (listaUsuarios)
                         (if (usuariosVacio? listaUsuarios)
                             (string-append  "" "\n")						              
                             (string-append (stringUsuario (getPrimerUser listaUsuarios)) "\n\n" (stringUsuarios (getSigUser listaUsuarios)))
                             )
                         )
  )

;Dominio: lista de etiquetas
;Recorrido: string
;Descripcion: Funcion que concatena a todas las etiquetas en un string
;Recursion: natural debido a que quedan operaciones pendientes.
(define stringEtiquetas (lambda (listaTags)
                          (if (null? listaTags)
                              (string-append  "" "-")						              
                              (string-append (getPrimerTag listaTags) "-" (stringEtiquetas (getSigTag listaTags)))
                              )
                          )
  )

;Dominio: respuesta
;Recorrido: string
;Descripcion: Funcion que concatena todos los datos de una respuesta
(define (stringRespuesta resp)
  (string-append "Id: " (number->string (getIDresp resp)) " , Fecha: " (number->string(getDia (getRfecha resp))) " " (number->string(getMes (getRfecha resp))) " " (number->string(getYear (getRfecha resp))) "\n"
                 "Etiquetas: " (stringEtiquetas (getRetiq resp)) "\n"
                 "Autor: " (getRautor resp) "\n" "Respuesta: " (getResp resp) "\n" "Votos positivos: " (number->string (getVFR resp)) ", Votos negativos: " (number->string (getVCR resp)))
  )

;Dominio: listaRespuestas
;Recorrido: string
;Descripcion: Funcion que concatena a todas las respuestas
;Recursion: natural debido a que quedan operaciones pendientes.
(define stringRespuestas (lambda (listaRespuestas)
                           (if (respuestasVacia? listaRespuestas)
                               (string-append  "" "\n")						              
                               (string-append (stringRespuesta (getPrimerResp listaRespuestas)) "\n\n" (stringRespuestas (getSigResp listaRespuestas)))
                               )
                           )
  )

;Dominio: pregunta
;Recorrido: string
;Descripcion: Funcion que concatena todos los datos de una pregunta
(define (stringPregunta preg)
  (string-append "Id: " (number->string (getID  preg))  " , Fecha: " (number->string(getDia (getFecha preg))) " " (number->string(getMes (getFecha preg))) " " (number->string(getYear (getFecha preg))) "\n"
                 "Estado: " (if (= 0 (getEstado preg)) "No resuelta" "Resuelta") "\nVisitas: " (number->string (getVistas  preg)) "\n"
                 "Autor: " (getAutor preg) "\nRecompensa: " (number->string (getRecom  preg)) "\n"
                 "Etiquetas: " (stringEtiquetas (getEtiq preg)) "\n"
                 "Pregunta: " (getPreg preg) "\nVotos positivos: " (number->string (getVF  preg)) ", Votos negativos: " (number->string (getVC  preg))
                 "\nRespuesta aceptada: " (if (= 0 (getRaceptada preg)) "Aun no se acepta una respuesta." (string-append "La respuesta aceptada tiene el id: " (number->string (getRaceptada  preg))))
                 "\nRespuestas:\n\n" (stringRespuestas (getRespuestas preg)))
  )

;Dominio: listaUsuarios
;Recorrido: string
;Descripcion: Funcion que concatena a todas las preguntas
;Recursion: natural debido a que quedan operaciones pendientes.
(define stringPreguntas (lambda (listaPreguntas)
                          (if (preguntasVacio? listaPreguntas)
                              (string-append  "" "\n")						              
                              (string-append (stringPregunta (getPrimerPregunta listaPreguntas)) "\n\n" (stringPreguntas (getSigPreg listaPreguntas)))
                              )
                          )
  )

;Dominio: stack
;Recorrido: string
;Descripcion: Se accede a la lista de usuario activo y se concatena los datos de este usario.
(define stringUsuarioActivo (lambda (stack)
                              (string-append "Usuario: " (getNombreActivo stack) "\nPassword: " (getPasswordActivo stack) "\nPuntos: " (number->string (getPuntosActivo stack))
                                             )
                              )
  )

;--------------------------------------------------------------------------------------------------------------------------
;FUNCIONES DE PRUEBA
;FUNCION REGISTER
;Se juntan tres register en un llamado para no tener que estar redefiniendo el stackOverflow
;(define stackOverflow (register (register (register stackVacio "jorge" "clave1") "juan" "clave2") "pedro" "clave3"))
;--------------------------------------------------------------------------------------------------------------------------
;ASK FUNCIONES DE EJEMPLO
;En la tercera funcion el usuario se logea con una contraseña erronea
;(define stackOverflow1 (((login stackOverflow "jorge" "clave1" ask)(date 12 11 2020))"pregunta de prueba sin tags" '()))
;(define stackOverflow2 (((login stackOverflow1 "pedro" "clave3" ask)(date 13 11 2020))"como puedo concatenar strings en scheme?" '("scheme" "drRacket" "strings")))
;(((login stackOverflow2 "juan" "clave3" ask)(date 13 11 2020))"pregunta generica" '("e1")) 
;--------------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------------------------------
;REWARD
;La tercera funcion se le esta aplicando a la pregunta 88 en este caso no existe
;(define stackOverflow3 (((login stackOverflow2 "pedro" "clave3" reward)2)200))
;(define stackOverflow4 (((login stackOverflow3 "jorge" "clave1" reward)1)5))
;(((login stackOverflow3 "jorge" "clave1" reward)88)100)
;--------------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------------------------------
;ANSWER
;Tercera funcion los parametros de entrada incorrectos
;(define stackOverflow5 ((((login stackOverflow4 "juan" "clave2" answer)(date 14 11 2020))2)"para concatenar string puedes utilizar la funcion string-append" '("drRacket")))
;(define stackOverflow6 ((((login stackOverflow5 "jorge" "clave1" answer)(date 14 11 2020))2)"respuesta generica" '()))
;((((login stackOverflow6 "jorge" "clave1" answer)(date 14 11 2020))2)9999999999999999 '(1 2 3 4))
;--------------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------------------------------
;ACCEPT
;Segunda funcion, el usuario que quiere acepatar una respuesta no es el autor de la pregunta
;Tercera funcion la pregunta y/o respuesta no existe
;(define stackOverflow7(((login stackOverflow6 "pedro" "clave3" accept)2)1))
;(((login stackOverflow6 "jorge" "clave1" accept)2)1)
;(((login stackOverflow6 "pedro" "clave3" accept)88)15) 
;--------------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------------------------------
;STACK TO STRING
;Primera funcion muestra todo el stack, segunda solo las preguntas y el usuario logeado, y la tercera muestra todo el stack
;debido a que el logeo fue incorrecto
;(stack->string stackOverflow7)
;(login "jorge" "clave1" stack->string)
;(login "jorge" "claveMala" stack->string)
;--------------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------------------------------
;VOTE
;La primera funcion agrega un voto negativo a la respuesta 2 de la pregunta 2
;La segunda funcion agrega un voto positivo a la pregunta 1
;La tercera funcion intenta agregar un voto a la pregunta 888 pero esta no existe, asi que no se generan cambios.
;((((login stackOverflow7 "jorge" "clave1" vote)(getAnswer 2))2)false)
;((((login stackOverflow7 "pedro" "clave3" vote)getQuestion)1)true)
;((((login stackOverflow7 "jorge" "clave1" vote)getQuestion)888)false)
;--------------------------------------------------------------------------------------------------------------------------
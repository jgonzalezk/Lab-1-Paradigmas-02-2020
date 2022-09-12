#lang racket

(require"TDAusuario.rkt")
(require"TDAPregunta.rkt")
(require"TDARespuestas.rkt")
(provide (all-defined-out))

;TDA Usuarios
;Constructores
(define usuariosVacio null) ;lista vacia de usuarios

(define usuariosVacio? null?);Funcion para comprobar si es null o no

;Pertenencia
;Dominio: usuarios
;Recorrido: Boolean 
;Descripcion: Funcion que verifica si usuarios es una lista, tambien verifica si sus elementos son usuario o no.
(define usuarios? (lambda (US) (if (list? US)
                                   (if (not (null? US))
                                       (andmap usuario? US)
                                       true);Retorna true si es nulo ya que puede que no existan usuarios
                                   false)
                    )
  )

;Modificadores
(define agregarUsuario cons)

;Dominio: usuarios X usuario (lista de usuarios y un usuario)
;Recorrido: usuarios
;Descripcion: Si el usuario que se desea agregar a la lista de usuarios es correcto entonces se agrega el usuario en la cola, si no se retorna la misma lista de usuarios
;Recursion: Natural
(define addUsuario (lambda (US us)
                     (if (usuario? us)
                         (if (usuariosVacio? US)
                             (agregarUsuario us US)
                             (agregarUsuario (getPrimerUser US) (addUsuario(getSigUser US) us)))
                         US)
                     )
  )

;Selectores
(define (getPrimerUser US)
  (if (and (usuarios? US) (not (usuariosVacio? US)))
      (car US)
      null)
  )
(define (getSigUser US)
  (if (and (usuarios? US) (not (usuariosVacio? US)))
      (cdr US)
      null)
  )
;------------------------------------------------
;TDA usuario activo
;Constructores
(define usuarioActivoVacio null);Creacion de una lista vacia de usuario activo

(define usarioActivoVacio? null?);Si usuario activo es nulo es porque no hay un usuario activo
;Modificadores
;Dominio: usuarioActivo X usuario
;Recorrido: usuarioActivo
;Descripcion: Funcion que agrega a un usuario como activo
(define addActivo cons)
(define agregarUsuarioAct (lambda(usAct U)
                            (if( usuario? U)
                               (addActivo U usAct)
                               U;Es lo mismo que devolviera un null
                               )
                            )
  )

;Dominio: stack X string X string
;Recorrido: UsuarioActivo
;Descripcion: Funcion que agrega al usuario ingresado activo, se busca la cantidad de puntos que posee para tambien agregarlos.
(define (pasarUsuarioActivo stack username password)
  (list (getUsuarios stack) (getPreguntas stack) (agregarUsuarioAct (getActivoS stack) (usuario username password
                                                                                                (getPuntosUsuarioLogin (getUsuarios stack) username password)))))


;Selector usuario activo
(define (getActivo uAct)
  (if (not (null? uAct))
      (car uAct)
      (raise "")
      )
  )
(define (getNombreActivo stack)
  (car (getActivo (getActivoS stack))))

(define (getPasswordActivo stack)
  (cadr (getActivo (getActivoS stack))))

(define (getPuntosActivo stack)
  (caddr (getActivo (getActivoS stack))))
                         
;------------------------------------------------
;TDA Preguntas
;Constructores
(define preguntasVacio null);Creacion de una lista vacia que contendra otras "pregunta"

(define preguntasVacio? null?);Funcion para comprobar si es null o no

;Modificadores
;Dominio: preguntas X pregunta (lista de preguntas X pregunta que se desea agregar)
;Recorrido:preguntas
;Descripcion: Funcion que agrega una pregunta al listado de preguntas
(define addPregunta cons)
(define agregarPregunta (lambda(Preguntas P)
                          (if( pregunta? P)
                             (addPregunta P Preguntas)
                             Preguntas;Si lo que se desea agregar no es una pregunta se retorna la misma lista de preguntas anterior                               
                             )
                          )
  )

;Pertenencia
;Dominio: preguntas
;Recorrido: Boolean
;Descripcion: Funcion que comprueba si "preguntas" es una lista de "pregunta" o no, si no lo es retorna false, si lo es retorna true, si preguntas esta vacia tambien retorna true
(define preguntas? (lambda (LPS) (if (list? LPS)
                                     (if (not (preguntasVacio? LPS))
                                         (andmap pregunta? LPS)
                                         true);Retorna true si es nulo ya que puede que no existan preguntas
                                     false)
                     )
  )

;Selectores
(define (getPrimerPregunta LPS);lista preguntas
  (if (and (preguntas? LPS) (not (null? LPS)))
      (car LPS)
      null)
  )
(define (getSigPreg LPS)
  (if (and (preguntas? LPS) (not (null? LPS)))
      (cdr LPS)
      null)
  )

;------------------------------------------------
;TDA Stack
;Representacion: '('(USUARIOS) '(PREGUNTAS) '(USUARIO ACTIVO))
;Constructor
(define stackVacio (list usuariosVacio preguntasVacio usuarioActivoVacio));Se crea una lista que contiene a los usuarios, preguntas y usuario activo, todas estas sub listas vacias

;Pertenencia
;Dominio: stack
;Recorrido: Boolean
;Descripcion: Comprueba si "stack" no es nulo, si no lo es entonces se comprueba si los elementos de este son nulos, si no lo son retorna false, si lo son returna true 
(define (stackVacio? stack)
  (if (and (not (null? stack))
           (andmap null? stack))
      true
      false)
  )

;Dominio: stack
;Recorrido: Boolean
;Descripcion: Funcion que comprueba si es un stack o no, se comprueban los elementos de este si son correctos o no, si todo es correcto retorna true, caso contrario retorna false
(define (stack? stack)
  (if (and (list? stack) (= (length stack) 3))
      (if (and (usuarios? (getUsuarios stack))
               (preguntas? (getPreguntas stack))
               (list? (getActivoS stack)))
          true
          false)
      false)
  )


       
;Selectores
(define (getUsuarios stack) (car stack))
(define (getPreguntas stack) (cadr stack))
(define (getActivoS stack) (caddr stack))

;Modificadores
;Agregar una lista de usuarios nueva
;Dominio: stack X usuarios
;Recorrido: stack
;Descripcion: Funcion que agrega la nueva lsita de usuarios al stack y se retorna el stack actualizado
(define (setUsuariosStack stack usuariosNews)
  (if (and (stack? stack) (usuarios? usuariosNews))
      (list usuariosNews (getPreguntas stack) (getActivoS stack))
      stack
      )
  )




;------------------------------------------------------------------
;OTRAS FUNCIONES
;Verificar si un usuario se encuentra ya en la lista de usuarios
;Dominio: usuarios X string
;Recorrido: Boolean
;Descripcion: Funcion que verifica si ya existe un usuario con el mismo nombre, si existe retorna  true, caso contrario retorna false
;Recursion: de cola, funcion que verifica usuario por usuario de la lista de usuarios si el nombre ya existe o no.
(define (usuarioYaExiste? US username)
  (if (null? (getPrimerUser US))
      false
      (if (eq? username (getUsuario (getPrimerUser US)))
          true
          (usuarioYaExiste? (cdr US) username))
      )
  )


;Dominio: usuarios X string X string
;Recorrido: Boolean
;Descripcion: Funcion que verifica si coinciden los datos de un usuario registrado con el nombre y contraseña ingresados.
;Recursion: de cola
(define (IniciarSesion? US username password)
  (if (null? (getPrimerUser US))
      false
      (if (and (eq? username (getUsuario (getPrimerUser US))) (eq? password (getPassword (getPrimerUser US))))
          true
          (IniciarSesion? (cdr US) username password))
      )
  )


;Dominio: ListaUsuarios X string X string
;Recorrido: int
;Descripcion: Funcion que busca a un usuario entre los registrados mediate su nombre y contraseña y retorna la cantidad de puntos que este posee.
;Recursion: de cola
(define (getPuntosUsuarioLogin US username password)
  (if (null? (getPrimerUser US))
      0
      (if (and (eq? username (getUsuario (getPrimerUser US))) (eq? password (getPassword (getPrimerUser US))))
          (getPoints (getPrimerUser US))
          (getPuntosUsuarioLogin (cdr US) username password))
      )
  )

;Dominio: stack
;Recorrido: int
;Descripcion: Funcion que retorna la ID de la pregunta anterior
(define (getIdAnterior stack)
  (if (null? (getPreguntas stack))
      0
      (getID (getPrimerPregunta(getPreguntas stack)))
      )
  )

;Dominio: list
;Recorrido: boolean
;Descripcion: Funcion que retorna true si todos los elementos de la lista son string, si no se cumple ninguna de estas condiciones retorna false.
(define (etiquetas? tags)
  (if (and (list? tags)
           (andmap string? tags))
      true
      false)
  )

;Dominio: ListaUsuarios X string X string X int X int
;Recorrido: ListaUsuarios
;Descripcion: Funcion que recorre a los usuarios registrados hasta encontrar el que se esta buscando y descontarle los puntos que ofrece como recompensa.
;Recursion: natural
(define cambiarPuntosUsuario (lambda (listaUsuarios nameActivo passActivo pointsActivo recompensa)
                               (if (usuariosVacio? listaUsuarios)
                                   listaUsuarios
                                   (if (equal? (getUsuario (getPrimerUser listaUsuarios)) nameActivo)
                                       (agregarUsuario (usuario nameActivo passActivo (- pointsActivo recompensa)) (getSigUser listaUsuarios))
                                       (agregarUsuario (getPrimerUser listaUsuarios) (cambiarPuntosUsuario  (getSigUser listaUsuarios) nameActivo passActivo pointsActivo recompensa))
                                       )
                                   )
                               )
  )

;Dominio: ListaPreguntas X int X int
;Recorrido: ListaPreguntas
;Descripcion: Funcion que recorre la lista de preguntas hasta encontrar la que coincide con el id ingresado y modificar los puntos ofrecidos de recompensa
;Recursion: natural
(define cambiarPuntosPregunta (lambda (listaPreguntas  IDPregunta recompensa)
                                (if (preguntasVacio? listaPreguntas)
                                    listaPreguntas
                                    (if (= (getID (getPrimerPregunta listaPreguntas)) IDPregunta)
                                        (addPregunta (setRecompensa (getPrimerPregunta listaPreguntas) recompensa) (getSigPreg listaPreguntas))
                                        (addPregunta (getPrimerPregunta listaPreguntas) (cambiarPuntosPregunta (getSigPreg listaPreguntas) IDPregunta recompensa))
                                        )
                                    )
                                )
  )



;Dominio: ListaRespuestas X date X string X etiquetas X string
;Recorrido: ListaRespuestas
;Descripcion: Funcion que agrega un respuesta a la lista de respuestas.
(define (nuevaListaRespuestas RS Fecha Respuesta tags autor)
  (addRespuesta RS Fecha Respuesta tags autor 1 0 0))

;Dominio: stack X ListaPreguntas X date X int X string X etiquetas
;Recorrido: ListaPreguntas
;Descripcion: Funcion que agrega una respuesta a la pregunta ingresada (id) y retorna la lista de preguntas actualizada con la respuesta nueva a la pregunta.
;Recursion: natural
(define AgregarRespuestaNueva (lambda (stack listaPreguntas Fecha IDPregunta Respuesta tags)
                                (if (preguntasVacio? listaPreguntas)
                                    listaPreguntas
                                    (if (= (getID (getPrimerPregunta listaPreguntas)) IDPregunta)
                                        (addPregunta (setRespuestas (getPrimerPregunta listaPreguntas) (nuevaListaRespuestas (getRespuestas(getPrimerPregunta listaPreguntas))
                                                                                                                             Fecha Respuesta tags (getNombreActivo stack))) (getSigPreg listaPreguntas))
                                        (addPregunta (getPrimerPregunta listaPreguntas) (AgregarRespuestaNueva stack (getSigPreg listaPreguntas) Fecha IDPregunta Respuesta tags))
                                        )
                                    )
                                )
  )

;Dominio: ListaPreguntas X string X int X int
;Recorrido: Boolean
;Descripcion: Funcion que retorna true si la respuesta de la pregunta que se esta buscando existe, si una de estas dos no existe retorna false
;Recursion: de cola
(define ExistePregunta?(lambda (listaPreguntas username IDPregunta IDRespuesta)
                         (if (null? (getPrimerPregunta listaPreguntas))
                             false
                             (if (and (eq? username (getAutor (getPrimerPregunta listaPreguntas))) (= IDPregunta (getID (getPrimerPregunta listaPreguntas))))
                                 (ExisteRespuesta? (getRespuestas (getPrimerPregunta listaPreguntas)) IDRespuesta)
                                 (ExistePregunta? (getSigPreg listaPreguntas) username IDPregunta IDRespuesta))
                             )
                         )
  )
                         
;Dominio: ListaRespuestas X int
;Recorrido: Boolean
;Descripcion: Funcion que retorna true si la respuesta que se esta buscando en la lista de respuestas se encuentra, caso contrario retorna false, esta funcion es utilizada en ExistePregunta?
;Recursion: de cola
(define ExisteRespuesta?(lambda (listaRespuestas IDRespuesta)
                          (if (null? (getPrimerResp listaRespuestas))
                              false
                              (if (= IDRespuesta (getIDresp (getPrimerResp listaRespuestas)))
                                  true
                                  (ExisteRespuesta? (getSigResp listaRespuestas) IDRespuesta))
                              )
                          )
  )

;Dominio: ListaPreguntas X int X int
;Recorrido: string
;Descripcion: Funcion que busca el nombre del autor de una respuesta a una pregunta en concreto.
;Recursion: de cola
(define conseguirNombreResp (lambda (listaPreguntas IDPregunta IDRespuesta)
                              (if (null? (getPrimerPregunta listaPreguntas))
                                  ""
                                  (if (= IDPregunta (getID (getPrimerPregunta listaPreguntas)))
                                      (conseguirAutor (getRespuestas (getPrimerPregunta listaPreguntas)) IDRespuesta)
                                      (conseguirNombreResp (getSigPreg listaPreguntas) IDPregunta IDRespuesta))
                                  )
                              )
  )

;Dominio: ListaRespuestas X int
;Recorrido: string
;Descripcion: Funcion que busca el nombre del autor de una respuesta en concreto, esta retorna el nombre del autor.
;Recursion: de cola
(define conseguirAutor (lambda (listaRespuestas IDRespuesta)
                         (if (null? (getPrimerResp listaRespuestas))
                             ""
                             (if (= IDRespuesta (getIDresp (getPrimerResp listaRespuestas)))
                                 (getRautor (getPrimerResp listaRespuestas))
                                 (conseguirAutor (getSigResp listaRespuestas) IDRespuesta))
                             )
                         )
  )


;Dominio: ListaPreguntas X int
;Recorrido: int
;Descripcion: Funcion devuelve el valor de recompensa a una pregunta exacta.
;Recursion: de cola
(define conseguirRecompensaP (lambda (listaPreguntas IDPregunta)
                               (if (null? (getPrimerPregunta listaPreguntas))
                                   0
                                   (if (= IDPregunta (getID (getPrimerPregunta listaPreguntas)))
                                       (getRecom (getPrimerPregunta listaPreguntas))
                                       (conseguirRecompensaP (getSigPreg listaPreguntas) IDPregunta))
                                   )
                               )
  )

;Dominio: ListaPreguntas X int X int
;Recorrido: ListaPreguntas
;Descripcion: Funcion que actualiza el estado de una pregunta a resuelta, retorna la lista de preguntas con la pregunta actualizada.
;Recursion: natural
(define preguntasActEstado (lambda (listaPreguntas  IDPregunta IDRespuesta)
                             (if (preguntasVacio? listaPreguntas)
                                 listaPreguntas
                                 (if (= (getID (getPrimerPregunta listaPreguntas)) IDPregunta)
                                     (addPregunta (setRespuestaAceptada (getPrimerPregunta listaPreguntas) IDRespuesta) (getSigPreg listaPreguntas))
                                     (addPregunta (getPrimerPregunta listaPreguntas) (preguntasActEstado (getSigPreg listaPreguntas) IDPregunta IDRespuesta))
                                     )
                                 )
                             )
  )

;Dominio: ListaUsuarios X string X int
;Recorrido: ListaUsuarios
;Descripcion: Funcion que actualiza los puntos de recompensa de un usario sumandole los de recompensa, retorna toda la lista de usuarios con el usuario actualizado.
;Recursion: natural
(define usuariosActRecompensa(lambda (listaUsuarios username recompensa)
                               (if (usuariosVacio? listaUsuarios)
                                   listaUsuarios
                                   (if (equal? (getUsuario (getPrimerUser listaUsuarios)) username)
                                       (agregarUsuario (usuario username (getPassword(getPrimerUser listaUsuarios)) (+ (getPoints (getPrimerUser listaUsuarios)) recompensa)) (getSigUser listaUsuarios))
                                       (agregarUsuario (getPrimerUser listaUsuarios) (usuariosActRecompensa (getSigUser listaUsuarios) username recompensa))
                                       )
                                   )
                               )
  )
                              
;---------------------------
;Selectores de los tags
(define (getPrimerTag tags)
  (if (not (null? tags))
      (car tags)
      null)
  )
(define (getSigTag tags)
  (if (not (null? tags))
      (cdr tags)
      null)
  )
;---------------------------

;Funciones vote
;Dominio: ListaPreguntas
;Recorrido: pregunta
;Descripcion: Funcion selectora que rectorna una pregunta.
;Recursion: de cola (debido a la funcion auxGetQuestion).
(define getQuestion (lambda (listaPreguntas)
                      (lambda (IDPregunta)
                        (auxGetQuestion listaPreguntas IDPregunta)
                        )
                      )
  )

;Dominio: ListaPreguntas X int
;Recorrido: pregunta
;Descripcion: Funcion que retorna la pregunta que se esta buscando por el id.
;Recursion: de cola
(define auxGetQuestion (lambda (listaPreguntas IDPregunta)
                         (if (null? (getPrimerPregunta listaPreguntas))
                             null
                             (if (= IDPregunta (getID (getPrimerPregunta listaPreguntas)))
                                 (getPrimerPregunta listaPreguntas)
                                 (auxGetQuestion (getSigPreg listaPreguntas) IDPregunta ))
                             )
                         )
  )

;Dominio: int
;Recorrido: respuesta
;Descripcion: Funcion que retorna la respuesta de la pregunta que se esta buscando.
;Recursion: de cola (debido a la funcion auxGetAnswer). 
(define getAnswer(lambda (IDPregunta)
                   (lambda (listaPreguntas)
                     (lambda (IDRespuesta)
                       (if (= -4000 IDRespuesta);Forzar que retorne el IDPregunta ya que no se tiene como saber la ID de la pregunta
                           IDPregunta
                           (auxGetAnswer listaPreguntas IDPregunta IDRespuesta))
                       )
                     )
                   )
  )

;Dominio: ListaPreguntas X int X int
;Recorrido: respuesta
;Descripcion: Funcion que retorna la respuesta que se esta buscando pero primero se busca la pregunta donde se encuentra la respuesta.
;Recursion: de cola (la funcion respuestaVotar tambien es de cola).
(define auxGetAnswer (lambda (listaPreguntas IDPregunta IDRespuesta)
                       (if (null? (getPrimerPregunta listaPreguntas))
                           null
                           (if (= IDPregunta (getID (getPrimerPregunta listaPreguntas)))
                               (respuestaVotar (getRespuestas (getPrimerPregunta listaPreguntas)) IDRespuesta)
                               (auxGetAnswer (getSigPreg listaPreguntas) IDPregunta IDRespuesta))
                           )
                       )
  )

;Dominio: ListaRespuestas X int
;Recorrido: respuesta
;Descripcion: Funcion que retorna que se esta buscando.
;Recursion: de cola.
(define respuestaVotar(lambda (listaRespuestas IDRespuesta)
                        (if (null? (getPrimerResp listaRespuestas))
                            null
                            (if (= IDRespuesta (getIDresp (getPrimerResp listaRespuestas)))
                                (getPrimerResp listaRespuestas)
                                (respuestaVotar (getSigResp listaRespuestas) IDRespuesta))
                            )
                        )
  )

;Dominio: ListaPreguntas X pregunta X Boolean
;Recorrido: ListaPreguntas
;Descripcion: Funcion que agrega un voto en contra o a favor a la pregunta ingresada, retorna la lista de preguntas actualizada.
;Recursion: natural
(define agregarVotoPregunta (lambda (listaPreguntas  preg voto)
                              (if (preguntasVacio? listaPreguntas)
                                  listaPreguntas
                                  (if (equal? preg (getPrimerPregunta listaPreguntas))
                                      (if (eq? voto true);Si es true agregar voto VF, caso contrario agregar voto VC
                                          (addPregunta (setVF (getPrimerPregunta listaPreguntas)) (getSigPreg listaPreguntas))
                                          (addPregunta (setVC (getPrimerPregunta listaPreguntas)) (getSigPreg listaPreguntas)))
                                      (addPregunta (getPrimerPregunta listaPreguntas) (agregarVotoPregunta (getSigPreg listaPreguntas) preg voto))
                                      )
                                  )
                              )
  )

;Dominio: ListaPreguntas X int X respuesta X Boolean
;Recorrido: ListaPreguntas
;Descripcion: Funcion que recorre las preguntas hasta encontrar la respuesta en donde se quiere agregar el voto a la respuesta ingresada, retorna la lista de preguntas actualizada.
;Recursion: natural
(define agregarVotoRespuesta (lambda (listaPreguntas IDPregunta resp voto)
                               (if (preguntasVacio? listaPreguntas)
                                   listaPreguntas
                                   (if (= IDPregunta (getID (getPrimerPregunta listaPreguntas)))
                                       (addPregunta (setRespuestas (getPrimerPregunta listaPreguntas) (auxAgregarVotoRespuesta (getRespuestas(getPrimerPregunta listaPreguntas)) resp voto))
                                                    (getSigPreg listaPreguntas))
                                       (addPregunta (getPrimerPregunta listaPreguntas) (agregarVotoRespuesta (getSigPreg listaPreguntas) IDPregunta resp voto))
                                       )
                                   )
                               )
  )



;Dominio: ListaRespuestas X respuesta X Boolean
;Recorrido: ListaRespuestas
;Descripcion: Funcion que recorre la lista de respuesta y agrega un voto a la pregunta ingresada segun el booleando ingresado, retorna la lista de respuesta actualizada.
;Recursion: natural
(define auxAgregarVotoRespuesta(lambda (listaRespuestas resp voto)
                                 (if (null? (getPrimerResp listaRespuestas))
                                     false
                                     (if (equal? resp (getPrimerResp listaRespuestas))
                                         (if (eq? voto true)
                                             (agregarRespuesta (setVFr (getPrimerResp listaRespuestas)) (getSigResp listaRespuestas))
                                             (agregarRespuesta (setVCr (getPrimerResp listaRespuestas)) (getSigResp listaRespuestas)))
                                         (agregarRespuesta (getPrimerResp listaRespuestas) (auxAgregarVotoRespuesta (getSigResp listaRespuestas) resp voto))
                                         )
                                     )
                                 )
  )
#lang racket

(provide (all-defined-out))

;TDA usuario
;Representacion: una lista ("username" "password" puntos)

;Constructor
;Dominio: string X string X int
;Recorrido: usuario
;Descripcion: Permite crear un usuario
(define usuario(lambda (username password points)
                 (if(and (string? username)(string? password) (integer? points));Comprobaciones del tipo de dato
                    (list username password points) 
                    null
                    )
                 )
  )
;Pertenencia
;Dominio: usuario
;Recorrido: Boolean
;Descripcion: Funcion que verifica si es una lista o no,
;si lo es comprueba si su largo es 3 y luego si los elementos de esta no son nulos, regresa true si se cumple todo, false si no
(define (usuario? user)
  (if (list? user)
      (if (and (= (length user) 3)
               (not (null? (getUsuario user)))
               (not (null? (getPassword user)))
               (not (null? (getPoints user)))
               )
          true
          false)
      false)
  )

;Selectores
(define (getUsuario user) (car user))
(define (getPassword user) (cadr user))
(define (getPoints user) (caddr user))

;Modificadores
;Dominio: usuario X int
;Recorrido: usuario
;Descripcion: Modifica los puntos del usuario segun la cantidad ingresada a la funcion, retorna al usuario con puntos modificados
(define (setPoints user newPoints)
  (if (and (usuario? user) (integer? newPoints))
      (usuario (getUsuario user) (getPassword user) newPoints)
      user
      )
  )

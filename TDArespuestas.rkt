#lang racket

(require"TDAPregunta.rkt")
(provide (all-defined-out))

;Respuesta
;Constructor
;Dominio: fecha X string X list X string X int
;Recorrido: respuesta
;Descripcion: Funcion que crea una respuesta, se comprueba si los datos son correctos, si lo son se crea la lista con todos estos, si no null
(define (respuesta F resp etiq autor IDresp VF VC)
  (if (and (date? F)
           (string? resp)
           (list? etiq)
           (string? autor)
           (integer? IDresp)
           (integer? VF)
           (integer? VC))
      (list F resp etiq autor IDresp VF VC)
      null
      )
  )

;Pertenencia
;Dominio: respuesta
;Recorrido: Boolean
;Descripcion: Se comprueba si el largo de la lista es 5, si lo es se comprueba si los elementos de esta no son nulos, si no lo son entonces retorna true, caso contrario retorna false
(define (respuesta? R)
  (if (= (length R) 7)
      (if (and (not (null? (getRfecha R)))
               (not (null? (getResp R)))
               (not (null? (getRautor R)))
               (not (null? (getIDresp R)))
               (not (null? (getVFR R)))
               (not (null? (getVCR R)))
               )
          true
          false)
      false)
  )

;Selectores
(define (getRfecha R) (car R))
(define (getResp R) (cadr R))
(define (getRetiq R) (caddr R))
(define (getRautor R) (cadddr R))
(define (getIDresp R) (cadddr (cdr R)))
(define (getVFR R) (cadddr (cddr R)))
(define (getVCR R) (cadddr (cdddr R)))


;Modificadores
;Dominio: respuesta
;Recorrido: respuesta
;Descripcion: Funcion que agrega un voto segun cual sea la funcion, retorna la respuesta actualizada.
(define (setVFr R)
  (respuesta (getRfecha R) (getResp R) (getRetiq R) (getRautor R) (getIDresp R) (+ 1 (getVFR R)) (getVCR R)))

(define (setVCr R)
  (respuesta (getRfecha R) (getResp R) (getRetiq R) (getRautor R) (getIDresp R) (getVFR R) (+ 1 (getVCR R))))

;--------------------------------------------------------------------------------------------
;TDA Respuestas
(define respuestasVacia null) ;Creacion de una lista vacia
(define respuestasVacia? null?) ;Comprobar si la lista de respuesta es vacia

;Dominio: respuestas
;Recorrido: Boolean
;Descripcion: Comprobar si E es una lista, luego si todos los elementos de la lista son una respuesta, si lo son retorna true, caso contrario false
(define respuestas? (lambda (E) (and (list? E) (andmap respuesta? E))))
(define agregarRespuesta cons)

;Modificadores
;Dominio: respuestas X respuesta
;Recorrido: respuestas
;Descripcion: Funcion que agregar la nueva respuesta en la cola de respuestas
;Recursion: Natural
(define addRespuesta (lambda (RS Fecha Respuesta tags autor ID VF VC)
                       (if (respuestasVacia? RS)
                           (agregarRespuesta (respuesta Fecha Respuesta tags autor ID VF VC) RS)
                           (agregarRespuesta (getPrimerResp RS) (addRespuesta(getSigResp RS) Fecha Respuesta tags autor (+ 1 ID) VF VC)))
                       )
                       
  )

;Selectores
(define (getPrimerResp RS)
  (if (and (respuestas? RS) (not (null? RS)))
      (car RS)
      null)
  )
(define (getSigResp RS)
  (if (and (respuestas? RS) (not (null? RS)))
      (cdr RS)
      null)
  )

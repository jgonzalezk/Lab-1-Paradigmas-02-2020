#lang racket

(provide (all-defined-out))

;TDA Pregunta
;Representacion: una lista (id "pregunta" '(fecha) votosFavor votosContra '(etiquetas) recompensa vistas estado autor '(respuestas) IDrespuestaAceptada) 

;Constructor
;Dominio: int X string X date X int X int X list X int X int X int X string X list X int
;Recorrido: pregunta
;Descripcion: Funcion que crea una pregunta con todos los datos que estan en la representacion
(define (pregunta id Pregunta Fecha votosF votosC etiquetas recompensa vistas estado autor Respuestas Raceptada)
  (if (and (integer? id);Comprobaciones del tipo de dato
           (string? Pregunta)
           (date? Fecha)
           (integer? votosF)
           (integer? votosC)
           (list? etiquetas);QUIZAS ESTA COMPROBACION NO VA
           (integer? recompensa)
           (integer? vistas)
           (integer? estado)
           (string? autor)
           (list? Respuestas)
           (integer? Raceptada))
      (list id Pregunta Fecha votosF votosC etiquetas recompensa vistas estado autor Respuestas Raceptada)
      null
      )
  )

;Pertenencia
;Dominio: pregunta
;Recorrido: Boolean
;Descripcion: Se verificar si P es una lista, si lo es se procede a verificar si los parametros no son nulos, devuelve un true si
;cumplen las condiciones, si no retorna false
(define (pregunta? P)
  (if (list? P)
      (if (and (= (length P) 12)
               (not (null? (getID P)))
               (not (null?  (getPreg P)))
               (not (null?  (getFecha P)))
               (not (null?  (getVF P)))
               (not (null?  (getVC P)))
               ;(not (null?  (getEtiq P))) Esta condicion no vale debido a que puede que no tenga etiquetas
               (not (null?  (getRecom P)))
               (not (null?  (getVistas P)))
               (not (null?  (getEstado P)))
               (not (null?  (getAutor P)))
               );No se comprueba si no es nulo respuestas, debido a que puede que no tenga respuestas aun, lo mismo con Raceptada
          true
          false
          )
      false)
  )

;Selectores
(define (getID P) (car P))
(define (getPreg P) (cadr P))
(define (getFecha P) (caddr P))
(define (getVF P) (cadddr P))
(define (getVC P) (cadddr (cdr P)))
(define (getEtiq P) (cadddr (cddr P)))
(define (getRecom P) (cadddr (cdddr P)))
(define (getVistas P) (cadddr (cdddr (cdr P))))
(define (getEstado P) (cadddr (cdddr (cddr P))))
(define (getAutor P) (cadddr (cdddr (cdddr P))))
(define (getRespuestas P) (cadddr (cdddr (cdddr (cdr P)))))
(define (getRaceptada P) (cadddr (cdddr (cdddr (cddr P)))))

;Modificadores
;Dominio: pregunta
;Recorrido: pregunta
;Descripcion: Se aumenta en uno el valor que se quiere cambiar y se devuelve la pregunta
(define (setVF P)
  (pregunta (getID P) (getPreg P) (getFecha P) (+ (getVF P) 1) (getVC P) (getEtiq P)
            (getRecom P) (getVistas P) (getEstado P) (getAutor P) (getRespuestas P) (getRaceptada P))
  )

(define (setVC P)
  (pregunta (getID P) (getPreg P) (getFecha P) (getVF P) (+ (getVC P) 1) (getEtiq P)
            (getRecom P) (getVistas P) (getEstado P) (getAutor P) (getRespuestas P) (getRaceptada P))
  )

(define (setVistas P)
  (pregunta (getID P) (getPreg P) (getFecha P) (getVF P) (getVC P) (getEtiq P)
            (getRecom P) (+ (getVistas P) 1) (getEstado P) (getAutor P) (getRespuestas P) (getRaceptada P))
  )

(define (setEstado P)
  (pregunta (getID P) (getPreg P) (getFecha P) (getVF P) (getVC P) (getEtiq P)
            (getRecom P) (getVistas P) (+ (getEstado P) 1) (getAutor P) (getRespuestas P) (getRaceptada P))
  )

(define (setRecompensa P Recom)
  (pregunta (getID P) (getPreg P) (getFecha P) (getVF P) (getVC P) (getEtiq P)
            Recom (getVistas P) (getEstado P) (getAutor P) (getRespuestas P) (getRaceptada P))
  )

(define (setRespuestaAceptada P IDRespuesta)
  (pregunta (getID P) (getPreg P) (getFecha P) (getVF P) (getVC P) (getEtiq P)
            0 (getVistas P) 1 (getAutor P) (getRespuestas P) IDRespuesta)
  )

;Dominio: pregunta X respuestas
;Recorrido: pregunta
;Descripcion: Se actualizan las antiguas respuestas por las nuevas y se devuelve la pregunta
(define (setRespuestas P newRespuestas)
  (pregunta (getID P) (getPreg P) (getFecha P) (getVF P) (getVC P) (getEtiq P)
            (getRecom P) (+ (getVistas P) 1) (getEstado P) (getAutor P) newRespuestas (getRaceptada P))
  )
;------------------------------------------------------------------------------------------------
;TDA Fecha
;Constructor
;Dominio: int X int X int
;Recorrido: date
;Descripcion: se crea una lista con los valores de la fecha
(define (date dia mes year)
  (list dia mes year)
  )

;Pertenencia
;Dominio: date
;Recorrido: Boolean
;Descripcion: Comprueba si F ingresado es una lista de 3 elementos, si lo es comprueba si los elementos de esta son enteros y estan dentro del rango de una fecha
;, si lo son retorna true si no false
(define (date? F)
  (if (= (length F) 3)
      (if (and (and (integer? (getDia F)) (<= (getDia F) 31))
               (and (integer? (getMes F)) (<= (getMes F) 12))
               (integer? (getYear F)))
          true
          false)
      false)
  )

;Selectores
(define (getDia F) (car F))
(define (getMes F) (cadr F))
(define (getYear F) (caddr F))
;------------------------------------------------------------------------------------------------

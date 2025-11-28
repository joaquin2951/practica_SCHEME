; '(seleccion1 (lstJugadoresQueMarcaron) seleccion2 (lstJugadoresQueMarcaron) "26/06/2004")

(define grupo-b
  '(
    (ecuador (sarmiento) venezuela (cadiz bello) "22/06/2024")
    (mexico (arteaga) jamaica () "22/06/2024")
    (ecuador (palmer minda palmer) jamaica () "26/06/2024")
    (venezuela (rondon) mexico () "26/06/2024")
    (mexico () ecuador () "30/06/2024")
    (jamaica () venezuela (bello rondon ramirez) "30/06/2024")
    )
  )

(define golesSeleccion1 (lambda (partido) (length (cadr partido))))
(define golesSeleccion2 (lambda (partido) (length (cadddr partido))))
(define nombreSeleccion1(lambda (partido) (car partido)))
(define nombreSeleccion2(lambda (partido) (caddr partido)))


(define empataron?
  (lambda (partido)
    (= (golesSeleccion1 partido) (golesSeleccion2 partido))
    )
  )

;a)
(define cantidad-empates
  (lambda (lista-juegos)
    (if (null? lista-juegos) 0
        (if (empataron? (car lista-juegos)) (+ 1 (cantidad-empates (cdr lista-juegos)) )
            (cantidad-empates (cdr lista-juegos))
            ))))
    
;b)
(define esSeleccion1? (lambda (seleccion partido) (equal? seleccion (nombreSeleccion1 partido)) ))
(define esSeleccion2? (lambda (seleccion partido) (equal? seleccion (nombreSeleccion2 partido)) ))

(define goles-convertidos
  (lambda (seleccion lista-juegos)
    (cond
         ( (null? lista-juegos) 0 )
         ( (esSeleccion1? seleccion (car lista-juegos))  (+ (golesSeleccion1 (car lista-juegos)) (goles-convertidos seleccion (cdr lista-juegos))) )
         ( (esSeleccion2? seleccion (car lista-juegos))  (+ (golesSeleccion2 (car lista-juegos)) (goles-convertidos seleccion (cdr lista-juegos))) )
         ( else (goles-convertidos seleccion (cdr lista-juegos)) )
         )))

;c)
(define goleadoresSeleccion1 (lambda (partido) (cadr partido)))
(define goleadoresSeleccion2 (lambda (partido) (cadddr partido)))

(define (goleadores seleccion lista-juegos)
  (cond
        (  (null? lista-juegos) '()  )
        (  (esSeleccion1? seleccion (car lista-juegos))  (append (goleadoresSeleccion1 (car lista-juegos)) (goleadores seleccion (cdr lista-juegos)))  )
        (  (esSeleccion2? seleccion (car lista-juegos))  (append (goleadoresSeleccion2 (car lista-juegos)) (goleadores seleccion (cdr lista-juegos)))  )
        ( else (goleadores seleccion (cdr lista-juegos)) )
        ))

;d)
(define (lista-rivales lista-juegos)
  (lambda (seleccion)
    (cond
          (  (null? lista-juegos)  '() )
          (  (esSeleccion1? seleccion (car lista-juegos)) (cons (nombreSeleccion2 (car lista-juegos)) ( (lista-rivales (cdr lista-juegos)) seleccion ))  )
          (  (esSeleccion2? seleccion (car lista-juegos)) (cons (nombreSeleccion1 (car lista-juegos)) ( (lista-rivales (cdr lista-juegos)) seleccion ))  )
          ( else ( (lista-rivales (cdr lista-juegos)) seleccion) )
          )))
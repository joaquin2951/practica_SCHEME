
;a)

;(x y)

(define (obtenerX punto) (car punto))
(define (obtenerY punto) (cadr punto))


(define (distancia P1 P2)
      (sqrt (+ (expt (- (obtenerX P2) (obtenerX P1) ) 2) (expt (- (obtenerY P2) (obtenerY P1) ) 2)))
  )

; (distancia '(1 2) '(1 -1))
; (distancia '(1 2) '(4 -2))
; (distancia '(1 2) '(6 1))


;b)

(define masCercano (lambda (P)
        (lambda (P1 P2)
          (< (distancia P1 P) (distancia P2 P) )
          )))

;(masCercano '(1 2))
;( (masCercano '(1 2) ) '(1 -1) '(6 1) )
;( (masCercano '(1 2) ) '(4 -2) '(1 -1) )


;c)

(define ordenados?
    (lambda (P lista-puntos)
      (cond
            (  (null? (cdr lista-puntos)) #t )
            (  ( (masCercano P) (car lista-puntos) (cadr lista-puntos) ) (ordenados? P (cdr lista-puntos)) )
            ( else #f )
            )))
; (ordenados? '(1 2) '( (1 -1) (4 -2) (6 1) ) )
; (ordenados? '(1 2) '( (6 1) (4 -2) ) )
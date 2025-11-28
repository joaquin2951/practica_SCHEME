;Para representar una matriz en Scheme se utilizará una lista de listas, en donde cada sublista 
;representa una fila de la matriz. 
;Se solicita desarrollar las siguientes funciones:

;a) (fila matriz n), cuyo resultado es una lista que contiene los elementos de la fila enésima de la 
;matriz pasada como argumento.

;b) (columna matriz n), cuyo resultado es una lista que contiene los elementos de la columna 
;enésima de la matriz pasada como argumento.

;c) (cuadradoMagico matriz), cuyo resultado es un valor booleano el cual será verdadero si la 
;matriz corresponde a un “cuadrado mágico”. Para ser un cuadrado mágico la matriz debe ser una 
;matriz cuadrada y cumplir la condición que la suma  de los elementos de cada fila y columna con 
;igual subíndice sea equivalente. Por ejemplo, sea la matriz siguiente:

(define matriz '( (3 5 1)
                  (4 6 3)
                  (2 2 8) ) )

;a)
(define (fila matriz n)
  (if (= 1 n) (car matriz)
      (fila (cdr matriz) (- n 1))))

;b)
(define (columna matriz n)
  (if (= 1 n) (map car matriz)
      (columna (map cdr matriz) (- n 1))
      ))

;c)
(define esCuadrada? (lambda (matriz) (= (length matriz) (length (car matriz)))))
(define sumaElementos
  (lambda (lst)
    (if (null? lst) 0
        (+ (car lst) (sumaElementos (cdr lst)))
        )))
                        
(define cuadradoMagico
  (lambda (matriz)
    (if (and (null? (cdr matriz)) (esCuadrada? matriz)) #t
      (if
        (and (esCuadrada? matriz) (= (sumaElementos (fila matriz 1)) (sumaElementos (columna matriz 1))))
             (esCuadrada? (map cdr (cdr matriz)) )
             #f
             ))))
             
             
    
                         
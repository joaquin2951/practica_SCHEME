;Ejercicio 3 
;Defina en Scheme la función  (potencias n x) que permita obtener la lista de potencias de un 
;número n menores a un límite x. 
;Por ejemplo, si n = 2 y x = 325, la función debe retornar la lista conteniendo 2´0, 2´1, 2´2, 2´3, ... y 
;2'8. El número 2'9 no será incluido pues es mayor que 325. 
; 
;>(potencias 2 325) 
; (1 2 4 8 16 32 64 128 256)
;>(potencias 5 724)
;(1 5 25 125 625)

(define (potenciasAux n x contador)
  (if (> (expt n contador) x) '()
      (cons (expt n contador) (potenciasAux n x (+ contador 1)))
      ))

(define (potencias n x)
  (potenciasAux n x 0))
  
   
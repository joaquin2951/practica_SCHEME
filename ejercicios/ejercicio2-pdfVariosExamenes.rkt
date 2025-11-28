;La bases de datos sobre actividades de personas pueden representarse mediante listas de elementos de la 
;forma (a b c d), donde a es el nombre de la persona, b su actividad, c su fecha de nacimiento y d la de su 
;fallecimiento. Un ejemplo es la siguiente lista que usaremos a lo largo de los siguientes ejercicios:

(define personas '(("Cervantes" "Literatura" 1547 1616)                 
                   ("Velazquez" "Pintura" 1599 1660) 
                   ("Picasso" "Pintura" 1881 1973) 
                   ("Beethoven" "Musica" 1770 1823) 
                   ("Poincare" "Ciencia" 1854 1912) 
                   ("Quevedo" "Literatura" 1580 1654) 
                   ("Goya" "Pintura" 1746 1828) 
                   ("Einstein" "Ciencia" 1879 1955) 
                   ("Mozart" "Musica" 1756 1791) 
                   ("Botticelli" "Pintura" 1445 1510) 
                   ("Borromini" "Arquitectura" 1599 1667) 
                   ("Bach" "Musica" 1685 1750) 
                  ))

(define nombrePersona (lambda(lst) (car lst)))
(define actividadPersona (lambda(lst) (cadr lst)))
(define fechaNacPersona (lambda(lst) (caddr lst)))
(define fechaFallecPersona (lambda(lst) (cadddr lst)))

;a) Definir la función nombres tal que (nombres bd) es la lista de los nombres de las personas de la base de 
;datos bd. Por ejemplo, 
;> (nombres personas) 
;("Cervantes" "Velazquez" "Picasso" "Beethoven" "Poincare" "Quevedo" "Goya" 
;"Einstein" "Mozart" "Botticelli" "Borromini" "Bach")

(define (nombres bd)
  (if (null? bd) '()
      (cons (nombrePersona (car bd)) (nombres (cdr bd)))
      ))

;b) Definir la función pintores tal que (pintores bd) es la lista de los nombres de los pintores de la base de 
;datos bd. Por ejemplo, 
;> (pintores personas) 
;("Velazquez" "Picasso" "Goya" "Botticelli")

(define esPintor? (lambda (persona) (equal? "Pintura" (actividadPersona persona))))

(define filtro
  (lambda (condicion lst)
    (if (null? lst) '()
      (if (condicion (car lst)) (cons (car lst) (filtro condicion (cdr lst)))
          (filtro condicion (cdr lst))
          ))))

  (define pintores
  (lambda (bd)
    (nombres (filtro esPintor? bd))))


;c) Definir la función de orden superior selecciona tal que al evaluar (selecciona bd) se obtiene una función 
;de 1 argumento que al ser evaluada con una actividad A como parámetro devuelve los nombres de las 
;personas que realizan la actividad A. Por ejemplo,  
;> (selecciona personas) 
;#<procedure:...func20151214.rkt:32:21> 
;> ((selecciona personas) "Musica") 
;("Beethoven" "Mozart" "Bach") 
;> ((selecciona personas) "Literatura") 
;("Cervantes" "Quevedo")


(define (selecciona bd)
  (lambda (act)
    (nombres (filtro (lambda(x) (equal? act (actividadPersona x) )) bd))
    ))


;d) Redefinir la función pintores del inciso b), utilizando la función de orden superior del inciso c).

(define pintores2
  (lambda(bd)
    ( (selecciona bd) "Pintura" ) ))


;e) Definir la función vivas tal que (vivas bd a) es la lista de los nombres de las personas de la base de datos 
;bd que estaban vivas en el año a. Por ejemplo: 
;> (vivas personas 1600) 
;("Cervantes" "Velazquez" "Quevedo" "Borromini")

(define estabaViva? (lambda (persona anio)
                      (and (<= (fechaNacPersona persona) anio) (>= (fechaFallecPersona persona) anio))))
(define (vivas bd a)
  (if (null? bd) '()
      (if (estabaViva? (car bd) a) (cons (nombrePersona (car bd)) (vivas (cdr bd) a))
          (vivas (cdr bd) a)
          )))



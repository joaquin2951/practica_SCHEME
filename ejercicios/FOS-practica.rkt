;FUNCIONES DE ORDEN SUPERIOR - PRACTICA

(define cuadrado
  (lambda(X) (* X X)))

(define cubo
  (lambda(X) (* X X X)))

;(define listaF1 '(cubo cuadrado)) MAL!!!!! -> esta definicion esta mal porque el QUOTE hace que cubo y cuadrado se guarden como LITERALES (SYMBOL)
;y NO como funciones

(define listaF1 (list cubo cuadrado)) ;BIEN!!!


(define combina (lambda(funcion n casoBase)
        (if (= n 0) casoBase
            (funcion n (combina funcion (- n 1) casoBase)))))

(define sumatoria
     (lambda(n) (combina + n 0)))

(define factorial
     (lambda(n) (combina * n 1)))

;----------------------------------------


(define mayor5 (lambda(X) (> X 5)))
(define increm (lambda(X) (+ X 1)))

(define aplicaSi
  (lambda(condicion funcionAaplicar)
    (lambda(lst)
          (if (null? lst) '()
              (if (condicion (car lst)) (cons (funcionAaplicar (car lst)) ((aplicaSi condicion funcionAaplicar) (cdr lst)) )
                  (cons (car lst) ((aplicaSi condicion funcionAaplicar) (cdr lst))))))))

(define composicion
(lambda(f g)
     (lambda (x)(f(g x))) ; LA LLAMADA (composicion funcion1 funcion2) RETORNA ESTA FUNCION
 ))



(define mapeo
  (lambda(funcion lst)
    (if (null? lst) '()
        (cons (funcion (car lst)) (mapeo funcion (cdr lst))))))
      
;(mapeo car '((1 2 3) (4 5 6) (a b c)))
;(mapeo increm '(1 2 3 4 5))

;-------------------------------------
(define atom
  (lambda (x)
    (if (and (not (null? x)) (not (pair? x)) ) #t
        #f))) 
         

(define filter
  (lambda (funcion lst)
    (if (null? lst) lst
        (if (funcion (car lst)) (cons (car lst) (filter funcion (cdr lst)))
            (filter funcion (cdr lst))))))

;(filter atom '('(1 2 3) (2 3) 4 (12 3) () 9 () 2 "hola" 'hola () ))   -->  (4 9 2 "hola")



;-----------------------

;Defina la función (aplica-mapeo LFunciones Lista) que 
;devuelve una lista que resulta de aplicar cada una de las 
;funciones que son miembros de LFunciones a cada 
;elemento de Lista.
 (define suma-1 (lambda (x) (+ x 1)))
 (define resta-1 (lambda (x) (-x 1))) 
 (define por-2 (lambda (x) (* x 2)))

(define aplica-mapeo (lambda (LFunciones Lista)
                       (if (null? LFunciones) LFunciones
                           (cons (mapeo (car LFunciones) Lista) (aplica-mapeo (cdr LFunciones) Lista))
                           )))



;Defina la función (aplica-filtro listaFiltros) que retorne una 
;función cuyo argumento es una lista. En este caso, listaFiltros es 
;una lista de funciones de un argumento que devuelven verdadero 
;o falso. Al evaluar la función resultante con una lista específica 
;se deben aplicar todos los filtros de la lista de funciones para 
;obtener la lista resultante.
(define mayor2 (lambda(X) (> X 2)))
(define menor4 (lambda(X) (< X 4)))

(define aplica-filtro (lambda (listaFiltros)
                        (lambda(lista)
                          (if (null? listaFiltros) lista
                             ( (aplica-filtro (cdr listaFiltros)) (filter (car listaFiltros) lista))
                          ))))


;Defina una función de orden superior que recibe como parámetros una 
;función (que retorna verdadero o falso) y una lista.
;La función (take-while condicion L) devuelve una lista con los n 
;primeros elementos de la lista L para los que condicion retorne 
;verdadero. Donde n es la posición del primer elemento para el que la 
;evaluación de condicion retorne falso.



(define (take-while condicion L)
        (if (null? L) L
            (if (condicion (car L)) (cons (car L) (take-while condicion (cdr L)))
                '()
             )))

;EJEMPLOS
;(take-while mayor2 '(5 6 7 2 8))
; (5 6 7)
; (take-while mayor2 '(5 6 7 9 8))
; (5 6 7 9 8)
; (take-while mayor2 '(1 2 3 3 2))
; ()



;De manera similar al punto anterior, defina la función 
;(drop-while condition L)
; que devuelve la lista L a la que se le eliminaron los primeros elementos 
;que satisfacen la condición (esto es aquellos elementos que al aplicarle 
;la función condition dan verdadero).

(define (drop-while condition L)
  (if (null? L) L
      (if (condition (car L)) (drop-while condition (cdr L))
            L
          )))

;EJEMPLOS:
;(drop-while menor4 '(2 3 1 5 6 ))
; (5 6)
; (drop-while menor4 '(2 3 1 5 6 1))
; (5 6 1)
; (drop-while menor4 '(5 6 1 2 3))  
;(5 6 1 2 3)




;Dada una función de un argumento f y un entero positivo n, se le solicita 
;que defina la función de orden superior (repetir f n), la cual retorna una 
;función de un argumento que aplica n veces la función f
(define cua (lambda (x) (* x x)))

  
(define repetir (lambda(f n)
                  (lambda(elemento)
                    (if (= 0 n) elemento
                        ( (repetir f (- n 1)) (f elemento) )
                        ))))
  
;  EJEMPLOS:
;  ((repetir cua 1) 2) 
; 4
; ((repetir cua 3) 2) 
; 256




;a) Se le solicita que defina la función Aplica de tres argumentos, una lista, una 
;función f y un número entero n , que dará como resultado la lista original pero 
;con el elemento que se encuentra en la posición n reemplazado por el 
;resultado de aplicar la función f al enésimo elemento.

(define Suma1 (lambda (x) (+ x 1)))

(define Aplica
  (lambda (f lista n)
    (if (null? lista) lista
        (if (= 1 n) (cons (f (car lista)) (cdr lista))
            (cons (car lista) (Aplica f (cdr lista) (- n 1)))
            ))))

;EJEMPLOS:
;(Aplica Suma1 '(2 4 9) 1)  -->  (3 4 9)
;(Aplica Suma1 '(2 4 9) 3)  -->  (2 4 10)



;b) Se le solicita que defina una función de dos argumentos, una lista y una 
;función f, aplica-all, que dará como resultado una lista de listas, en la cual el 
;enésimo elemento es la lista original en la cual se ha reemplazado su enésimo 
;elemento por el resultado de aplicar f al mismo.


........aún no realizado............

;EJEMPLOS:
;(define Suma1 (lambda (x) (+ x 1)))
; (aplica-all Suma1 ‘(2 4 9))
; ((3 4 9) (2 5 9) (2 4 10))
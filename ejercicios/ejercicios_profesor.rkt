(define longitud
(lambda(lista)
(if(null? lista)
0
(+ 1 (longitud (cdr lista)))
)
)
)

;(reversa '(1 2 3))
; (reversa '(1 '(2 3) 3))

(define reversaProfundidad
(lambda(lst)
(if (null? lst) lst
(append (reversaProfundidad (cdr lst))
(list (if (list? (car lst)) (reversaProfundidad (car lst)) (car lst)))
)
)
)
)



;Elimina un elemento de una lista
(define elimina
  (lambda(lst elemento)
   (if (null? lst) lst
    (if (= (car lst) elemento)
        (cdr lst)
        (append (list (car lst)) (elimina (cdr lst) elemento))
     )
    )
  )
)
; (elimina '(1 2 3) 2)

(define estaEnLista
  (lambda (lst elemento)
    (if (null? lst) #f
        (if (= (car lst) elemento) #t
            (estaEnLista (cdr lst) elemento)
            )
        )
    )
  )
;(estaEnLista '(1 2 3) 2)


;008. Elimina duplicados de una lista
(define eliminaDuplicados
(lambda(lst)
(if (null? lst) lst
(if (estaEnLista (cdr lst) (car lst))
    (eliminaDuplicados (append (list (car lst)) (elimina (cdr lst) (car lst))))
    (append (list (car lst)) (eliminaDuplicados (cdr lst)))
)
)
)
)
; (eliminaDuplicados '(1 2 3))



;26. Definir en Scheme una función productoPolinomio

(define agrega
(lambda (j lst)
(map (lambda(k) (cons j k)) lst)
)
)

(define productoPolinomio
(lambda(lst)
(cond ((null? lst) lst)
(null? (cdr lst) (map list (car lst) ) )
(else
(apply append
(map (lambda (k)
(map (lambda (x) (agrega k x))
(productoPolinomio (cdr lst)))
(car lst))
)
)))))

; 28. (moverAtras lst num) , num > 0
; Definir en Scheme, una función de dos argumentos, una lista y un número entero positivo n,
; de tal forma que al ser evaluada retorne como resultado la lista original,
; en la cual los primeros n elementos se han colocado al final de lista.
; Por ejemplo,
; (moverAtras '(a s d 3 4 5) 2) => (d 3 4 5 a s)
; (moverAtras '(d d 2 3 7 a a) 3) => (3 7 a a d d 2)


(define moverAtras
(lambda(lst num)
(if (= num 0) lst ;si
(moverAtras (append (cdr lst) (list (car lst))) (- num 1));else
)
)
)
; (moverAtras '(a s d 3 4 5) 2)


(define sacarNElementos
(lambda(lst N)
(if (= N 0) lst
(sacarNElementos (cdr lst) (- N 1))
)
)
)

(define primerosNElementos
(lambda (lst N)
(if (= N 0) '()
(append (list(car lst)) (primerosNElementos (cdr lst) (- N 1)))
)
)
)
; (primerosNElementos '(1 2 3 4 5 6) 2)



;009. Buscar y reemplazar elemento de lista
(define (reemplaza buscado reemplazante lst)
   ( cond ( (null? lst) lst)
          ( (= (car lst) buscado) (reemplaza buscado reemplazante (append (list reemplazante) (cdr lst)) ) )
          ( else (append (list (car lst)) (reemplaza buscado reemplazante (cdr lst)) ) )
    )
 )
;(reemplaza 2 4 '(1 2 3 4))


;010. Concatenar dos listas

(define (concatenar lst1 lst2)
   (if (null? lst1) lst2
        (cons (car lst1) (concatenar (cdr lst1) lst2))
    )
)
;(concatenar '(1 2 3) '(4 5 6))



;011. Aplana una lista anidada

(define atom?
  (lambda(x)
    (and (not (pair? x)) (not (null? x)))
    )
  )

(define aplana
  (lambda(lst)
          (cond ( (null? lst) lst)
                ( (atom? (car lst)) (append (list (car lst)) (aplana (cdr lst))) )
                ( (not(atom? (car lst))) (append (aplana (car lst)) (aplana (cdr lst)))  )
                 
          )
    )
 )
; (aplana '(1 2 3))
; (aplana '(1 2 (3 4 5) (6 7 8) 9 10 (11) (12 (13))))
; (aplana '(1 2 (3 (4 5) (6 (7 ((8) 9))))))



;012. Une dos listas tipo cierre de cremallera
;PRIMERA SOLUCION:
;(define zip2
;  (lambda(lst1 lst2)
;      (cond
;            ( (and (null? lst1) (null? lst2)) '() )
;            ( (null? lst2) (append (list (list (car lst1) '())) (zip2 (cdr lst1) lst2))  )
;            ( (null? lst1) (append (list (list '() (car lst2))) (zip2 lst1 (cdr lst2)))  )
;            ( else (append (list (list (car lst1) (car lst2))) (zip2 (cdr lst1) (cdr lst2))) )
;       )
;   )
; )

;SEGUNDA FORMA DE SOLUCIONARLO:
(define zip2
  (lambda(lst1 lst2)
      (cond
            ( (and (null? lst1) (null? lst2)) '() )
            ( (null? lst2) (cons (list (car lst1) '()) (zip2 (cdr lst1) lst2))  )
            ( (null? lst1) (cons (list '() (car lst2)) (zip2 lst1 (cdr lst2)))  )
            ( else (cons (list (car lst1) (car lst2)) (zip2 (cdr lst1) (cdr lst2))) )
       )
   )
 )
;(zip2 '(1 2 3 4 5) '(a b c d e))
;(zip2 '(1 2 3 4 5) '(a b c))
;(zip2 '(1 2) '(a b c d e))
;(zip2 '() '())



;013. Determinar si una lista es prefijo de otra

(define prefijo?
  (lambda(lst1 lst2)
      (if (> (length lst2) (length lst1)) #f
           (if (equal? lst1 (append lst2 (list-tail lst1 (length lst2))))
               #t
               #f
           )
      )
    )
  )
;(prefijo? '(1 2 3) '(1 2 3))

; 29. Definir en Scheme, una función sust de tres argumentos, el primero es Lista1 (una lista), el segundo es Patron1 (lista), el tercero es un Patron2 (lista).
; La función sust al ser evaluada devolverá como resultado la lista resultante de sustituir todas las ocurrencias de Patron1 en Lista1 por Patron2.


(define sust
(lambda(lst patron1 patron2)
(cond ((null? lst) lst)
((null? patron1) lst)
((< (length lst) (length patron2)) lst)
((if (equal? (primerosNElementos lst (length patron1)) patron1) (sust (append patron2 (sacarNElementos lst (length patron1))))
(sust (cdr lst) patron1 patron2);no...
))
)
)
)

; EJEMPLO: (sust '(a b c d 1 a b c 1 a) '(1 a) '( c ) ) => (a b c d c b c c)
; (sust '(1 2 s d a a 1 d f a a 1 d f) '(a a 1 ) '( c c c c )) => (1 2 s d c c c c d f c c c c d f)
; (sust '(a b c) '(a b) '(z) ) => '(z c)
; (sust '() '(..) '(..)) => '()


(define (cuadrado X) (* X X)) ;es EQUIVALENTE A: (define cuadrado (lambda (X) (* X X)))










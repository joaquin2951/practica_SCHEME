
;a)
;(condicion funcion)
(define obtenerCondicion (lambda(lst) (car lst)))
(define obtenerFuncion (lambda(lst) (cadr lst)))

(define aplica-cond
  (lambda (lista-pares)
    (lambda (valor)
      (cond
           ( (null? lista-pares) valor)
           ( ( (obtenerCondicion (car lista-pares)) valor) ( (obtenerFuncion (car lista-pares)) valor) )
           ( else ( (aplica-cond (cdr lista-pares)) valor ) )
           ))))


(define suma1 (lambda(x) (+ x 1)))
(define cuadrado (lambda(x) (* x x)))
(define lista-pares1 (list (list even? suma1) (list odd? cuadrado)))
(define lista-pares2 (list (list number? cuadrado) (list pair? cdr) (list symbol? list)))


;b)
(define (map-condicional lista-pares lista)
  (if (null? lista) '()
      (cons ( (aplica-cond lista-pares) (car lista) ) (map-condicional lista-pares (cdr lista)) )
      ))
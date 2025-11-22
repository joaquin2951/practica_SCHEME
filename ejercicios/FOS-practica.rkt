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


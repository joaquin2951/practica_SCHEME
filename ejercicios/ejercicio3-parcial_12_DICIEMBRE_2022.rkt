

(define reduceIz ;retorna una funcion que recibe UN argumento, que es una lista (lst)
  (lambda(f) ;f es de DOS argumentos
    (lambda(lst)
      (cond
            ( (= (length lst) 1) (car lst) )
            ( else ( (reduceIz f) (cons (f (car lst) (cadr lst) ) (cddr lst)) ) )
       ))))
          
      
                   
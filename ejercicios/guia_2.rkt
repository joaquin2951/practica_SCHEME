;10. Un árbol binario de nivel n es completo, si “cada nodo de nivel n es una hoja y cada nodo 
;de nivel menor que n no tiene subárboles izquierdo y derecho vacíos” 
;Definir una función de un argumento para determinar si el árbol dado como argumento es 
;completo.

; representacion de arbol binario:  '(Raiz AI AD)
; representacion arbol binario vacio: '()

(define (AI arbol)
  (car (cdr arbol))
  )

(define (AD arbol)
  (car (cdr (cdr arbol)))
  )

(define (esVacio? arbol)
  (null? arbol)
  )

(define (esHoja? arbol)
  (and (not(esVacio? arbol)) (esVacio? (AI arbol)) (esVacio? (AD arbol)))
  )


(define esCompleto?
  (lambda(arbol)
    (if (esHoja? arbol) #t
      (and  (not (esVacio? (AI arbol))) (not (esVacio? (AD arbol))) (esCompleto? (AI arbol)) (esCompleto? (AD arbol)) )
      )
    )
  )

(define (Raiz arbol)
  (car arbol))



(define arbol1
  '(1 () ()) )

(define arbol2
  '() )

(define arbol3
  '(1 (2 (4 () () ) ()) (3 () () ) )
  )

(define arbol4
  '(1 (2 () ()) (3 () ()))
  )

;12. Recorrer un arbol en inorden, formando una lista con los nodos que se visitan
(define inorden
  (lambda(arbol)
      (cond
           ((esVacio? arbol) '())
           ((esHoja? arbol) (list (Raiz arbol)))
           (else (append (inorden (AI arbol)) (list (Raiz arbol)) (inorden(AD arbol))) )
        )
    )
  )

;13. Recorrer un arbol en posorden, formando una lista con los nodos que se visitan
(define posorden
  (lambda(arbol)
      (cond
           ((esVacio? arbol) '())
           ((esHoja? arbol) (list (Raiz arbol)))
           (else (append (posorden (AI arbol)) (posorden (AD arbol)) (list (Raiz arbol)) ) )
        )
    )
  )


;17. Funcion para calcular el n-esimo elemento de la serie FIBONACCI

(define fbcc
  (lambda(enesimo)
    (cond ( (= 1 enesimo) 1)
          ( (= 2 enesimo) 1)
          ( else (+ (fbcc (- enesimo 1)) (fbcc (- enesimo 2)))  )
          )
    )
  )

;21. Dados una lista y dos elementos, determinar si estos son consecutivos.

(define consecutivos?
  (lambda(lst e1 e2)
    (cond ((null? lst) #f)
          ((= (car lst) e1) (= (cadr lst) e2))
          (else (consecutivos? (cdr lst) e1 e2))
    )
  )
)
        

;31. Se le solicita que proponga las funciones Scheme PUTPROP y GETPROP.




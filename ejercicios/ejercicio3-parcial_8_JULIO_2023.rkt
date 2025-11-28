(define arbol '(3 (5 nil nil) (7 nil nil)))

(define arbol2 '(12 (23 (4 nil nil) (5 nil nil)) 
(14 (24 nil nil) nil)))

(define arbol3 '(12 (36 (44 nil nil) (82 nil nil)) 
(14 (24 nil nil) (66 nil nil))))

(define menor10 (lambda (nro) (< nro 10))) 
; even? Retorna #t si el argumento es un número par, #f en caso contrario 
; odd? Retorna #t si el argumento es un número impar, #f en caso contrario 
;'(raiz AI AD)

(define (raiz arbol) (car arbol) )
(define (AI arbol) (cadr arbol))
(define (AD arbol) (caddr arbol))
(define esVacio? (lambda(arbol) (equal? arbol 'nil)))


(define (los-que-cumplen f)
   (lambda(arbol)
      (if (esVacio? arbol) '()
          (if (f (raiz arbol)) (cons (raiz arbol) (append ((los-que-cumplen f) (AI arbol)) ((los-que-cumplen f) (AD arbol))))
              (append ((los-que-cumplen f) (AI arbol)) ((los-que-cumplen f) (AD arbol)))
              ))))
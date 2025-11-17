;AL ARBOL en SCHEME lo imaginaremos como una lista de la forma
; '(RAIZ AI AD)
; UN ARBOL VACIO SERA REPRESENTADO COMO '()

;Para caso de prueba con ARBOL HOJA
(define arbolPrueba
  (lambda()
    '(2 (3 () () )
        (4
          (5
             (6
               ()
               (7 () () )
              )
             ()
           )
          ()
         )
      )
    )
  )

;Para caso de prueba con ARBOL VACIO
(define arbolVacioPrueba
  (lambda()
    '()
    )
  )



(define esVacio
  (lambda (arbol)
    (null? arbol)
    )
  )

(define AD
  (lambda (arbol)
    (car (cdr (cdr arbol)))
    )
  )

(define AI
  (lambda (arbol)
    (car (cdr arbol))
    )
  )

(define Raiz
  (lambda (arbol)
    (car arbol)
    )
  )


(define esHoja
  (lambda (arbol)
   (and (esVacio (AI arbol)) (esVacio (AD arbol)))
    )
  )


;101. Dados un arbol y un elemento cualquiera, determinar si el elemento pertenece al arbol

(define pertenece
  (lambda(arbol elemento)
   (if (esVacio arbol) #f
    (if (esHoja arbol) ;condicion
        (= elemento (Raiz arbol));si condicion es verdadera
        (if (= (Raiz arbol) elemento) #true
           (or (pertenece (AI arbol) elemento) (pertenece (AD arbol) elemento))
         );si condicion es falsa
     )
    )
  )
)
        
;(pertenece (arbolPrueba) 3)



;RECORRIDOS PREORDEN INORDEN Y POSORDEN
;(list Raiz '() '()) DISTINTO DE (append Raiz '() '())


(define preOrden
(lambda(arbol lst)
(if ( not(esVacio arbol) )
(append (append (append lst (list (raiz arbol))) (preOrden (AI arbol) '())) (preOrden (AD arbol) '() ))
'()
)
)
)

; (preOrden arbol '())
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

(define base '((juan edad 30) (juan direccion (lavaise 610)) (pedro edad 34) (pedro hijos 
2) (pedro sueldo 1400)))


;GETPROP obtiene el valor para un conjunto (objeto atributo)
;ejemplo:  (GETPROP base 'juan 'edad)  --> 30
;las listas tienen la forma '(OBJETO ATRIBUTO VALOR)
(define obj
  (lambda(lst)
    (car lst)))

(define atr
  (lambda(lst)
    (cadr lst)))

(define (val lst)
  (caddr lst)
  )

(define GETPROP
  (lambda (BaseDeDatos Objeto Atributo)
    (cond
      ((null? BaseDeDatos) -1)

      ( (eqv? (and (equal? (obj (car BaseDeDatos)) Objeto) (equal? (atr (car BaseDeDatos)) Atributo)) #t)
       (val (car BaseDeDatos)))

      (else
       (GETPROP (cdr BaseDeDatos) Objeto Atributo)))))



(define PUTPROP 
(lambda (BasedeDatos Objeto Atributo Valor)
  (if (equal? (GETPROP BasedeDatos Objeto Atributo) -1) (cons (list Objeto Atributo Valor) BasedeDatos)
      #f);deberiamos cambiar el retorno de #f por un camino en el que hagamos eliminar la terna (Objeto Atributo X) de la lista y entonces luego si hacer
         ;el CONS de la terna nueva al principio de la lista
)
)




;35. Uno de los recorridos de árboles es en forma horizontal, el cual consiste en visitar los 
;nodos que están en un mismo nivel (de izquierda a derecha), luego proceder con los nodos 
;del próximo nivel y así sucesivamente hasta visitar todos los nodos del árbol.
;
;FORMA DE ARBOL = '(RAIZ AI AD)
(define arbol35
  '(a (c
        (t () () )
        (r
           (q () ())
           (z () () )) )
      (j
         (h () () )
         (m () () )) )
)

;el proceso podría pensarse a partir de una lista de árboles, y a medida que se extrae 
;una raíz, los árboles generados se incorporan al final de la lista.

(define (recorridoHorizontal lstArboles)
  (cond
      ( (null? lstArboles) '() )
      ( (esVacio? (car lstArboles)) (recorridoHorizontal (cdr lstArboles)) )
      ;( (esHoja?  (car lstArboles)) (list (Raiz (car lstArboles))))  NO HACE FALTA PENSAR ESTA CONDICION, SE SATISFACE CON LA DE ABAJO
      ( else (append (list (Raiz (car lstArboles))) (recorridoHorizontal (append (cdr lstArboles) (list (AI (car lstArboles))) (list (AD (car lstArboles))) )) ) )
   )
 )

;PRUEBAS:
; (recorridoHorizontal (list arbol35))
; (recorridoHorizontal (list arbol1))
; (recorridoHorizontal (list arbol2))
; (recorridoHorizontal (list arbol3))
; (recorridoHorizontal (list arbol4))




;38. Se le solicita que defina la siguiente función en Scheme:
;en donde ListaFunciones es una lista de funciones de un argumento. La aplicación de 
;mapeoFunciones aplica todas las funciones de ListaFunciones a los elementos de Lista, 
;retornando una lista con los resultados de estas evaluaciones.

(define suma1 (lambda(x) (+ 1 x)))
(define resta1 (lambda(x) (- x 1)))
(define por2 (lambda(x) (* x 2)))
(define lstFunct (list suma1 resta1 por2))

(define evaluarFunciones
  (lambda(ListaFunciones Elemento)
    (if (null? ListaFunciones) '()
        (append (list ( (car ListaFunciones) Elemento )) (evaluarFunciones (cdr ListaFunciones) Elemento) )
    )))

(define mapeoFunciones
  (lambda(ListaFunciones Lista)
     (if (null? Lista) Lista
        (append (list (evaluarFunciones ListaFunciones (car Lista))) (mapeoFunciones ListaFunciones (cdr Lista)))
  )))




;42. Se quiere calcular las comisiones de viajantes de una empresa. Las mismas depende de los 
;montos vendidos por cada viajante y del producto que vendió. Para ello se cuenta con la 
;información de las ventas por producto que realizó cada viajante.

;la información sobre viajantes está organizada de la siguiente manera:
;((viajante1(venta,productoA)(vta,productoB)....) (viajante2(venta, productoC)(venta,ProductoA)..)...) 
;y la información sobre los productos está organizado en otra lista de la siguiente manera: 
;( (productoA, porcentajeA) (produtoB, porcentajeB).......)

;El resultado de la evaluación de la función que debe crear, debe ser la lista de los viajantes 
;a la que se le agrega un dato más al final que es su comisión. De esta manera la lista 
;resultante será: 
;((viajante1(vta prodA)(vta prodB)..... comisión)(....))
(define Productos
  '( (productoA 10) (productoB 10) (productoC 0) (productoD 100)  )
  )
(define Viajantes
  '(
    (viajante1 (10 productoA) (10 productoB) (0 productoC))
    (viajante2 (10 productoA) (10 productoB) (10 productoC) (10 productoD) )
    (viajante3 (10 productoA) (10 productoB) (10 productoC) (0 productoD) )
    )
 )

(define porcentajeProducto
  (lambda(Producto lstProd)
    (cond
      ( (null? lstProd) 0)
      ( (equal? (caar lstProd) Producto) (cadar lstProd))
      ( else (porcentajeProducto Producto (cdr lstProd)) )
     )))

(define calculaComisionViajante
  (lambda(viajante lstProductos) ;recibe una lista de forma ( (10 prodA) (15 prodB) ... etc)
    (if (null? viajante) 0
         (+ (+ (caar viajante) (porcentajeProducto (cadar viajante) lstProductos)) (calculaComisionViajante (cdr viajante) lstProductos))
         )))
    

(define listaComisiones
  (lambda(lstViajantes lstProductos)
    (if (null? lstViajantes) '()
         (cons (append (car lstViajantes) (list (calculaComisionViajante (cdar lstViajantes) lstProductos)) ) (listaComisiones (cdr lstViajantes) lstProductos))
     )))
        
  

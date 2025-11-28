(define grafo-proceso 
'((orden (dibujar 12 48) plano) (plano (cortar 10 2) cortado) 
(cortado (plegar 8 4) plegado) (cortado (soldar 16 20) soldado) 
(cortado (pintar 34 20) pintado) (plegado (soldar 20 25) soldado) 
(soldado (pintar 34 24) pintado) (pintado (empaquetar 20 2) producto)) 
)


;a) (transicion GP nodo-desde actividad)  que devuelve todos los datos de la transición al realizar la 
;actividad indicada por el tercer argumento desde el nodo indicado por el segundo. El argumento 
;GP es el grafo del proceso de acuerdo a la representación presentada anteriormente. 
;Note en los ejemplos que esta función devuelve alguna de las listas contenidas en la lista GP. Si la 
;transición no existe, debe retornar falso.
;
;> (transicion grafo-proceso 'plano 'cortar) 
;(plano (cortar 10 2) cortado) 
;> (transicion grafo-proceso 'cortado 'plegar) 
;(cortado (plegar 8 4) plegado) 
;> (transicion grafo-proceso 'soldado 'cortar) 
;#f
(define (getTransicion GP) (car GP))
(define (nombreNodoDesde transicion) (car transicion))
(define (nombreNodoHasta transicion) (caddr transicion))
(define (nombreActividad transicion) (caadr transicion))

(define (transicion GP nodo-desde actividad)
  (cond
        ( (null? GP) #f )
        ( (and (equal? (nombreNodoDesde (getTransicion GP)) nodo-desde) (equal? (nombreActividad (getTransicion GP)) actividad) ) (getTransicion GP) )
        ( else (transicion (cdr GP) nodo-desde actividad) )
        ))

;b) (nodo-hasta transicion)  que devuelve el nodo destino de ejecutar la transición recibida como 
;argumento. Ver los ejemplos que siguen: 
;> (nodo-hasta '(plano (cortar 10 2) cortado)) 
;cortado 
;> (nodo-hasta (transicion grafo-proceso 'cortado 'plegar)) 
;plegado
(define (nodo-hasta transicion) (nombreNodoHasta transicion))



;c) (lista-actividades GP)  que devuelve una lista con los nombres de las actividades del proceso 
;representado por GP, sin elementos repetidos.
;
;> (lista-actividades grafo-proceso) 
;(dibujar cortar plegar soldar pintar empaquetar)


(define (lista-actividades GP)
    (cond
          (  (null? GP) '() )
          (  (not (member (nombreActividad (getTransicion GP)) (lista-actividades (cdr GP))) )  (cons (nombreActividad (getTransicion GP)) (lista-actividades (cdr GP))))
          ( else (lista-actividades (cdr GP)) )
          ))


;d) (proceso-valido? GP info-proceso)  que devuelve verdadero si info-proceso representa un proceso 
;válido según lo indicado en el grafo GP.

;El argumento info-proceso es una lista de 3 elementos, donde los dos primeros son el nodo inicial 
;(NI) y el nodo final (NF) del proceso y el tercer elemento es una lista con las actividades que 
;llevarían desde NI a NF. En caso de que info-proceso no corresponda con un camino válido en el 
;grafo, la función deberá retornar falso. 
;> (proceso-valido? grafo-proceso '(orden producto (dibujar cortar plegar soldar pintar empaquetar)))  
;#t 
;> (proceso-valido? grafo-proceso  '(orden producto (dibujar plegar soldar pintar cortar)))  
;#f

(define (lstActividades info-proceso) (caddr info-proceso))
(define (proceso-valido? GP info-proceso)
  (cond
        ( (null? (lstActividades info-proceso)) #t  ) ;condicion1 / retorno1
        
        ( (transicion GP (car info-proceso) (caaddr info-proceso)) ;condicion2
                         (proceso-valido? GP
                                          (list
                                               (nodo-hasta (transicion GP (car info-proceso) (caaddr info-proceso)))
                                               (cadr info-proceso)
                                               (cdr (lstActividades info-proceso))
                                           )
                          );retorno2
        )
        
        ( else #f ) ;condicion3 / retorno 3
    ))
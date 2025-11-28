;decide representar un examen como una lista de ejercicios. A su vez, 
;propone representar cada ejercicio como una lista consistente de un código, un tema y una 
;nota máxima. Por ejemplo, un examen final de Paradigmas podría estar representado por la 
;siguiente lista: 

(define examenParadigmas 
'((ej1 logica 6) (ej2 logica 24) (ej3 objetos 6)  
(ej4 objetos 34) (ej5 funcional 6) (ej6 funcional 24)) )

;Por otra parte, los resultados de un examen para un alumno dado se representarán como 
;listas con la siguiente estructura:   
;'(nombreAlumno (ejercicio1 nota1) (ejercicio2 nota2) ...)
;Donde nombreAlumno será una lista conteniendo los nombres y finalizando con el apellido 
;del alumno (al final de la lista).  
;Además, cuando la nota de un ejercicio es 0 (cero) no se incluye la misma en el listado de 
;notas.  Por ejemplo, los resultados del alumno Ariel J. Prieto serán representados como: 
(define resultadosAriel  
'((ariel j prieto) (ej1 6) (ej2 24) (ej3 6)  
(ej4 30) (ej5 6) (ej6 24)))
;
;Considere además la siguiente lista, que incluye los resultados de tres alumnos: 
(define listaResultados  
  '( ((nicolas lopez) (ej1 2) (ej2 10) (ej4 8) (ej6 5)) 
     ((juan l perez) (ej2 4) (ej4 12) (ej5 2) (ej6 3)) 
     ((ariel j prieto) (ej1 6) (ej2 24) (ej3 6)  
                       (ej4 30) (ej5 6) (ej6 24)) ) )

;a) ( maximaNota  examen ) 
;Calcula la nota total máxima que un alumno puede obtener en el examen pasado como 
;argumento.  
;Ejemplo:  
;> (maximaNota examenParadigmas) 
;100 
(define (getEjercicio examen) (car examen))
(define (notaEjercicio ejercicio) (caddr ejercicio) )

(define maximaNota
  (lambda (examen)
    (if (null? examen) 0
        (+ (notaEjercicio (getEjercicio examen)) (maximaNota (cdr examen)) )
        )))

;b) ( notaTotalAlumno  examen  resultados ) 
;Calcula la nota total de un alumno en base a los resultados del mismo. Si la nota 
;obtenida es mayor a la nota máxima del examen debe retornar el símbolo  'error. 
;Ejemplo:  
;> (notaTotalAlumno examenParadigmas resultadosAriel) 
;96


(define (notaTotalAlumno examen resultados)
  (cond
        ( (null? resultados) 0)
        ( (not (integer? (cadar resultados))) (notaTotalAlumno examen (cdr resultados)) ) 
        (else (+ (cadar resultados) (notaTotalAlumno examen (cdr resultados))) )
        ))

;c)  ( armarPlanilla  examen  listaResultados ) 
;Permite obtener una planilla con los resultados por alumno para un examen dado. Para 
;cada alumno se indicará su apellido, su nota total y su condición. La condición será 
;“aprobado” si la nota final es mayor o igual al 60% de la nota máxima del examen. Caso 
;contrario será “no-aprobado”.  
;Ejemplo:  
;> (armarPlanilla examenParadigmas listaResultados ) 
;'((lopez 25 no-aprobado) (perez 21 no-aprobado) (prieto 96 
;aprobado))
(define listaNombre (lambda (lst) (car lst)))

(define apellido
  (lambda (lstNombre)
    (if (null? (cdr lstNombre)) (car lstNombre)
        (apellido (cdr lstNombre)))))

(define condicionAlumno
  (lambda(resultado examen)
    (if (>= (notaTotalAlumno examen resultado) 60) "aprobado"
        "no-aprobado"
        )))
    
(define armarResultadoAlumno
  (lambda (resultado examen)
    (list (apellido (listaNombre resultado)) (notaTotalAlumno examen resultado) (condicionAlumno resultado examen) )
    ))
                                                                                 
(define (armarPlanilla examen listaResultados)
  (if (null? listaResultados) '()
      (cons (armarResultadoAlumno (car listaResultados) examen) (armarPlanilla examen (cdr listaResultados)))
      ))


;d) ( filtraPlanilla criterio ) 
;Permite obtener una función que recibe como argumentos una planilla (como la 
;devuelta por la función del ítem anterior) y genera una lista con los apellidos de los 
;alumnos cuyo resultado cumple con el argumento criterio. filtraPlanilla al ser aplicada a 
;un criterio retorna una función de 1 argumento, el cual debe ser una lista.  
;Ejemplos: 
;> (filtraPlanilla 'no-aprobado) 
;#<procedure> 
;> ((filtraPlanilla 'no-aprobado) '((lopez 25 no-aprobado) (perez 
;21 no-aprobado) (prieto 96 aprobado))) 
;'(lopez perez) 
;> ((filtraPlanilla 'aprobado) '((lopez 25 no-aprobado) (perez 21 
;no-aprobado) (prieto 96 aprobado))) 
;'(prieto)  


(define filtraPlanilla
  (lambda (criterio)
    (lambda (planilla)
      (cond ( (null? planilla) '() )
            ( (equal? criterio (caddar planilla)) (cons (caar planilla) ( (filtraPlanilla criterio) (cdr planilla) ) ) )
            ( else ( (filtraPlanilla criterio) (cdr planilla) ) )
            ))))
          
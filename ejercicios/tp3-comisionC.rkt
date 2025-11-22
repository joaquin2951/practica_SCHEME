;Enunciado 
;Trabajaremos con una base de datos veterinaria donde cada animal se representa 
;como una lista con 5 elementos: 
;(id nombre (clasificaciones...) (caracteristicas...) edad) 
;Donde: 
;• id: número entero único 
;• nombre: string con el nombre del animal 
;• clasificaciones: lista de strings (ej: "mamifero", "domestico") 
;• caracteristicas: lista de strings (ej: "vacunado", "esterilizado") 
;• edad: número entero (años)

;BASE DE DATOS DE EJEMPLO: 
 
    (define bd-animales 
      '((1 "Max" ("mamifero" "domestico") ("vacunado" "esterilizado") 5) 
        (2 "Luna" ("mamifero" "domestico") ("vacunada") 3) 
        (3 "Pipo" ("ave" "domestico") ("parlanchin") 2) 
        (4 "Rex" ("mamifero" "domestico") ("guardian") 7) 
        (5 "Coco" ("ave" "domestico") () 1) 
        (6 "Simba" ("mamifero" "salvaje") ("carnivoro" "territorial") 8) 
        (7 "Kaa" ("reptil" "salvaje") ("venenoso") 12) 
        (8 "Zara" ("ave" "salvaje") ("rapaz" "nocturna") 4)))


;EJERCICIOS A RESOLVER 
;───────────────────────────────────────────────────────── 
;1. OBTENER EDAD DE UN ANIMAL
;FUNCION DE ORDEN SUPERIOR PARA GENERALIZAR EL CASO DE obtener-edad , obtener-id, obtener-caracteristicas, etc
(define obtener-dato (lambda(animal)

                       (lambda(posicion)
                         (if (= 0 posicion) (car animal)
                             ( (obtener-dato (cdr animal)) (- posicion 1) )))))
;ejemplo: ( (obtener-dato (car bd-animales) ) 0)


(define edad (lambda(animal)
                       ( ( obtener-dato animal ) 4)))


;2. OBTENER CARACTERÍSTICAS DE UN ANIMAL 
;Definir la función características que recibe un animal y retorna una  lista con sus 
;características.
(define caracteristicas (lambda(animal)
               ( ( obtener-dato animal) 3)))



;3. BUSCAR ANIMAL POR ID 
;Definir la función buscar-por-id que recibe una base de datos y un id, y retorna el 
;animal con ese id (o #f si no existe).
(define identidad (lambda(animal)
             ( (obtener-dato animal) 0)))

(define buscar-por-id (lambda(lst id)
                        (cond ( (null? lst) #f)
                              ( (= id (identidad (car lst))) (car lst))
                              ( else (buscar-por-id (cdr lst) id) )
                              )))

;ejemplo:    (buscar-por-id bd-animales 2)
;   (buscar-por-id bd-animales 8) 
;      (buscar-por-id bd-animales 9)


;FUNCION de orden superior para generalizar el caso de listar-nombres , listar-ids, listar-caracteristicas, etc
(define listar-por (lambda (funcion)
                     (lambda(lst)
                       (if (null? lst) lst
                           (cons (funcion (car lst)) ( (listar-por funcion) (cdr lst)))
                           ))))
                       
; ( (listar-por identidad)  bd-animales)


(define nombre (lambda(animal)
               ( ( obtener-dato animal) 1)))

(define listar-nombres
  (lambda(lst) ( (listar-por nombre) lst)))

;uso:   (listar-nombres bd-animales)



;5. CONTAR ANIMALES ADULTOS 
;Definir la función contar-adultos que recibe una base de datos y un número n, y 
;cuenta cuántos animales tienen edad mayor a n.
(define listar-edad
  (lambda(lst) ( (listar-por edad) lst)))  ;uso: (listar-edad bd-animales)

(define filtrar (lambda (condicion lst)
                  (if (null? lst) lst
                      (if (condicion (car lst)) (cons (car lst) (filtrar condicion (cdr lst)))
                          (filtrar condicion (cdr lst))))))

(define mayorA (lambda(n)
                 (lambda(edad)
                 (> edad n)))
  )

(define contar-adultos (lambda(lst n)
                      (length  (filtrar (mayorA n) (listar-edad lst)))))
                         
;  (contar-adultos bd-animales 3)
;   (contar-adultos bd-animales 7)



;6. VERIFICAR SI UN ANIMAL TIENE UNA CLASIFICACIÓN 
;Definir la función tiene-clasificacion? que recibe un animal y una clasificación, y 
;retorna #t si el animal tiene esa clasificación, #f en caso contrario.
(define clasificacion (lambda(animal)
               ( ( obtener-dato animal) 2)))

(define clasificacionesIguales (lambda(n)
                 (lambda(c)
                 (equal? c n)))
  )

(define tiene-clasificacion?
   (lambda (animal clf)
     (> (length (filtrar (clasificacionesIguales clf) (clasificacion animal))) 0)))
     

;  (tiene-clasificacion? (car bd-animales) "mamifero")
;  (tiene-clasificacion? (car bd-animales) "ave")


;7. FILTRAR ANIMALES POR CLASIFICACIÓN 
;Definir la función filtrar-por-clasificacion que recibe una base de datos y una 
;clasificación, y retorna una lista con los animales que tienen esa clasificación.

(define filtrar2Argumentos (lambda (condicion lst arg2)
                  (if (null? lst) lst
                      (if (condicion (car lst) arg2) (cons (car lst) (filtrar2Argumentos condicion (cdr lst) arg2))
                          (filtrar2Argumentos condicion (cdr lst) arg2)))))

(define filtrar-por-clasificacion (lambda (lst clasif)
                             (filtrar2Argumentos tiene-clasificacion? lst clasif)))

;  (filtrar-por-clasificacion bd-animales "ave")
;   (filtrar-por-clasificacion bd-animales "salvaje")



;8. ENCONTRAR EL ANIMAL MÁS VIEJO 
;Definir la función animal-mas-viejo que recibe una base de datos (no vacía) y retorna 
;el animal con mayor edad.

(define  animal-mas-viejo
  (lambda (lst)
    (if (= (length lst) 1) (car lst)
        (if (> (edad (car lst)) (edad (animal-mas-viejo (cdr lst)))) (car lst)
            (animal-mas-viejo (cdr lst))
            ))))


;9. CONTAR ANIMALES QUE CUMPLEN CONDICIÓN (ORDEN SUPERIOR) 
;Definir la función contar-si que recibe un predicado (función que retorna #t o #f) y 
;una base de datos, y retorna la cantidad de animales que cumplen esa condición.

(define contar-si (lambda (predicado lst)
  (length (filtrar predicado lst))
  ))

;Ejemplo de uso: 
;     Contar animales mayores a 5 años 
;    (contar-si (lambda (animal) (> (edad animal) 5)) bd-animales) 
;    => 3  ; Rex (7), Simba (8), Kaa (12) 
; 
;     Contar animales domésticos 
;    (contar-si (lambda (animal) (tiene-clasificacion? animal "domestico")) bd-animales) 
;    => 5  ; Max, Luna, Pipo, Rex, Coco 
; 
;     Contar aves 
;    (contar-si (lambda (animal) (tiene-clasificacion? animal "ave")) bd-animales) 
;    => 3  ; Pipo, Coco, Zara 
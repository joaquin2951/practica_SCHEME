; funciones auxiliares para los ejemplos
(define suma-1(lambda(x) (+ x 1)))
(define resta-1 (lambda (x) (- x 1)))


(define cuadrado (lambda (x) (* x x)))
(define cubo (lambda (x) (* x x x)))

; definir una lista con las funciones cubo y cuadrado


(define por-2 (lambda (x) (* x 2)))
(define mayor2 (lambda (x) (> x 2)))
(define menor4 (lambda (x) (< x 4)))
(define mayor5 (lambda (x) (> x 5)))
(define increm (lambda (x) (+ x 1)))

; funciones como argumentos 
(define factorial (lambda (n)
  (if (= n 0) 1
      (* n (factorial  (- n 1))))))

(define sumatoria (lambda (n)
  (if (= n 0) 0
      (+ n (sumatoria  (- n 1))))))

(define combina (lambda (f n cb)
  (if (= n 0) cb
      (f n (combina f (- n 1) cb )))))

;redefinir factorial y sumatoria utilizando combina

; Funciones que construyen funciones
; composicion
(define composicion (lambda (f1 f2)
  (lambda (x)
    (f1 (f2 x)))))

; aplicaSi
(define aplica-si (lambda (condic operac)
  (lambda (l)
   (if (null? l)'()
    (if (condic (car l))
   (cons (operac (car l))((aplica-si condic operac)(cdr l)))
   (cons (car l) ((aplica-si condic operac) (cdr l))))))
  )
)

;Definir la función mapeo

;Definir la función filtro

;Definir la función (aplica-mapeo LFunciones Lista)

;Definir la función(aplica-filtro listaFiltros)

;Definir la función (take-while condicion L)

;Definir la función (drop-while condicion L)

;Definir la función (repetir f n)

; Definir la función (aplica L f n)

; Definir la función (aplica-all f L)

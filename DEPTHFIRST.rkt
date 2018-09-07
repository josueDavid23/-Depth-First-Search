#lang racket
;Tarea Alejandro Salas - version 3.0

;(define lista (list))
(require graph math/matrix)
(require typed-stack)
(define-struct nodo (Nombre Sequence)#:mutable #:transparent)
(define-struct arista (IdNodo1 IdNodo2 Peso))
(define Nodos (list))
(define Aristas (list))
(define pila (make-stack '(8 4)))



;3 perro y 2 hueso
(define A(matrix ([ 0 1 1 1 0 1 1 2 0 1]
                  [ 0 0 1 0 0 1 0 0 0 1]
                  [ 0 1 1 0 1 1 1 1 1 1]
                  [ 0 0 0 0 0 1 0 0 0 0]
                  [ 1 1 1 0 1 1 1 1 1 1]
                  [ 1 0 0 0 1 0 0 0 1 0]
                  [ 1 1 1 1 1 0 1 0 1 1]
                  [ 0 0 0 0 1 0 1 0 1 0]
                  [ 1 1 1 1 3 1 1 0 1 1]
                  )))

(define (ciclo)
  
  (set! Nodos (list))
  (define node(make-nodo (string-append (~v 8) (~v 4)) '()))
  (set! Aristas (list))
  (set! pila (make-stack '(8 4)))
  (set! Nodos (append Nodos (list node)))
  (cicloInicial)
  (define x (car(top pila)))
  (define y (last(top pila)))

  (define s(string-append (~v  x) (~v y)))

  (reverse (camino (string-append (~v  x) (~v y)) (list s)) )
  )



(define (cicloInicial)
  (if (equal? 2 (matrix-ref A (car(top pila))(last(top pila)))) pila
      (validaciones))
  )

(define (validaciones)
 
  
  (define x (car(top pila)))
  (define y (last(top pila)))
  (define pilaNueva (pop pila))
  (set! pila pilaNueva)
  (if (>= (-(length (matrix-cols A))1) (+ y 1))(hayConexion  x (+ y 1) x y)  #f)
  
  (if (<= 0 (- x 1)) (hayConexion (- x 1) y x y) #f)
  (if (>= (-(length (matrix-rows A))1) (+ x 1)) (hayConexion (+ x 1) y x y) #f)
  (if (<= 0 (- y 1))(hayConexion x (- y 1) x y) #f )
  (cicloInicial)
  )

(define (hayConexion nuevoI nuevoJ i j)
  (if (or (equal? 2 (matrix-ref A nuevoI nuevoJ))(equal? 1 (matrix-ref A nuevoI nuevoJ))) (agregar nuevoI nuevoJ i j)  
      #f) 
  )

(define (agregar i j xP yP)
  (if (empty? (buscarNodo (string-append (~v i) (~v j)))) (agregarAux i j xP yP)  
      #f)

  )

(define (agregarAux i j xP yP)
  (define node(make-nodo (string-append (~v i) (~v j)) '()))
  (define edge(make-arista (string-append (~v xP) (~v yP)) (string-append (~v i) (~v j)) (matrix-ref A i j)))
  (set! Nodos (append Nodos (list node)))
  (set! Aristas (append Aristas (list edge)))
  (set-nodo-Sequence! (car(buscarNodo (string-append (~v xP) (~v yP)))) (append (nodo-Sequence(car(buscarNodo (string-append (~v xP) (~v yP)))))(list(string-append (~v i) (~v j)))))
  (define pilaNueva (push pila (list i j)))
  (set! pila pilaNueva)
  )

  
(define (buscarNodo idNodo)
    (filter (lambda(x) (equal? (nodo-Nombre x) idNodo)) Nodos))

(define (buscarNodoPadreAux idNodo)
    (buscarNodo (arista-IdNodo1 (first(filter (lambda(x) (equal? (arista-IdNodo2 x) idNodo)) Aristas)))))

(define (camino id lista )
  (if (equal? "84" (nodo-Nombre(car(buscarNodoPadreAux id)))) (append lista (list "84"))  
      (camino (nodo-Nombre(car(buscarNodoPadreAux id))) (append lista (list(nodo-Nombre(car(buscarNodoPadreAux id)))) )))

  )
  (define (arbol)
    Nodos)
;(stack->list pila)
;Principallllllllllllllllllllllllllllllllllllllllllllll


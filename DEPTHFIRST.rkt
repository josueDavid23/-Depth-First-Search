#lang racket
;Tarea- version 3.1
;$ (busquedaProfundidad 8 4)
;(define lista (list))
(require graph math/matrix)

(define-struct nodo (Nombre Sequence)#:mutable #:transparent)
(define-struct arista (IdNodo1 IdNodo2 Peso))
(define arbolN (list))
(define arbolA (list))


(struct stack (tamano lista) #:mutable)
(define (size stack) (stack-tamano stack))
(define pila2 (stack 0 '()))

(define (push stack item)
  (set-stack-lista! stack ;setee el buffer de la pila
      (append (list item) (stack-lista stack)))
      (set-stack-tamano! stack (+ (stack-tamano stack) 1)))


(define (pop pila_2)
      (let ([primer (car (stack-lista pila_2))]
            [resto (cdr (stack-lista pila_2))])

        (set-stack-lista! pila_2 resto)
        (set-stack-tamano! pila_2 (- (stack-tamano pila_2) 1))
        primer))
;FUNCIONES DE PILA
;3 perro y 2 hueso
(define C(matrix ([ 0 1 1 1 0 1 1 2 0 1]
                  [ 0 0 1 0 0 1 0 0 0 1]
                  [ 0 1 1 0 1 1 1 1 1 1]
                  [ 0 0 0 0 0 1 0 0 0 0]
                  [ 1 1 1 0 1 1 1 1 1 1]
                  [ 1 0 0 0 1 0 0 0 1 0]
                  [ 1 1 1 1 1 0 1 0 1 1]
                  [ 0 0 0 0 1 0 1 0 1 0]
                  [ 1 1 1 1 3 1 1 0 1 1]
                  )))

(define (busquedaProfundidad x y)
  (set! pila2 (stack 0 (list x y)))
  (set! arbolN (list))
  (define node(make-nodo (string-append (~v x) (~v y)) '()))
  (set! arbolA (list))
  (push pila2 (list x y))
  (set! arbolN (append arbolN (list node)))
  (main x y)
  )



(define (cicloInicial)
  (if (equal? 2 (matrix-ref C (car (car(stack-lista pila2)))(last (car(stack-lista pila2))))) pila2
      (verficarlados))
  )

(define (verficarlados)
  (define x (car (car(stack-lista pila2))))
  (define y (last (car(stack-lista pila2))))
  (pop pila2)
  (verificandoLadosAux x y)
  (cicloInicial)
  
  )

(define (hayConexion nuevoI nuevoJ i j)
  (if (or (equal? 2 (matrix-ref C nuevoI nuevoJ))(equal? 1 (matrix-ref C nuevoI nuevoJ))) (agregar nuevoI nuevoJ i j)  
      #f) 
  )

(define (agregar i j xP yP)
  (if (empty? (buscarNodo (string-append (~v i) (~v j)))) (agregarAux i j xP yP)  
      #f)

  )

(define (agregarAux i j xP yP)
  (define node(make-nodo (string-append (~v i) (~v j)) '()))
  (define edge(make-arista (string-append (~v xP) (~v yP)) (string-append (~v i) (~v j)) (matrix-ref C i j)))
  (set! arbolN (append arbolN (list node)))
  (set! arbolA (append arbolA (list edge)))
  (set-nodo-Sequence! (car(buscarNodo (string-append (~v xP) (~v yP)))) (append (nodo-Sequence(car(buscarNodo (string-append (~v xP) (~v yP)))))(list(string-append (~v i) (~v j)))))
  (push pila2 (list i j))
  
  )

  
(define (buscarNodo idNodo)
    (filter (lambda(x) (equal? (nodo-Nombre x) idNodo)) arbolN))

(define (buscarNodoPadreAux idNodo)
    (buscarNodo (arista-IdNodo1 (first(filter (lambda(x) (equal? (arista-IdNodo2 x) idNodo)) arbolA)))))

(define (camino id lista fin)
  (if (equal? fin (nodo-Nombre(car(buscarNodoPadreAux id)))) (append lista (list fin))  
      (camino (nodo-Nombre(car(buscarNodoPadreAux id))) (append lista (list(nodo-Nombre(car(buscarNodoPadreAux id)))) )fin))

  )
  (define (arbol)
    arbolN)

(define (main x y)
  (cicloInicial)
  (define fila (car(car (stack-lista pila2))))
  (define columna (last(car (stack-lista pila2))))
  
  (define s(string-append (~v  fila) (~v columna)))
  
  (reverse (camino (string-append (~v  fila) (~v columna)) (list s) (string-append (~v  x) (~v y))) )
  

  )

(define (verificandoLadosAux x y)
  (if (>= (-(length (matrix-cols C))1) (+ y 1))(hayConexion  x (+ y 1) x y)  #f)
  
  (if (<= 0 (- x 1)) (hayConexion (- x 1) y x y) #f)
  (if (>= (-(length (matrix-rows C))1) (+ x 1)) (hayConexion (+ x 1) y x y) #f)
  (if (<= 0 (- y 1))(hayConexion x (- y 1) x y) #f )
  )

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Cubo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))


;crea una lista con n elementos elem, y a ambos lados con ceros.
;ejemplo con n=4 y elem=1: (list 0 0 0 0 1 1 1 1 0 0 0 0)
(define (CrearListaEspecialV1 n elem)
  (AuxCrearListaEspecialV1 n elem 0 '())
  )

(define (AuxCrearListaEspecialV1 n elem cont lista)
  (cond
    ((equal? (* n 3) cont) lista)
    ((>= cont (* n 2)) (AuxCrearListaEspecialV1 n elem (+ cont 1) (append lista (list 0))))
    ((< cont n) (AuxCrearListaEspecialV1 n elem (+ cont 1) (append lista (list 0))))
    (else
     (AuxCrearListaEspecialV1 n elem (+ cont 1) (append lista (list elem)))
    )
  )
 )







;crea una lista con n elementos elem1, n elementos elem2, n elementos elem3,
;ejemplo con n=4 y elem1=5, elem2=6, elem3=2: (list 5 5 5 5 6 6 6 6 2 2 2 2)
(define (CrearListaEspecialV3 n elem1 elem2 elem3)
  (AuxCrearListaEspecialV3 n elem1 elem2 elem3 0 '())
  )

(define (AuxCrearListaEspecialV3 n elem1 elem2 elem3 cont lista)
  (cond
    ((equal? (* n 3) cont) lista)
    ((< cont n) (AuxCrearListaEspecialV3 n elem1 elem2 elem3 (+ cont 1) (append lista (list elem1))))
    ((>= cont (* n 2)) (AuxCrearListaEspecialV3 n elem1 elem2 elem3 (+ cont 1) (append lista (list elem2))))
    (else
     (AuxCrearListaEspecialV3 n elem1 elem2 elem3 (+ cont 1) (append lista (list elem3)))
    )
  )
 )


;crea la matriz del cubo con todas las listas especiales usando V1 y V3
(define (CrearMatriz n)
  (AuxCrearMatriz n 0 '())
  )

(define (AuxCrearMatriz n cont matriz)
  (cond
    ((equal? cont (+ n (* n 3))) matriz)
    ((< cont n) (AuxCrearMatriz n (+ cont 1) (append matriz (list (CrearListaEspecialV1 n 4)))))
    ((< cont (* n 2)) (AuxCrearMatriz n (+ cont 1) (append matriz (list (CrearListaEspecialV1 n 3)))))
    ((< cont (* n 3)) (AuxCrearMatriz n (+ cont 1) (append matriz (list (CrearListaEspecialV3 n 5 2 6)))))
    (else
     (AuxCrearMatriz n (+ cont 1) (append matriz (list (CrearListaEspecialV1 n 1))))
     )
   )
  )




;(define (MovVert1 n columna matriz)
;  (AuxMovVert1 n columna 0 n matriz) ;n, columna a cambiar, contador de filas, contador de columnas temp (va de n hasta (n*2-1)),
;  )

;n, columna a cambiar, contador de filas, contador de columnas, matriz
;(define (AuxMovVert1 n columna contfila contcolumna matriz)
;  (cond
;    ((equal? contfila (+ (* n 3) n)) matriz)
;    ((equal? columna contcolumna))
;     
;    )
;  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;extrae un elemento de una matriz segun la fila y columna dada
(define (ExtraerElemMatriz fila columna matriz)
  (AuxExtraerElemMatriz fila columna 0 matriz )
  )

(define (AuxExtraerElemMatriz fila columna contfila matriz)
  (cond
    ((equal? fila contfila) (ExtraerElemLista columna 0 (car matriz)))
    (else
     (AuxExtraerElemMatriz fila columna (+ contfila 1) (cdr matriz))
     )
    )
  )
     
;extrae un elemento de una lista segun el indice dado
(define (ExtraerElemLista indice contindice lista)
  (cond
    ((equal? indice contindice) (car lista))
    (else
     (ExtraerElemLista indice (+ contindice 1) (cdr lista))
     )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Toma una matriz y la retorna con cada fila en reversa
(define (ReversarFilas matriz)
  (AuxReversarFilas matriz '())
  )

(define (AuxReversarFilas matriz nuevamatriz)
  (cond
    ((null? matriz) nuevamatriz)
    (else
     (AuxReversarFilas (cdr matriz) (append nuevamatriz (list (ReversarLista (car matriz) '()))))
     )
    )
  )


;Toma una lista y la retorna en reversa
(define (ReversarLista lista nuevalista)
  (cond
    ((null? lista) nuevalista)
    (else
     (ReversarLista (cdr lista) (cons (car lista) nuevalista))
     )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Toma una matriz y convierte todas las columnas en filas
(define (ConvertirColumnasAFilas matriz)
  (AuxConvertirColumnasAFilas matriz 0 '())
  )

(define (AuxConvertirColumnasAFilas matriz contindice nuevamatriz)
  (cond
    ((equal? (contarelementos matriz) contindice) nuevamatriz)
    (else
     (AuxConvertirColumnasAFilas matriz (+ contindice 1) (append nuevamatriz (list (CrearFila matriz contindice 0 '()))))
     )
    )
  )

;Convierte en fila una columna según el indice dado (solo retorna 1 columna convertida en fila)
;indicetemp inicial es 0 y fila inicial es ´()
(define (CrearFila matriz indice indicetemp  fila)
  (cond
    ((equal? matriz '()) fila)
    ((equal? indice indicetemp) (CrearFila (cdr matriz) indice 0  (append fila (list (car (car matriz ))))))
    (else
     (CrearFila (append (list (cdr (car matriz))) (cdr matriz)) indice (+ indicetemp 1)  fila)
     )
    )
  )

;cuenta los elementos de una lista
(define (contarelementos lista)
  (cond
    ((null? (cdr lista)) 1)
    (else
     (+ 1 (contarelementos (cdr lista)))
     )
    )
  )

;pruebas:
;(ConvertirColumnasAFilas '((1 2 3) (4 5 6) (7 8 9)))
;(CrearFila '((1 2 3) (4 5 6) (7 8 9)) 2 0  '())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;extrae la matriz del lado izquierdo de la matriz del cubo 
(define (GetMatrizIzquierda matriz n)
  (AuxGetMatriz matriz n 0 0 0 '() '())
  )
  
;extrae la matriz del lado derecho de la matriz del cubo   
(define (GetMatrizDerecha matriz n)
  (AuxGetMatriz matriz n 0 0 (* n 2) '() '())
  )

;extrae la matriz del centro de la matriz del cubo   
(define (GetMatrizCentro matriz n)
  (AuxGetMatriz matriz n 0 0 n '() '())
  )

;auxiliar de GetMatriz, solamente recine una posición donde iniciar a extraer.
;Si la posición es cero entonces extrae la matriz izquierda y si es n*2 extrae la derecha  y si es n extrae la del centro               
(define (AuxGetMatriz matriz n contfilas contcolumnas posicioninicial listatemp nuevamatriz )
  (cond
    ((equal? contfilas (* n 3)) nuevamatriz)
    ((>= contcolumnas (+ posicioninicial n)) (AuxGetMatriz (cdr matriz) n (+ contfilas 1 ) 0 posicioninicial '() (append nuevamatriz (list listatemp)) )) ;suma 1 a fila, pasa a 0 el contcolumns add listatemp
    ((< contfilas (* n 2)) (AuxGetMatriz (cdr matriz) n (+ contfilas 1) contcolumnas posicioninicial listatemp nuevamatriz ))
    
    ((>= contcolumnas posicioninicial) (AuxGetMatriz (cons (cdr (car matriz)) (cdr matriz)) n contfilas (+ contcolumnas 1) posicioninicial (append listatemp (list (car (car matriz)))) nuevamatriz ))
    (else
     (AuxGetMatriz (cons (cdr (car matriz)) (cdr matriz)) n contfilas (+ contcolumnas 1) posicioninicial listatemp nuevamatriz )
     
     )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Pega la matriz izquierda
(define (PegarMatrizIzquierda  matrizmayor matrizmenor n)
  (AuxPegarMatriz  matrizmayor matrizmenor n 0 0 0 '() '()  )
  )

;Pega la matriz derecha
(define (PegarMatrizDerecha  matrizmayor matrizmenor n)
  (AuxPegarMatriz  matrizmayor matrizmenor n (* n 2) 0 0 '() '()  )
  )

;Auxiliar de pegar matriz. 
(define (AuxPegarMatriz  matrizmayor matrizmenor n posicioninicial contfilas contcolumnas  listatemp nuevamatriz )
  (cond
    ((equal? contfilas (* n 4)) nuevamatriz)
    ((and
      (< contfilas (* n 2))
      (>= contfilas (* n 3))) (AuxPegarMatriz  (cdr matrizmayor) matrizmenor n posicioninicial (+ contfilas 1) contcolumnas listatemp (append nuevamatriz (list (car matrizmayor) ))))

    ((>= contcolumnas (* n 3)) (AuxPegarMatriz  (cdr matrizmayor) (cdr matrizmenor) n posicioninicial (+ contfilas 1) 0 '() (append nuevamatriz (list listatemp)))) ;pasa a nueva fila reinicia todo


    ((and
      (< contcolumnas n)
      (equal? posicioninicial 0)) (AuxPegarMatriz  (append (list (cdr (car matrizmayor))) (list (cdr matrizmayor))) (append (list (cdr (car matrizmenor))) (list (cdr matrizmenor)))
                                                    n posicioninicial contfilas (+ 1 contcolumnas) (append listatemp (list (car (car matrizmenor)))) nuevamatriz ))   ;voy agregando lo que ya estaba

    ((and
      (>= contcolumnas (* n 2))
      (>= posicioninicial (* n 2))) (AuxPegarMatriz  (append (list (cdr (car matrizmayor))) (list (cdr matrizmayor))) (append (list (cdr (car matrizmenor))) (list (cdr matrizmenor)))
                                                    n posicioninicial contfilas (+ 1 contcolumnas) (append listatemp (list (car (car matrizmenor)))) nuevamatriz ))
    
    
    (else
     (AuxPegarMatriz  (append (list (cdr (car matrizmayor))) (list (cdr matrizmayor))) matrizmenor
                                                    n posicioninicial contfilas (+ 1 contcolumnas) (append listatemp (list (car (car matrizmayor)))) nuevamatriz )
     )
    )
  )
   ;(append (list (cdr (car (list '(1 2 3) '(1 2 3) '(1 2 3))))) (list (cdr (list '(1 2 3) '(1 2 3) '(1 2 3)))))
   ;(append (list (cdr (car (list '() '(1 2 3) '(1 2 3))))) (list (cdr (list '(1 2 3) '(1 2 3) '(1 2 3)))))

(CrearMatriz 4)


;Pruebas:
;(GetMatrizDerecha (CrearMatriz 4) 4)
;(GetMatrizDerecha (CrearMatriz 4) 4)

;(PegarMatrizIzquierda  (CrearMatriz 4) (GetMatrizDerecha (CrearMatriz 4) 4) 4)


;(ExtraerElemMatriz 2 7 '((0 0 0 0 1 2 3 4 0 0 0 0) ( 0 0 0 0 4 5 6 4 0 0 0 0) (0 0 0 0 7 8 9 4 0 0 0 0)))
;(ExtraerElemLista 4 0 '(2 3 4 5 6))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






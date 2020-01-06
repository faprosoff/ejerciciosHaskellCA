-- PARCIAL --

{- EJERCICIO 1
   Dada una lista de enteros L1 y una lista L2, devolver una lista formada por listas de elementos tomados sucesivamente de L2
   de modo que sus longitudes respectivas sean los sucesivos valores de L1 en orden de aparición.
   EJEMPLOS:
   ej1 ([3,2,3], "abracadabra") -> ["abr", "ac", "ada"]
   ej1 ([1,2,4,0], "abracadabra") -> ["a", "br", "acad", ""] -}

ej1 :: ([Int], [a]) -> [[a]]
ej1 ([], y) = []
ej1 (x, []) = []
ej1 (x:xs, y) = dameListaHasta(y,x) : ej1(xs, dameListaDesde(y, x+1))

dameListaHasta :: ([a], Int) -> [a]
dameListaHasta ([], y) = []
dameListaHasta (x, 0) = []
dameListaHasta ((x:y), n) = x : (dameListaHasta (y, n-1))

dameListaDesde :: ([a], Int) -> [a]
dameListaDesde ([], y) = []
dameListaDesde (x, 1) = x
dameListaDesde ((x:y), n) = dameListaDesde (y, n-1)

{- EJERCICIO 2
   Dada una matriz, de cualquier tamaño y contenido representada por filas de manera habitual, y dado un entero i, devolver la
   matriz que resulta igual a la matriz anterior con la excepción de la columna i-ésima, la cual contendrá todos elementos
   iguales al elemento de la posición (0,0) de la matriz.
   EJEMPLO: ej2 ([[4,3,2], [1,0,3]], 1) -> [[4,4,2], [1,4,3]] 
            ej2 ([[1,2,3],[4,5,6],[7,8,9]], 2) -> [[1,1,3],[4,1,6],[7,1,9]]-}

ej2 :: ([[a]], Int) -> [[a]]
ej2 ([], n) = []
ej2 (x:y, n) = reemplazarColumnaXElem (x:y, cabeza x, n)

reemplazarColumnaXElem :: ([[a]], a, Int) -> [[a]]
reemplazarColumnaXElem ([], e, n) = []
reemplazarColumnaXElem (x:y, e, n) = reemplazarFilaXElem (x, e, n) : reemplazarColumnaXElem (y, e, n)

reemplazarFilaXElem :: ([a], a, Int) -> [a]
reemplazarFilaXElem ([], e, n) = []
reemplazarFilaXElem (x:y, e, 1) = e : reemplazarFilaXElem(y, e, n-1)
reemplazarFilaXElem (x:y, e, n) = x : reemplazarFilaXElem(y, e, n-1)

cabeza :: [a] -> a
cabeza (x:y) = x

{- EJERCICIO 3
   Dada una matriz de float de cualquier tamaño representada por filas de manera habitual, devolver la suma de los promedios
   por columna teniendo en cuenta sólo las columnas que no repitan valores.
   EJEMPLO: ej3 [[3,1,1], [5,9,2], [4,1,6]] -> 7 (= 4 + 3) -}

ej3 :: [[Float]] -> Float
ej3 [] = 0
ej3 ([]:y) = 0
ej3 x = (if noRepiteValor(cabezaListas x) then promedio(cabezaListas x) else 0) + ej3 (colaListas x)

noRepiteValor :: [Float] -> Bool
noRepiteValor [] = True
noRepiteValor (x:y) = if encontre (x,y) then False else noRepiteValor y

encontre :: (Float, [Float]) -> Bool
encontre (e, []) = False
encontre (e, x:y) = if x == e then True else encontre (e, y)

promedio :: [Float] -> Float
promedio [] = 0
promedio x = suma x / long x

suma :: [Float] -> Float
suma [] = 0
suma (x:y) = x + suma y

long :: [a] -> Float
long [] = 0
long (x:y) = 1 + long y

cabezaListas :: [[a]] -> [a]
cabezaListas [] = []
cabezaListas (x:y) = cabeza x : cabezaListas y

colaListas :: [[a]] -> [[a]]
colaListas [] = []
colaListas (x:y) = cola x : colaListas y

cola :: [a] -> [a]
cola (x:y) = y
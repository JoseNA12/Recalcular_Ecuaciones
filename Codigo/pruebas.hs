{--1.	Definir un tipo de datos Arbol que permita almacenar las expresions:
en las hojas vienen constantes o el nombre de una variable, y en los nodos 
internos viene una función binaria de enteros (+,-,*) y los dos subárboles.--}

type IntFunc = Int -> Int -> Int
--data Hoja = String | Int deriving (Show)
data Arbol = Hoja String | Nodo String (Arbol) (Arbol) deriving (Show)

{--2.	Elabore una función crearArbol que tome una tira de caracteres con una 
operación binaria simple ("operando  operación  operando") y devuelva un árbol 
como del punto 1:--}

crearArbol :: [String] -> Arbol
crearArbol ec = if length (ec) == 1 
    then Hoja (head ec)
    else Nodo (ec !!1) (crearArbol [head ec]) (crearArbol [last ec])

obtOperacion :: String -> Int
obtOperacion op = case op of 
             "+" ->  1--(+)
             "-" ->  2--(-)
             "*" ->  3--(*)


{--3.	Elabore una función sustVar que tome una variable y un Arbol que representa 
su ecuación y sustituya en otro Arbol las apariciones de esa variable por copias del 
Arbol asociado.:--}

--sustVar "b" (Nodo "+" (Hoja "a") (Hoja "2")) (Nodo "*" (Hoja "b") (Hoja "p"))

--			 var    ecuación   a mod
sustVar :: String -> Arbol -> Arbol -> Arbol --[String]
sustVar variable (Hoja valor_1) (Hoja valor_2)


nHojas :: Arbol -> Int
nHojas (Hoja _) = 1
nHojas (Nodo x i d) = nHojas i + nHojas d

{--4.	Elabore una función listaVar que tome un Arbol y devuelva una lista con las 
variables que aparecen en dicho árbol; cada variable debe aparecer una sola vez en 
la lista:--}

--listaVar (Nodo "*" ((Nodo "+" (Hoja "a") (Nodo "+" (Hoja "2") (Hoja "c")))) (Hoja "z"))
--Árbol para (a+(2+c))*z> produce ["a","c","z"]

listaVar2 :: Arbol -> [String]
listaVar2 (Hoja valor) = if (esInt valor) == False
                         then [valor]
                         else [""]
listaVar2 (Nodo raiz izq der) = quitarRep(quitarEsp(valores))
       where valores = listaVar2 izq ++ listaVar2 der


listaVar :: Arbol -> [String]
listaVar (Hoja valor) = [valor] 
listaVar (Nodo raiz izq der) --head para obtener el string, ["a"] -> "a"
    |(esInt (head (listaVar izq)) == False) && (esInt (head (listaVar der)) == False) 
                     = head [listaVar izq] ++ head [listaVar der]
    |(esInt (head (listaVar izq)) == False) = head [listaVar izq]
    |(esInt (head (listaVar der)) == False) = head [listaVar der]
    |otherwise = [""]

esInt var = case reads var :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

quitarEsp :: [String] -> [String]
quitarEsp [] = []
quitarEsp lista = [x | x <- lista, x `notElem` [""]]

quitarRep :: [String] -> [String]
quitarRep [] = []
quitarRep [x] = [x]
quitarRep (x:xs) = x : [k  | k <- quitarRep (xs), k /= x]

--quitarRep(quitarEsp(listaVar()))


{--5.	Elabore una función evalArb que tome un Arbol y una lista de valores, 
y devuelva el resultado de evaluar dicho Arbol usando esos valores; los valores 
se asocian con las variables siguiendo el orden especificado por el resultado 
de listaVar para ese Arbol:--}

--evalArb (Nodo "*" ((Nodo "+" (Hoja "a") (Nodo "+" (Hoja "2") (Hoja "c")))) (Hoja "z")) (enlazarValores ["a", "c", "z"] [1, 2, 3])
--evalArb (Nodo "*" ((Nodo "+" (Hoja "a") (Nodo "+" (Hoja "2") (Hoja "c")))) (Hoja "z")) (enlazarValores (listaVar (Nodo "*" ((Nodo "+" (Hoja "a") (Nodo "+" (Hoja "2") (Hoja "c")))) (Hoja "z"))) [1, 2, 3])
--evalArb (Nodo "*" ((Nodo "+" (Hoja "a") (Nodo "+" (Hoja "e") (Hoja "c")))) (Hoja "z")) (enlazarValores (listaVar (Nodo "*" ((Nodo "+" (Hoja "a") (Nodo "+" (Hoja "e") (Hoja "c")))) (Hoja "z"))) [3, 2, 2, 2])

--evalArb Arbol (enlazarValores (listaVar Arbol) [1, 2, 3])

evalArb :: Arbol -> [(String, Int)] -> Int
evalArb (Hoja valor) tupla = if (esInt valor) == False --si es variable/incognita
                                    then (obtenerValor valor tupla) --devuelva el valor asosiado de la tupla: "b" [("a", 1), ("b", 2)]
                                    else (convAInt valor) --si es numero dejelo asi y conviertalo en Int
evalArb (Nodo operador izq der) tupla
   |operador == "+" = (+) (evalArb izq tupla) (evalArb der tupla)
   |operador == "-" = (-) (evalArb izq tupla) (evalArb der tupla)
   |operador == "*" = (*) (evalArb izq tupla) (evalArb der tupla)

convAInt :: String -> Int
convAInt "" = 0
convAInt caracter = read (caracter) :: Int

enlazarValores :: [String] -> [Int] -> [(String, Int)] --enlazarValores ["a", "b", "c"] [1, 2, 3]
enlazarValores (x:xs) (y:ys)
    |length (xs) == 0 = [(x, y)]
    |length (x:xs) == length (y:ys) = [(x, y)] ++ (enlazarValores xs ys)
    |otherwise = [("error", 0)]

obtenerValor :: String -> [(String, Int)] -> Int
obtenerValor var tupla = head [y | (x, y) <- tupla, x == var] -- obtenerValor "a" [("b", 5), ("a", 2)]


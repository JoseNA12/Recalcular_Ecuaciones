{--1.	Definir un tipo de datos Arbol que permita almacenar las expresions:
en las hojas vienen constantes o el nombre de una variable, y en los nodos 
internos viene una función binaria de enteros (+,-,*) y los dos subárboles.--}

type ValoresFunc = String->String->Int
data Arbol = Hoja String | Nodo String (Arbol) (Arbol) deriving (Show, Eq)


{--2.	Elabore una función crearArbol que tome una tira de caracteres con una 
operación binaria simple ("operando  operación  operando") y devuelva un árbol 
como del punto 1:--}

crearArbol :: String -> Arbol
crearArbol ec = if length (words ec) == 1 
    then Hoja ec
    else Nodo (words ec !!1) (crearArbol (words ec !!0)) (crearArbol (words ec !!2))


{--3.	Elabore una función sustVar que tome una variable y un Arbol que representa 
su ecuación y sustituya en otro Arbol las apariciones de esa variable por copias del 
Arbol asociado.:--}

--sustVar "b" (Nodo "+" (Hoja "a") (Hoja "2")) (Nodo "*" (Hoja "b") (Hoja "p"))

--			var    ecuación   a mod
sustVar :: String -> Arbol -> Arbol -> Arbol -- [String]
sustVar variable (Hoja valor_1) (Hoja valor_2) = Hoja valor_2
sustVar variable (Nodo raiz_1 izq_1 der_1) (Nodo raiz_2 izq_2 der_2)
    |variable == raiz_2 = (Nodo variable (Hoja "p") (Hoja "f"))

    --(Nodo variable (Hoja "p") (Hoja "f"))


{--4.	Elabore una función listaVar que tome un Arbol y devuelva una lista con las 
variables que aparecen en dicho árbol; cada variable debe aparecer una sola vez en 
la lista:--}

--listaVar (Nodo "*" ((Nodo "+" (Hoja "a") (Nodo "+" (Hoja "2") (Hoja "c")))) (Hoja "z"))
--Árbol para (a+(2+c))*z> produce ["a","c","z"]

esInt s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

--remove elemento lista = filter (\e -> e /= elemento) lista

--VALIDAR ESPACIOS
listaVar :: Arbol -> [String]
listaVar (Hoja valor) = [valor] 
listaVar (Nodo raiz izq der) --head para obtener el string, ["a"] -> "a"
    |(esInt (head (listaVar izq)) == False) && (esInt (head (listaVar der)) == False) 
                     = head [listaVar izq] ++ head [listaVar der]
    |(esInt (head (listaVar izq)) == False) = head [listaVar izq]
    |(esInt (head (listaVar der)) == False) = head [listaVar der]
    |otherwise = [""]

quitarEspacios :: [String] -> [String]
quitarEspacios (x:xs)
    |length (xs) <= 0 = if [x]!!((length [x])-1) == "" then take ((length [x])-2) [x] else [x] --agarraba el ultimo sin importar el valor
    |x == "" = quitarEspacios xs
    |otherwise = [x] ++ quitarEspacios xs

quitarRepetidos :: [String] -> [String]
quitarRepetidos (x:xs)
    |length (xs) <= 0 = [x]
    |elem x xs == True = quitarRepetidos xs
    |otherwise = [x] ++ quitarRepetidos xs

--quitarRepetidos(quitarEspacios(listaVar()))


{--5.	Elabore una función evalArb que tome un Arbol y una lista de valores, 
y devuelva el resultado de evaluar dicho Arbol usando esos valores; los valores 
se asocian con las variables siguiendo el orden especificado por el resultado 
de listaVar para ese Arbol:--}

--evalArb :: Arbol -> [Int] -> Int
--evalArb (Hoja valor) = [valor]
 
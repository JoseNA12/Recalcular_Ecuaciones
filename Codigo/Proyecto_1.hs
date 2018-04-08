
-- Ejemplo de un ciclo interactivo en Haskell

-- El ciclo principal recibe un estado [(String,String)],
-- en cada iteración del ciclo principal, se lee una línea,
-- la cual se interpreta como si fuera un comando.
--    si empieza con "def"
--       generar un nuevo estado agregando al estado
--       anterior un nuevo par formado por la segunda y
--       tercera palabra del comando:
--       >> def a saludo
--    si empieza con "borrar"
--       eliminar del estado el primer par cuyo primer
--       componente sea igual a la segunda palabra del comando
--       >> borrar a
--    si empieza con "imp"
--       imprimir estado actual (print)
--       >> imp
--    si empieza con "fin"
--       terminar el ciclo
--       >> fin
--
-- Luego de interpretar el comando, se invoca recursivamente 
-- el ciclo principal con el nuevo estado si es del caso.

type Estado = [(String, [String], Arbol)] --, [String], Arbol)]

type IntFunc = Int -> Int -> Int
--data Hoja = String | Int deriving (Show)
data Arbol = Hoja String | Nodo String (Arbol) (Arbol) deriving (Show)

main :: IO ()
main = do 
       mainloop [] -- Ejecutar mainloop con estado inicial nulo

-- mainloop es una función que recibe un Estado y  
-- retorna una acción la cual al ser ejecutada no retorna nada, 
-- pero puede tener efectos colaterales (lee, modifica, escribe)
mainloop :: Estado -> IO ()
mainloop estado = do
    -- putStr::String -> IO (), es una función que recibe una tira y 
    -- devuelve una acción, la cual al ser ejecutada imprime la tira 
    -- (efecto colateral) y retorna nada
    putStr ">> "
    
    -- getLine es una función que retorna una acción que al ser ejecutada
    -- retorna una tira; la construcción "<-" ejecuta la acción de getLine
    -- y extrae la tira leída    
    inpStr <- getLine
    
    -- procesar es una función "pura" que toma la tira de entrada y el estado,
    -- y devuelve una tripleta:
    --     Bool terminar que indica si se debe terminar el ciclo principal (fin)
    --     Estado nuevoestado obtenido al ejecutar el comando a partir del estado actual
    --     String salida con un texto que se imprime como resultado del comando    
    let (terminar,nuevoestado,salida) = procesar inpStr estado
    
    -- impresión de la salida provocada por la ejecución del comando
    putStrLn salida
    
    -- terminar el ciclo si el comando así lo indica
    -- en caso contrario usar recursión de cola para obtener 
    -- el siguiente comando con el nuevo estado
    if terminar
       then return ()  -- return crea una acción que al ser ejecutada no retorna nada
                       -- es lo que se supone que debe devolver mainloop
       else mainloop nuevoestado

-- procesar recibe un comando y un estado y calcula la tripleta que
-- se produce al ejecutar el comando y afectar al estado 
procesar :: String -> Estado -> (Bool, Estado, String)
procesar comando estado =
     -- tokens es la línea de comando separada en palabras
     -- dependiendo de la primera palabra se invoca a la
     -- función que implementa dicho comando
     case tokens!!0 of
          "ie" -> ie (tail tokens) estado
          --"borrar" -> cmd_borrar (tail tokens) estado
          "imp" -> cmd_imp estado 
          -- comando fin: retornar tripleta que finaliza ciclo          
          "fin" -> (True, estado, "Saliendo...")
          _     -> cmd_desconocido (tokens!!0) comando estado
       where tokens = words comando --["x","=","2","+","3"]

-- función que implementa el comando def
--   crea nuevo estado agregando un nuevo par
{--cmd_def::[String] -> Estado -> (Bool, Estado, String)
cmd_def tokens estado = (False, nuevoestado, mensaje)
       where nuevoestado = estado ++ [(tokens!!0,tokens!!1)]
             mensaje = "Definido " ++ tokens!!0-}

-- función que implementa el comando borrar
--   
{--cmd_borrar::[String] -> Estado -> (Bool, Estado, String)
cmd_borrar [] estado = (False, estado, "No se especificó qué borrar")
cmd_borrar (v:_) estado = let (res, nuevoestado) = borrar v estado
                             in if res
                                  then (False, nuevoestado, v ++ " borrado")
                                  else (False, estado, v ++ " no estaba definido")--}


-- función que busca un nombre en el estado
-- 
{--buscar :: String -> Estado -> (Bool, String)
buscar _ [] = (False, "")
buscar v1 ((v2,y):estado) = if v1 == v2
                               then (True,y)
                               else  buscar v1 estado--}

-- función que elimina un par del estado
-- 
{--borrar :: String -> Estado -> (Bool, Estado)
borrar _ [] = (False, [])
borrar v1 ((v2,y):estado) = let (res,nuevoestado) = borrar v1 estado
                                 in if v1 == v2
                                      then (True,estado)
                                      else  (res, (v2,y):nuevoestado)--}


-- función que maneja un comando desconocido
--
cmd_desconocido :: String -> String -> Estado -> (Bool, Estado, String)
cmd_desconocido cmd comando estado = (False, estado, mensaje)
       where mensaje = "Comando desconocido ("++ cmd ++"): '" ++ comando ++ "'"

-- función que implementa el comando imp
--
cmd_imp :: Estado -> (Bool, Estado, String)
cmd_imp estado = (False, estado, show estado)







--Insertar ecuacion (ie)
{--
ie :: [String] -> Estado -> (Bool, Estado, String)
ie tokens estado = (False, nuevoestado, mensaje)
       where nuevoestado = estado ++ [(tokens!!0, quitarRep(quitarEsp(listaVar(crearArbol(drop 2 tokens)))), crearArbol(drop 2 tokens))] --Variable, Lista variables, Arbol
             mensaje = "Se definido " ++ tokens!!0
--}

ie :: [String] -> Estado -> (Bool, Estado, String)
ie tokens estado 
    | (valid_1_ie (tokens!!0) (drop 2 tokens)) == True = (False, estado, "Error, la incógnita '" ++ tokens!!0 ++ "' se encuentra en la expresión!.")
    | (valid_2_ie (tokens!!0) estado) == True = (False, estado, "Error, la incógnita '" ++ tokens!!0 ++ "' ya se encuentra definida!.")
    | ()
    | otherwise = (False, nuevoestado, mensaje)
       where nuevoestado = estado ++ [(tokens!!0, quitarRep(quitarEsp(listaVar(crearArbol(drop 2 tokens)))), crearArbol(drop 2 tokens))] --Variable, Lista variables, Arbol
             mensaje = "Se definió " ++ tokens!!0


--quitarRep(quitarEsp(listaVar(crearArbol(drop 4 tokens))))
--quitarRep(quitarEsp(listaVar(crearArbol(drop 4 (tokens!!0)))))
--quitarRep(quitarEsp(listaVar(crearArbol(drop 4 tokens))))

valid_1_ie :: String -> [String] -> Bool -- x = x + 2
valid_1_ie var ec = if var `elem` ec
                  then True
                  else False

valid_2_ie :: String -> Estado -> Bool  -- (x = y + b), (x = 2 * q)
valid_2_ie var estado
    | length(estado) <= 0 = False
    | var == (fst3 (head estado)) = True
    | otherwise = valid_2_ie var (tail estado)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

valid_3_ie :: String -> Estado -> Bool  -- (x = y + b), (x = 2 * q)
valid_3_ie var estado
    | length(estado) <= 0 = False
    | var == (fst3 (head estado)) = True
    | otherwise = valid_2_ie var (tail estado)

--Mostrar-variable (mv):

--Mostrar-ambiente (ma):

--Calcular-variable (cv):

--Calcular-variable-original (cvo):

--Mostrar-parámetros (mp):

--Evaluar-todo (et):

--Terminar (fin)



{--2. Elabore una función crearArbol que tome una tira de caracteres con una 
operación binaria simple ("operando  operación  operando") y devuelva un árbol 
como del punto 1:--}

crearArbol :: [String] -> Arbol
crearArbol ec = if length (ec) == 1 
    then Hoja (head ec) --["a"] -> "a"
    else Nodo (ec !!1) (crearArbol [head ec]) (crearArbol [last ec])

obtOperacion :: String -> Int
obtOperacion op = case op of 
             "+" ->  1--(+)
             "-" ->  2--(-)
             "*" ->  3--(*)


{--3. Elabore una función sustVar que tome una variable y un Arbol que representa 
su ecuación y sustituya en otro Arbol las apariciones de esa variable por copias del 
Arbol asociado.:--}

--sustVar "b" (Nodo "+" (Hoja "a") (Hoja "2")) (Nodo "*" (Hoja "b") (Hoja "p"))

--       var    ecuación   a mod    a mod
sustVar :: String -> Arbol -> Arbol -> Arbol -> Arbol --[String]
sustVar variable (Hoja valor_1) (Hoja valor_2) (Hoja valor_3) = if ((variable == valor_2) || (variable == valor_3)) == True
    then (Hoja valor_3) 
    else (Hoja valor_2)
sustVar variable (Nodo raiz_1 izq_1 der_1) (Nodo raiz_2 izq_2 der_2) (Nodo raiz_3 izq_3 der_3)
    |variable == raiz_2 = (Nodo raiz_1 izq_1 der_1)
    |otherwise = sustVar variable (Nodo raiz_1 izq_1 der_1) izq_2 der_2

nHojas :: Arbol -> Int
nHojas (Hoja _) = 1
nHojas (Nodo x i d) = nHojas i + nHojas d


{--4. Elabore una función listaVar que tome un Arbol y devuelva una lista con las 
variables que aparecen en dicho árbol; cada variable debe aparecer una sola vez en 
la lista:--}

--listaVar (Nodo "*" ((Nodo "+" (Hoja "a") (Nodo "+" (Hoja "2") (Hoja "c")))) (Hoja "z"))
--Árbol para (a+(2+c))*z> produce ["a","c","z"]

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


{--5. Elabore una función evalArb que tome un Arbol y una lista de valores, 
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


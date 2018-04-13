
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

type Estado = [(String, [String], Arbol, [String], Arbol)]

type Ecuacion = Int -> Int -> Int
data Termino = Variable String | Entero Int deriving (Eq)
data Arbol = Hoja Termino | Nodo Ecuacion Arbol Arbol --deriving (Show)


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
          "mv" -> cmd_mv (tail tokens) estado
          "ma" -> cmd_ma (tail tokens) estado
          --"cv" ->
          "cvo" -> cmd_cvo (tail tokens) estado
          --"mp" ->
          --"et" ->
          -- comando fin: retornar tripleta que finaliza ciclo
          "fin" -> (True, estado, "Saliendo...")
          _     -> cmd_desconocido (tokens!!0) comando estado
       where tokens = words comando --["x","=","2","+","3"]
--}
-- función que implementa el comando def
--   crea nuevo estado agregando un nuevo par
{--cmd_def::[String] -> Estado -> (Bool, Estado, String)
cmd_def tokens estado = (False, nuevoestado, mensaje)
       where nuevoestado = estado ++ [(tokens!!0,tokens!!1)]
             mensaje = "Definido " ++ tokens!!0--}

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
buscar :: String -> Estado -> (Bool, Estado)
buscar _ []  = (False, [("", [""], (Nodo (+) (Hoja (Variable "")) (Hoja (Variable ""))), [""], (Nodo (+) (Hoja (Variable "")) (Hoja (Variable ""))))])
buscar v1 ((v2, y, q, w, p):estado) = if v1 == v2
                                         then (True, [(v2 ,y, q, w, p)])
                                         else buscar v1 estado
                                     
--Buscar primero con la funcion de arriba, y luego obtener los datos especificos
buscarLstOrig :: String -> Estado -> [String]
buscarLstOrig _ []  = [""]
buscarLstOrig v1 ((v2, y, q, w, p):estado) = if v1 == v2
                                         then y
                                         else buscarLstOrig v1 estado

buscarArbOrig :: String -> Estado -> Arbol
buscarArbOrig _ []  = (Nodo (+) (Hoja (Variable "")) (Hoja (Variable "")))
buscarArbOrig v1 ((v2, y, q, w, p):estado) = if v1 == v2
                                         then q
                                         else buscarArbOrig v1 estado

buscarLstVig :: String -> Estado -> [String]
buscarLstVig _ []  = [""]
buscarLstVig v1 ((v2, y, q, w, p):estado) = if v1 == v2
                                         then w
                                         else buscarLstVig v1 estado

buscarArbVig :: String -> Estado -> Arbol
buscarArbVig _ []  = (Nodo (+) (Hoja (Variable "")) (Hoja (Variable "")))
buscarArbVig v1 ((v2, y, q, w, p):estado) = if v1 == v2
                                         then p
                                         else buscarArbVig v1 estado
{--   
-- función que elimina un par del estado
-- 
{--borrar :: String -> Estado -> (Bool, Estado)
borrar _ [] = (False, [])
borrar v1 ((v2,y):estado) = let (res,nuevoestado) = borrar v1 estado
                                 in if v1 == v2
                                      then (True,estado)
                                      else  (res, (v2,y):nuevoestado)--}


--}
-- función que maneja un comando desconocido
--
cmd_desconocido :: String -> String -> Estado -> (Bool, Estado, String)
cmd_desconocido cmd comando estado = (False, estado, mensaje)
       where mensaje = "Comando desconocido ("++ cmd ++"): '" ++ comando ++ "'"
{--
-- función que implementa el comando imp
--
cmd_imp :: Estado -> (Bool, Estado, String)
cmd_imp estado = (False, estado, show estado)






--}
--Insertar ecuacion (ie)
ie :: [String] -> Estado -> (Bool, Estado, String)
ie tokens estado 
    | (valid_Fort_ie tokens) == True = (False, estado, "Error, ecuación inválida!.")
    | (valid_1_ie (tokens!!0) (drop 2 tokens)) == True = (False, estado, "Error, la incógnita '" ++ tokens!!0 ++ "' se encuentra en la expresión!.")
    | (valid_2_ie (tokens!!0) estado) == True = (False, estado, "Error, la incógnita '" ++ tokens!!0 ++ "' ya se encuentra definida!.")
    | (valid_3_ie (tokens) estado) == True = (False, estado, "Error, la incógnita '" ++ tokens!!0 ++ "' produce un ciclo!.")
    | otherwise = (False, nuevoestado, (formatEst [(last nuevoestado)]))
       where nuevoestado = estado ++ [(tokens!!0, listaVar(crearArbol(drop 2 tokens)), crearArbol(drop 2 tokens), listaVar(crearArbol(drop 2 tokens)), (Nodo (+) (Hoja (Variable "")) (Hoja (Variable ""))))] --Variable, Lista variables, Arbol
             --mensaje = "Se definió " ++ tokens!!0

--Rechazar la ecuación si tiene errores sintácticos en ecuación 
valid_Fort_ie :: [String] -> Bool
valid_Fort_ie tokens
    | length(tokens) == 0 = True
    | length(tokens) /= 5 = True
    | esInt(head tokens) = True
    | (tokens!!3 /= "+") && (tokens!!3 /= "-") && (tokens!!3 /= "*") = True
    | otherwise = False

--La variable a la izquierda de la ecuación no debe tener una ecuación previa
valid_1_ie :: String -> [String] -> Bool -- x = x + 2
valid_1_ie var ec = if var `elem` ec
                  then True
                  else False

--La variable a la izquierda de la ecuación no debe aparecer a la derecha de esa misma ecuación
valid_2_ie :: String -> Estado -> Bool  -- (x = y + b), (x = 2 * q)
valid_2_ie var estado
    | length(estado) <= 0 = False
    | var == (fstEstado (head estado)) = True
    | otherwise = valid_2_ie var (tail estado)

--o la nueva ecuación no debe formar un ciclo de dependencias con las ecuaciones anteriores; 
--esto es para cada una de las ecuaciones anteriores, revisar que la variable de la nueva ecuación 
--no aparece a la derecha de una ecuación anterior y que la variable de esa ecuación anterior no 
--aparece a la derecha de la nueva ecuación
valid_3_ie :: [String] -> Estado -> Bool -- (x = 2 - y), (y = x * 6)
valid_3_ie tokens estado
    | length(estado) <= 0 = False
    | ((tokens!!0) `elem` (frhEstado (head estado))) && ((fstEstado (head estado)) `elem` (drop 2 tokens)) = True
    | otherwise = valid_3_ie tokens (tail estado)


fstEstado :: (var, lista1, arbol1, lista2, arbol2) -> var
fstEstado (x, _, _, _, _) = x

scdEstado:: (var, lista1, arbol1, lista2, arbol2) -> lista1
scdEstado (_, x, _, _, _) = x

trdEstado:: (var, lista1, arbol1, lista2, arbol2) -> arbol1
trdEstado (_, _, x, _, _) = x

frhEstado :: (var, lista1, arbol1, lista2, arbol2) -> lista2
frhEstado (_, _, _, x, _) = x

fveEstado :: (var, lista1, arbol1, lista2, arbol2) -> arbol2
fveEstado (_, _, _, _, x) = x


--Mostrar-variable (mv):
cmd_mv :: [String] -> Estado -> (Bool, Estado, String)
cmd_mv tokens estado
    | length(tokens) <= 0 = (False, estado, "Error, ingrese la incognita!.")
    | length(tokens) /= 1 = (False, estado, "Error, debe ingresar solo una incognita!.")
    | (fst (buscar (head tokens) estado)) == True = (False, estado, (formatEst(snd (buscar (head tokens) estado)))) --snd (_, estado), head [()] -> ()
    | otherwise = (False, estado, "Error, la incognita no se encuentra definida!.") --"La incognita no existe!.")

--Mostrar-ambiente (ma):
cmd_ma :: [String] -> Estado -> (Bool, Estado, String)
cmd_ma tokens estado 
    | length(tokens) /= 0 = (False, estado, "Error, esta funcionalidad no recibe ningún parámetro!.")
    | otherwise = (False, estado, (iterar estado))

--Calcular-variable (cv):

--Calcular-variable-original (cvo):
cmd_cvo :: [String] -> Estado -> (Bool, Estado, String)
cmd_cvo tokens estado 
    | length(tokens) <= 0 = (False, estado, "Error, ingrese una expresión a evaluar!.")
    | esInt(head tokens) == True = (False, estado, "Error, se debe ingresar una incognita válida!.")
    | (fst (buscar (head tokens) estado)) == False = (False, estado, "Error, la incognita ingresada aún no se registrado!.")
    | length(buscarLstOrig (head tokens) estado) /= length(tail tokens) = (False, estado, "Error, cantidad incorrecta de valores para la expresión!.")
    | verifVarsInt(tail tokens) == False = (False, estado, "Error, las variables ingresadas deben ser números enteros!.")
    | otherwise = (False, estado, show(evalArb(buscarArbOrig (tokens!!0) estado) (enlazarValores (listaVar (buscarArbOrig (tokens!!0) estado)) (convVars(tail tokens)))) )

convVars :: [String] -> [Int]
convVars [] = init [0] --no incluir el [0]
convVars (x:xs) = (convAInt(x) : convVars(xs)) --ya se verifica que sean Int

verifVarsInt :: [String] -> Bool -- Verificar que todos sean int (cvo)
verifVarsInt [] = True
verifVarsInt (x:xs) 
    | esInt(x) == False = False
    | otherwise = verifVarsInt xs 

--Mostrar-parámetros (mp):

--Evaluar-todo (et):

--Terminar (fin)

--MostrarArbol (mostArbol)
mostArbol :: Arbol -> String
mostArbol (Hoja valor) = (obtValorTipo(valor))
mostArbol (Nodo x i d) = "(" ++ (mostArbol i) ++ " " ++ obtOperacionStr(x) ++ " " ++ (mostArbol d) ++ ")"

quitarParent :: String -> String --"( (3 + (x + 2)) * 2 )"
quitarParent ecuacion = reverse (drop 1 (reverse (drop 1 ecuacion)))

iterar :: Estado -> String
iterar [] = ""
iterar ((v2, y, q, w, p):estado) = "\n" ++ (formatEst [(v2, y, q, w, p)]) ++ iterar estado --(show (v2, y, q, w, p))

formatEst :: Estado -> String --Solo puede mostrar el primer estado en la tupla
formatEst [] = ""
formatEst estado = "{ " ++ fstEstado(head estado) ++ ", " 
                        ++ (show (scdEstado(head estado))) ++ ", " 
                        ++ quitarParent(mostArbol(trdEstado(head estado))) ++ ", " 
                        ++ (show(frhEstado(head estado))) ++ ", "
                        ++ quitarParent(mostArbol(fveEstado(head estado))) ++ " }"


{--2. Elabore una función crearArbol que tome una tira de caracteres con una 
operación binaria simple ("operando  operación  operando") y devuelva un árbol 
como del punto 1:--}

crearArbol :: [String] -> Arbol
crearArbol ec = if length (ec) == 1 
    then Hoja (defHoja (head ec)) --["a"] -> "a"
    else Nodo (obtOperacionEc(ec!!1)) (crearArbol [head ec]) (crearArbol [last ec])

defHoja :: String -> Termino
defHoja var = if esInt(var)
              then Entero (convAInt(var))
              else Variable var 

obtOperacionEc :: String -> Ecuacion
obtOperacionEc op = case op of 
             "+" ->  (+)
             "-" ->  (-)
             "*" ->  (*)

obtOperacionStr :: Ecuacion -> String
obtOperacionStr op 
    | ((op) 2 1) == 3 = "+"
    | ((op) 2 1) == 1 = "-" 
    | ((op) 2 1) == 2 = "*"


{--3. Elabore una función sustVar que tome una variable y un Arbol que representa 
su ecuación y sustituya en otro Arbol las apariciones de esa variable por copias del 
Arbol asociado.:--}

--sustVar "b" (Nodo (+) (Hoja (Variable "a")) (Hoja (Entero 2))) (Nodo (*) (Hoja (Variable "b")) (Hoja (Variable "p")))

--          var    ecuación   a mod    a mod
sustVar :: String -> Arbol -> Arbol  -> Arbol --[String]
sustVar var (Hoja valor_1) (Hoja valor_2) = if ((Variable var) == valor_2)
                                            then (Hoja valor_1)
                                            else (Hoja valor_2)
sustVar var (Nodo x1 i1 d1) (Nodo x2 i2 d2) = sustVar var (Nodo x1 i1 d1) i2

prueba3 :: Arbol -> String
prueba3 arbol = (formatEst [("var", ["a", "b"], arbol, ["a", "b"], arbol)])

{--
nHojas :: Arbol -> Int
nHojas (Hoja _) = 1
nHojas (Nodo x i d) = nHojas i + nHojas d
--}

{--4. Elabore una función listaVar que tome un Arbol y devuelva una lista con las 
variables que aparecen en dicho árbol; cada variable debe aparecer una sola vez en 
la lista:--}

--listaVar (Nodo (*) ((Nodo (+) (Hoja (Variable "a")) (Nodo (+) (Hoja (Entero 2)) (Hoja (Variable "c"))))) (Hoja (Variable "z")))
--Árbol para (a+(2+c))*z> produce ["a","c","z"]

listaVar :: Arbol -> [String]
listaVar (Hoja valor) = if esInt(obtValorTipo(valor)) == False
                        then [obtValorTipo(valor)]
                        else [""]
listaVar (Nodo raiz izq der) = quitarRep(quitarEsp(valores))
       where valores = listaVar izq ++ listaVar der

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

obtValorTipo :: Termino -> String
obtValorTipo (Variable var) = (var)
obtValorTipo (Entero var) = show(var) --int

--quitarRep(quitarEsp(listaVar()))


{--5. Elabore una función evalArb que tome un Arbol y una lista de valores, 
y devuelva el resultado de evaluar dicho Arbol usando esos valores; los valores 
se asocian con las variables siguiendo el orden especificado por el resultado 
de listaVar para ese Arbol:--}

--evalArb (Nodo (*) ((Nodo (+) (Hoja (Variable "a")) (Nodo (+) (Hoja (Entero 2)) (Hoja (Variable "c"))))) (Hoja (Variable "z"))) (enlazarValores ["a", "c", "z"] [1, 2, 3])
--evalArb (Nodo (*) ((Nodo (+) (Hoja (Variable "a")) (Nodo (+) (Hoja (Entero 2)) (Hoja (Variable "c"))))) (Hoja (Variable "z"))) (enlazarValores (listaVar (Nodo (*) ((Nodo (+) (Hoja (Variable "a")) (Nodo (+) (Hoja (Entero 2)) (Hoja (Variable "c"))))) (Hoja (Variable "z")))) [1, 2, 3])
--evalArb (Nodo (*) ((Nodo (+) (Hoja (Variable "a")) (Nodo (+) (Hoja (Variable "e")) (Hoja (Variable "c"))))) (Hoja (Variable "z"))) (enlazarValores (listaVar (Nodo (*) ((Nodo (+) (Hoja (Variable "a")) (Nodo (+) (Hoja (Variable "e")) (Hoja (Variable "c"))))) (Hoja (Variable "z")))) [3, 2, 2, 2])
--evalArb (Nodo (*) ((Nodo (+) (Hoja (Entero 3)) (Nodo (+) (Hoja (Entero 2)) (Hoja (Entero 2))))) (Hoja (Entero 2))) (enlazarValores ["a", "c", "z"] [1, 2, 3])

--evalArb Arbol (enlazarValores (listaVar Arbol) [1, 2, 3])

evalArb :: Arbol -> [(String, Int)] -> Int
evalArb (Hoja valor) tupla = if esInt(obtValorTipo(valor)) == False --si es variable/incognita
                                    then (obtenerValor (obtValorTipo(valor)) tupla) --devuelva el valor asosiado de la tupla: "b" [("a", 1), ("b", 2)]
                                    else (convAInt (obtValorTipo(valor))) --si es numero dejelo asi y conviertalo en Int
evalArb (Nodo operador izq der) tupla
   |obtOperacionStr(operador) == "+" = (operador) (evalArb izq tupla) (evalArb der tupla)
   |obtOperacionStr(operador) == "-" = (operador) (evalArb izq tupla) (evalArb der tupla)
   |obtOperacionStr(operador) == "*" = (operador) (evalArb izq tupla) (evalArb der tupla)


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


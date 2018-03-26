
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

type Estado = [(String,String)]

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
          "def" -> cmd_def (tail tokens) estado
          "borrar" -> cmd_borrar (tail tokens) estado
          "imp" -> cmd_imp estado 
          -- comando fin: retornar tripleta que finaliza ciclo          
          "fin" -> (True, estado, "Saliendo...")
          _     -> cmd_desconocido (tokens!!0) comando estado
       where tokens = words comando

-- función que implementa el comando def
--   crea nuevo estado agregando un nuevo par
cmd_def::[String] -> Estado -> (Bool, Estado, String)
cmd_def tokens estado = (False, nuevoestado, mensaje)
       where nuevoestado = estado ++ [(tokens!!0,tokens!!1)]
             mensaje = "Definido " ++ tokens!!0

-- función que implementa el comando borrar
--   
cmd_borrar::[String] -> Estado -> (Bool, Estado, String)
cmd_borrar [] estado = (False, estado, "No se especificó qué borrar")
cmd_borrar (v:_) estado = let (res, nuevoestado) = borrar v estado
                             in if res
                                  then (False, nuevoestado, v ++ " borrado")
                                  else (False, estado, v ++ " no estaba definido")


-- función que busca un nombre en el estado
-- 
buscar :: String -> Estado -> (Bool, String)
buscar _ [] = (False, "")
buscar v1 ((v2,y):estado) = if v1 == v2
                               then (True,y)
                               else  buscar v1 estado

-- función que elimina un par del estado
-- 
borrar :: String -> Estado -> (Bool, Estado)
borrar _ [] = (False, [])
borrar v1 ((v2,y):estado) = let (res,nuevoestado) = borrar v1 estado
                                 in if v1 == v2
                                      then (True,estado)
                                      else  (res, (v2,y):nuevoestado)


-- función que maneja un comando desconocido
--
cmd_desconocido :: String -> String -> Estado -> (Bool, Estado, String)
cmd_desconocido cmd comando estado = (False, estado, mensaje)
       where mensaje = "Comando desconocido ("++ cmd ++"): '" ++ comando ++ "'"

-- función que implementa el comando imp
--
cmd_imp :: Estado -> (Bool, Estado, String)
cmd_imp estado = (False, estado, show estado)

--cmd_fin :: Estado -> (Bool, Estado, String)
--cmd_fin estado = (False, estado, show estado)
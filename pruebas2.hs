--Verificar si un String es numerico

isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False
 
isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False
 
isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s


--------------------------------------------------------------

remove element list = filter (\e -> e/= element) list

--------------------------------------------------------------


elegir :: Int -> String
elegir x = case of
	1 -> "A"
	2 -> "B"
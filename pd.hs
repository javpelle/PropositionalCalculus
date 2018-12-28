-------------------------------
--- JAVIER PELLEJERO ORTEGA ---
-------------------------------

------------ DATOS, INSTANCIAS Y FORMULAS PREDEFINIDAS DEL PROGRAMA ------------

type Var = String -- nombres de variables
data FProp = V Var | No FProp | Y FProp FProp | O FProp FProp | Si FProp FProp | Sii FProp FProp deriving Read

instance Eq FProp where
  (V p1) == (V p2) = p1 == p2
  (No f1) == (No f2) = f1 == f2
  (Y f1 f2) == (Y g1 g2) = ((f1 == g1) && (f2 == g2)) || ((f1 == g2) && (f2 == g1))
  (O f1 f2) == (O g1 g2) = ((f1 == g1) && (f2 == g2)) || ((f1 == g2) && (f2 == g1))
  (Sii f1 f2) == (Sii g1 g2) = ((f1 == g1) && (f2 == g2)) || ((f1 == g2) && (f2 == g1))
  (Si f1 f2) == (Si g1 g2) = (f1 == g1) && (f2 == g2)
  f1 == f2 = False
-- A parte de la disyuncion y la conjuncion, tambien he considerado como estructuralmente
-- igual el SII
  
instance Ord FProp where
  f1 <= f2 = consecuencia f2 f1
  f1 >= f2 = f2 <= f1
  f1 < f2 = (f1 <= f2) && not (equivalente f1 f2)
  f1 > f2 = (f1 >= f2) && not (equivalente f1 f2)
-- La definicion para ordenes estrictos es consecuencia pero NO EQUIVALENTES
-- Otra opcion: f1 < f2 = consecuencia f2 f1 && not (equivalente f1 f2)

instance Show FProp where
  show (V p) = p
  show (No f) = "~" ++ show f
  show (Y f1 f2) = "(" ++ show f1 ++ " /\\ " ++ show f2 ++ ")"
  show (O f1 f2) = "(" ++ show f1 ++ " \\/ " ++ show f2 ++ ")"
  show (Si f1 f2) = "(" ++ show f1 ++ " -> " ++ show f2 ++ ")"
  show (Sii f1 f2) = "(" ++ show f1 ++ " <-> " ++ show f2 ++ ")"
  

f1 = Si (No (V "p")) (Si (V "p") (Y (V "q") (No (V "q")))) -- Tautologica
f2 = Y (V "p") (Si (No (V "q")) (No (V "p"))) -- Satisfactible pero no tautologica
f3 = Y (Y (V "p")(V "q")) (O (No (V "p")) (V "r")) -- Satisfactible pero no tautologica
f4 = Y (V "p") (No (V "p")) -- No satisfactible
f5 = Y (V "p") (V "q") -- Satisfactible pero no tautologica y equivalente a f2

------------ ENTRADA-SALIDA ------------

-- Funcion principal del programa
main:: IO()
main = do
 putStrLn "Bienvenido a este maravilloso programa de logica prosicional. Elige una opcion:"
 putStrLn "1. Evaluar una formula."
 putStrLn "2. Devuelve todas las asignaciones a variables que hacen cierta una formula."
 putStrLn "3. Evalua si una formula es tautologica."
 putStrLn "4. Evalua si una formula es satisfactible."
 putStrLn "5. Evalua si dos formulas son equivalentes."
 putStrLn "6. Dada una lista de formulas, las divide en listas de conjuntos equivalentes."
 putStrLn "0. Salir."
 opcion <- getInt
 if opcion == 0 then do putStrLn "Adios"
 else if opcion == 1 then do {mainEvalua; main}
 else if opcion == 2 then do {mainSatisfacer; main}
 else if opcion == 3 then do {mainTautologia; main}
 else if opcion == 4 then do {mainSatisfactible; main}
 else if opcion == 5 then do {mainEquivalente; main}
 else if opcion == 6 then do {mainEquivalentes; main}
 else main

-- Realiza la opcion del main de evaluar una formula
mainEvalua:: IO()
mainEvalua = do
 putStrLn "Introduce una formula:"
 p <- getFProp
 putStrLn ("Introduce una lista de bboleanos de tamano " ++ show(length (vars p)) ++ ". (Ej. [True, False... ]).")
 b <- getBools
 if (evalua p (zip (vars p) b)) then putStrLn "La formula evaluada es CIERTA."
 else putStrLn "La formula evaluada es FALSA."
 
 -- Realiza la opcion del main de devolver las asignaciones verdaderas posibles de una formula
mainSatisfacer:: IO()
mainSatisfacer = do
 putStrLn "Introduce una formula:"
 p <- getFProp
 print (filter (evalua p) (combinarP (vars p)))

-- Realiza la opcion del main de comprobar si una formula es tautologica
mainTautologia:: IO()
mainTautologia = do
 putStrLn "Introduce una formula:"
 p <- getFProp
 if (tautologia p) then putStrLn "La formula introducida es tautologica."
 else putStrLn "La formula introducida NO es tautologica."

-- Realiza la opcion del main de comprobar si una formula es satisfactible
mainSatisfactible:: IO()
mainSatisfactible = do
 putStrLn "Introduce una formula:"
 p <- getFProp
 if (satisfactible p) then putStrLn "La formula introducida es satisfactible."
 else putStrLn "La formula introducida NO es satisfactible."

-- Realiza la opcion del main de evaluar si dos formulas son equivalentes
mainEquivalente:: IO()
mainEquivalente = do
 putStrLn "Introduce una formula:"
 p1 <- getFProp
 putStrLn "Introduce otra formula:"
 p2 <- getFProp
 if (equivalente p1 p2) then putStrLn "Las formulas introducidas son equivalentes."
 else putStrLn "Las formulas introducidas NO son equivalentes."
 
-- Realiza la opcion del main de dividir una lista de formulas en listas de equivalentes
mainEquivalentes:: IO()
mainEquivalentes = do
 putStrLn "Cuantas formulas vas a introducir:"
 num <- getInt
 list <-(leeFormulas num)
 print (equivalentes list)

-- Lee una lista de x formulas proposicionales
leeFormulas:: Int -> IO [FProp]
leeFormulas 0 = return ([])
leeFormulas x = do
  putStrLn ("Introduce una formula. Quedan " ++ (show x) ++ " por introducir:")
  p <- getFProp
  list <- leeFormulas (x - 1)
  return (p:list)

-- Lee un entero
getInt:: IO Int
getInt = do
 line <- getLine
 return (read line::Int)

-- Lee una formula proposicional
getFProp:: IO FProp
getFProp = do
 line <- getLine
 return (read line::FProp)
 
 -- Lee una lista de booleanos
getBools:: IO [Bool]
getBools = do
 line <- getLine
 return (read line::[Bool])

------------ FUNCIONES DE LA PARTE BASICA ------------

-- Devuelve una lista con las variables de un formula (sin repetir)
vars::FProp -> [Var]
vars a = elimRep(vars' a)

-- Devuelve si un formula es tautologico
tautologia::FProp -> Bool
tautologia f = foldr (&&) True (map (evalua f) (combinarP (vars f)))

-- Devuelve si un formula es satisfactible para alguna combinacion.
satisfactible::FProp -> Bool
satisfactible f = foldr (||) False (map (evalua f) (combinarP (vars f)))

-- Dados dos formulas, devuelve si el primero es consecuencia logica del segundo.
consecuencia::FProp -> FProp -> Bool
consecuencia f1 f2 = tautologia (Si f2 f1)

-- Dados dos formulas, devuelve si son equivalentes.
equivalente::FProp -> FProp -> Bool
equivalente f1 f2 = tautologia (Sii f2 f1)

-- Dada una lista de formulas, devuelve una lista de duplas formada por un formula y una lista
-- de los formulas de dicha lista dada que son consecuencia del formula en cuestion.
-- Dado un formula de la lista, este no se incluira en la lista de consecuentes de su dupla.
consecuencias::[FProp] -> [(FProp,[FProp])]
consecuencias fs = [(i, [j | j <- fs, consecuencia j i]) | i <- fs]

-- Dada una lista de formulas, devuelve una lista de listas formadas por formulas equivalentes.
equivalentes::[FProp] -> [[FProp]]
equivalentes fs = equivalentes' fs []

------------ FUNCIONES AUXILIARES DE LA PARTE BASICA ------------

-- Dada una lista de formulas y una lista de clases de equivalencia de formulas (listas
-- de formulas equivalentes), introduce cada formula de la primera lista en la clase de
-- equivalencia correspondiente de la segunda.
equivalentes'::[FProp] -> [[FProp]] -> [[FProp]]
equivalentes' [] eqs = eqs
equivalentes' (f:fs) eqs = equivalentes' fs (equivalentes'' f eqs) 

-- Dada un formula y una lista de clases de equivalencia de formulas (listas de formulas
--  equivalentes), lo introduce en su clase correspondiente o crea una nueva si no existe.
equivalentes''::FProp -> [[FProp]] -> [[FProp]]
equivalentes'' f [] = [[f]]
equivalentes'' f (e:eqs) =  if equivalente f (e !! 0) then (f:e):eqs else e:(equivalentes'' f eqs)

-- Evalua un formula para ciertos valores de las variables
evalua::FProp -> [(Var, Bool)] -> Bool
evalua (V var) xs = hashMap var xs
evalua (Y x1 x2) xs = evalua x1 xs && evalua x2 xs
evalua (O x1 x2) xs = evalua x1 xs || evalua x2 xs
evalua (Si x1 x2) xs = evalua x1 xs <= evalua x2 xs
evalua (Sii x1 x2) xs = evalua x1 xs == evalua x2 xs
evalua (No x) xs = not (evalua x xs)

-- Devuelve una lista con todos las variables de un formula (incluido formulas).
vars'::FProp -> [Var]
vars' (V var) = [var]
vars' (Y x1 x2) = vars x1 ++ vars x2
vars' (O x1 x2) = vars x1 ++ vars x2
vars' (Si x1 x2) = vars x1 ++ vars x2
vars' (Sii x1 x2) = vars x1 ++ vars x2
vars' (No x) = vars x

-- Dada una lista, elimina los elementos repetidos dejando solo uno de ellos.
elimRep::Eq a => [a] -> [a]
elimRep [] = []
elimRep (x:xs) = if elem x xs then elimRep xs else x:elimRep xs

-- Devuelve una lista de listas con todas las combinaciones (True o False) para una lista de variables
combinarP::[Var] -> [[(Var, Bool)]]
combinarP [v] = [[(v, False)],[(v, True)]]
combinarP (v:vs) = map ((v, False):) (combinarP vs) ++ map ((v, True):) (combinarP vs)

-- Devuelve la pareja del primer elemento a que encuentre. Si no, error
hashMap:: Eq a => a -> [(a,b)] -> b
hashMap x ((a,b):xs) = if a == x then b else hashMap x xs
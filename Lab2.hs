module Lab2 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: German Caballero
-- Números: 165435
----------------------------------------------------

import Prelude

-- Formalización del lenguaje
type Var = String

data L = V Var | Neg L | Bin L BC L
  deriving (Eq)
data BC = And | Or | Imp | Iff
  deriving (Eq)

-- Fórmulas del Lab1
p = V "p"
q = V "q"
r = V "r"
fa :: L
fa = Bin p And (Neg (Neg q))                   -- (p ∧ ¬¬q)
fb :: L
fb = Bin p And (Bin (Neg q) And (Neg r))       -- (p ∧ ¬q ∧ ¬r)
fc :: L
fc = Bin (Neg (Neg p)) Or (Neg (Bin q And p))  -- (¬¬p ∨ ¬(q ∧ p))
fd :: L
fd = Bin (Neg (Bin r Imp r)) And fc            -- ¬(r ⊃ r) ∧ (¬¬p ∨ ¬(q ∧ p))


-- EJERCICIO 1 --
--1.1)
eval :: (Var -> Bool) -> L -> Bool
eval i (V x) = i x
eval i (Neg (V x)) = not (eval i (V x))
eval i (Bin f1 And f2) = (eval i f1) && (eval i f2)
eval i (Bin f1 Or f2) = (eval i f1) || (eval i f2)
eval i (Bin f1 Imp f2) = (eval i f1) || (eval i f2)
eval i (Bin f1 Iff f2) = (eval i f1) == (eval i f2)


--1.2)
itodasverdaderas ::  Var -> Bool
itodasverdaderas = undefined

--1.3)
itodasfalsas :: Var -> Bool
itodasfalsas = undefined

--1.4)
irfalsa :: Var -> Bool
irfalsa = undefined 

--1.5)
-- Completar con verdadera/falsa:
-- fa es false bajo itodasfalsas
-- fb es false bajo itodasfalsas
-- fc es true bajo itodasfalsas
-- fd es false bajo itodasfalsas
-- 
-- fa es true bajo itodasverdaderas
-- fb es false bajo itodasverdaderas
-- fc es true bajo itodasverdaderas
-- fd es false bajo itodasverdaderas
--
-- fa es true bajo irfalsa
-- fb es false bajo irfalsa
-- fc es true bajo irfalsa
-- fd es false bajo irfalsa

--1.6)
creari :: [(Var, Bool)] -> (Var -> Bool)
creari = undefined

--1.7)
-- Responder aquí.


-- EJERCICIO 2 --
type Fila = [(Var, Bool)]
type TV = [(Fila, Bool)]

data Clase = Tau | Contra | Cont | Sat | Fal

--2.1)
filas :: [Var] -> [Fila]
filas [] = [[]]
filas (x:xs) = [ (x, v):fila | v <- [False, True], fila <- filas xs ]

nub :: [Var] -> [Var]
nub [] = []
nub (x:xs) = if elem x xs then nub xs else x : nub xs

listarProp :: L -> [Var]
listarProp (V x) = [x]
listarProp (Neg f) = listarProp f
listarProp (Bin f1 _ f2) = nub (listarProp f1 ++ listarProp f2)

evalFila :: L -> Fila -> Bool
evalFila (V x) [] = error "variable no encontrada"
evalFila (V x) ((a,b):xs) = if x == a then b else evalFila (V x) xs
evalFila (Neg f) fila = not (evalFila f fila)
evalFila (Bin a c b) fila = 
   let ea = evalFila a fila
       eb = evalFila b fila
  in case c of
       And -> ea && eb
       Or  -> ea || eb
       Imp -> not ea || eb
       Iff -> ea == eb
 

tv :: L -> TV
tv f =  [ (fila, evalFila f fila) | fila <- filas (listarProp f) ]

--2.3)
listaBC :: [(Bool)] -> BC -> Bool
listaBC x bc = x
listaBC (x:xs) And = x && (listaBC xs bc)


es :: L -> Clase -> Bool
es f Tau = listaBC [ (evalFila f fila) | fila <- filas (listarProp f) ] And
es f Contra = False == listaBC [ (evalFila f fila) | fila <- filas (listarProp f) ] Or
es f Cont =
es f Sat =
es f Fal

--2.4)
-- Completar con tautología/contingencia/contradicción:
-- fa es ...
-- fb es ...
-- fc es ...
-- fd es ...

--2.5) 
fnc :: L -> L
fnc = undefined


----------------------------------------------------------------------------------
-- Pretty Printing (rudimentario)
----------------------------------------------------------------------------------
instance Show L where
  show (V p)         = p
  show (Neg (Neg a)) = "¬" ++ show (Neg a)
  show (Neg (V p))   = "¬ " ++ show (V p)
  show (Neg a)       = "¬ (" ++ show a ++ ")"
  show (Bin a And b) = "(" ++ show a ++ ") /\\ (" ++ show b ++ ")"
  show (Bin a Or b)  = "(" ++ show a ++ ") \\/ (" ++ show b ++ ")"
  show (Bin a Imp b) = "(" ++ show a ++ ") --> (" ++ show b ++ ")"
  show (Bin a Iff b) = "(" ++ show a ++ ") <-> (" ++ show b ++ ")"











  -- EJERCICIO 1 --
--1.1)
eval :: (Var -> Bool) -> L -> Bool
eval i (V x) = i x
eval i (Neg (V x)) = not (eval i (V x))
eval i (Bin f1 And f2) = (eval i f1) && (eval i f2)
eval i (Bin f1 Or f2) = (eval i f1) || (eval i f2)
eval i (Bin f1 Imp f2) = (eval i f1) || (eval i f2)
eval i (Bin f1 Iff f2) = (eval i f1) == (eval i f2)

--1.2)
itodasverdaderas ::  Var -> Bool
itodasverdaderas _ = True

--1.3)
itodasfalsas :: Var -> Bool
itodasfalsas _ = False

--1.4)
irfalsa :: Var -> Bool
irfalsa "r" = False
irfalsa _ = True


--1.6)
creari :: [(Var, Bool)] -> Var -> Bool
creari [] v =  error "la variable no se encuentra en la lista"
creari ((var, b):xs) v
  | v == var = b
  | otherwise = creari xs v 

--1.7)
-- Si, la interpretacion es la misma porque creari en el unico caso que da false es con la letra "r"

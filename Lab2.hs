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
interpretacionBC :: BC -> Bool -> Bool -> Bool
interpretacionBC bc a b = 
  case bc of
    And -> a && b
    Or  -> a || b
    Imp -> not a || b
    Iff -> a == b

eval :: (Var -> Bool) -> L -> Bool
eval i (V x) = i x
eval i (Neg f) = not (eval i f)
eval i (Bin f1 bc f2) = interpretacionBC bc (eval i f1) (eval i f2)


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

--1.5)
-- Completar con verdadera/falsa:
-- fa es falsa bajo itodasfalsas
-- fb es falsa bajo itodasfalsas
-- fc es verdadera bajo itodasfalsas
-- fd es falsa bajo itodasfalsas
-- 
-- fa es verdadera bajo itodasverdaderas
-- fb es falsa bajo itodasverdaderas
-- fc es verdadera bajo itodasverdaderas
-- fd es falsa bajo itodasverdaderas
--
-- fa es verdadera bajo irfalsa
-- fb es falsa bajo irfalsa
-- fc es verdadera bajo irfalsa
-- fd es falsa bajo irfalsa

--1.6)
creari :: [(Var, Bool)] -> (Var -> Bool)
creari [] _ = False
creari ((v,b):xs) x
  | x == v    = b
  | otherwise = creari xs x


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

--2.2)
nub :: [Var] -> [Var]
nub [] = []
nub (x:xs) = if elem x xs then nub xs else x : nub xs

listarProp :: L -> [Var]
listarProp (V x) = [x]
listarProp (Neg f) = listarProp f
listarProp (Bin f1 _ f2) = nub (listarProp f1 ++ listarProp f2)

tv :: L -> TV
tv f = [ (fila, eval (creari fila) f) | fila <- filas (listarProp f) ]

--2.3)
listaBC :: [Bool] -> BC -> Bool
listaBC []     And = True          -- neutro de (&&)
listaBC []     Or  = False         -- neutro de (||)
listaBC (x:xs) And = x && listaBC xs And
listaBC (x:xs) Or  = x || listaBC xs Or
listaBC _ Imp = error "Imp no es asociativa para listas"
listaBC _ Iff = error "Iff no es asociativa para listas"


data Clase = Tau | Contra | Cont | Sat | Fal
  deriving (Eq, Show)

es :: L -> Clase -> Bool
es f Tau    = listaBC vals And
es f Contra = not (listaBC vals Or)
es f Cont   = listaBC vals Or && not (listaBC vals And)
es f Sat    = listaBC vals Or
es f Fal    = not (listaBC vals And)
  where
    vals = [ eval (creari fila) f | fila <- filas (listarProp f) ]


--2.4)
-- Completar con tautología/contingencia/contradicción:
-- fa es ...
-- fb es ...
-- fc es ...
-- fd es ...

--2.5) 
fnc :: L -> L
fnc (V x) = undefined
fnc (Neg f) = undefined
nc (Bin f1 Imp f2) = fnc (Bin (Neg f1) Or f2)
fnc (Bin f1 iff f2) = fnc ( Bin
                                (Bin f1 Imp f2))
                              And
                                (Bin f2 Imp f1))



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

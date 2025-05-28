module Semantics where 

import Syntax
import State

{-  
 -
 - Este módulo también sigue muy rígidamente el apunte. Así como la semántica 
 - de operadores sintácticos mapea a operadores matemáticos, la semántica de
 - operadores sintácticos mapeará a operadores de Haskell.
 -
 - Respetamos que ⟦ _ ⟧ ∈ <cat> →  (Σ →  ℤ) donde <cat> es alguna categoría
 - sintáctica.
 -  
 -}



{-  
 - Semántica de expresiones enteras, seguimos rígidamente las filminas,
 - nada que aclarar.
 -}

semIntExp :: IntExp -> State -> Integer
semIntExp (Const n) _ = n
semIntExp (Var v) σ = σ v
semIntExp (Neg e) σ = - (semIntExp e σ)
semIntExp (BinOp op e1 e2) σ = semIntOp op (semIntExp e1 σ) (semIntExp e2 σ)

{-  
 - Semántica de operadores enteros, nada creativo acá tampoco.
 -}
semIntOp :: OpInt -> Integer -> Integer -> Integer
semIntOp Add = (+)
semIntOp Sub = (-)
semIntOp Mul = (*)
semIntOp Div = div
semIntOp Mod = mod
semIntOp Rem = rem

{-  
 - Semántica de operadores booleanos.
 -}

semBoolExp :: BoolExp -> State -> Bool
semBoolExp (BConst b) _ = b
semBoolExp (Not b) σ = not (semBoolExp b σ)
semBoolExp (RelOp op e1 e2) σ = semRel op (semIntExp e1 σ) (semIntExp e2 σ)
semBoolExp (BoolOp op b1 b2) σ = semBoolOp op (semBoolExp b1 σ) (semBoolExp b2 σ)

semRel :: OpRel -> Integer -> Integer -> Bool
semRel Lt = (<)
semRel Le = (<=)
semRel Eq = (==)
semRel Ne = (/=)
semRel Ge = (>=)
semRel Gt = (>)

semBoolOp :: OpBool -> Bool -> Bool -> Bool
semBoolOp And = (&&)
semBoolOp Or  = (||)
semBoolOp Imp = \a b -> not a || b  {- Usamos la definición de la implicancia -}
semBoolOp Iff = (==) {- Dudoso? -}


{-  
 -                Semántica de comandos
 -}


semComm :: Comm -> State -> State 
semComm Skip σ = σ
semComm (Assign v e) σ = actualizar σ v (semIntExp e σ)
semComm (IfThenElse b c1 c2) σ =
  if semBoolExp b σ then semComm c1 σ else semComm c2 σ

semComm (Concat c1 c2) σ = semComm c2 σ' 
                        where σ' = semComm c1 σ
semComm (While b c) σ =
  if semBoolExp b σ then semComm (While b c) σ' else σ
                        where σ' = semComm c σ

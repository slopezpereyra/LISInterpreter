module Syntax where 

import Numeric.Natural

{- Variables
 - -------------------------------- 
 -
 - Este es el único punto en que (sólo en apariencia) desvíamos de LIS.
 - Las variables en LIS se denotan con las metavariables v, w, u, ...,
 - pero son un conjunto numerable. Por ende, dado un ordenamiento 
 - arbitrario v₁, v₂, … , es válido hablar de la primera variable, la segunda
 - variable, etc. Por ende, hacemos que Var sea de tipo Natural, i.e. 
 - toda variable es un elemento de {1, 2, … } = ℕ. 
 -
 - Se sigue que Σ ∈ ℕ →  ℤ.
 -
 - -}

type Var = Natural

{- Expresiones enteras y operadores 
 - -------------------------------- 
 -
 - Seguimos rígidamente la sintaxis abstracta,
 - nada particularmente creativo.
 -
 - -------------------------------- 
 - -}

data IntExp
  = Const Integer
  | Var Var
  | Neg IntExp                {- Neg de negativo, no de negación, vale aclarar. -}
  | BinOp OpInt IntExp IntExp
  deriving (Show)

data OpInt = Add | Sub | Mul | Div | Mod | Rem deriving (Show)


{- Expresiones booleanas y operadores -}

data BoolExp
  = BConst Bool
  | Not BoolExp
  | RelOp OpRel IntExp IntExp
  | BoolOp OpBool BoolExp BoolExp
  deriving (Show)

{- 
 - Por costumbre, siglas en inglés: Less than (Le), Less or equal (Le), etc. 
 - (El cipayismo es total.)
 - -}
data OpRel = Lt | Le | Eq | Ne | Ge | Gt deriving (Show)
data OpBool = And | Or | Imp | Iff deriving (Show)


{- Sintaxis de comandos -}
data Comm
  = Skip
  | Assign Var IntExp
  | IfThenElse BoolExp Comm Comm
  | Concat Comm Comm    -- Concatenación de comandos, juega el papel de ";".
  | While BoolExp Comm  -- 
  deriving (Show)


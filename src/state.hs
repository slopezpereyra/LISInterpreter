module State where 

import Syntax
import Numeric.Natural

type State = Natural -> Integer

{- La actualización (parcial) de un estado es el resultado de la asignación: 
 - [v := k]σ ↦  [σ ∣ v : k] -}
actualizar :: State -> Var -> Integer -> State
actualizar σ v n = \x -> if x == v then n else σ x


{- 
 -
 - La actualización (finita) de un estado es el resultado de múltiples
 - asignaciones. Lo hacemos con foldl.
 -
 - Por ejemplo, asumamos que σ ∈ ℕ →  ℤ es la identidad. 
 - actualizarMultiple σ [(1, -1), (2, -2), (3, -3)] foldea como sigue: 
 -
 - (a) LLama `actualizar a σ (-1, 1)` lo cual resulta en σ₁. 
 - (b) LLama `actualizar a σ₁ (2, -2)` lo cual resulta en σ₂
 - (c) LLama `actualizar a σ₃ (3, -3)` lo cual resulta en σ₃. 
 -
 - Se termina devolviendo σ₃, que tiene todas las actualizaciones hechas.
 -
 - -}
actualizarMultiple :: State -> [( Var, Integer )] -> State 
actualizarMultiple = foldl ( \σ' (v, n) -> actualizar σ' v n)

-- Para facilitarnos la vida al testear, definimos de base el estado `idState`,
-- que mapea todos los naturales a sí mismos. (El estado con la primera variable
-- en 1, la segunda en 2, etc.)
idState = fromIntegral :: Natural -> Integer

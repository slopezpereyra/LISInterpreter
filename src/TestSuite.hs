module TestSuite where

import Syntax
import State
import Semantics

import Test.HUnit
import Numeric.Natural

-- Test de asignación simple: x1 := 42
test_assign = TestCase $
  let c = Assign 1 (Const 42)
      σ = semComm c idState
  in assertEqual "Assign 42 to var 1" 42 (σ 1)

-- Test de concatenación: x1 := 3; x2 := x1 + 4
test_concat = TestCase $
  let c = Concat (Assign 1 (Const 3)) (Assign 2 (BinOp Add (Var 1) (Const 4)))
      σ = semComm c idState
  in assertEqual "Var 2 should be 3 + 4 = 7" 7 (σ 2)

-- Test de if con condición verdadera: se ejecuta rama "then"
test_if_true = TestCase $
  let c = IfThenElse (BConst True)
                     (Assign 1 (Const 1))
                     (Assign 1 (Const 2))
      σ = semComm c idState
  in assertEqual "Should pick then-branch" 1 (σ 1)

-- Test de if con condición falsa: se ejecuta rama "else"
test_if_false = TestCase $
  let c = IfThenElse (BConst False)
                     (Assign 1 (Const 1))
                     (Assign 1 (Const 2))
      σ = semComm c idState
  in assertEqual "Should pick else-branch" 2 (σ 1)

-- Test de bucle while: mientras x1 > 0 hacer x1 := x1 - 1 (empieza en 3)
test_while = TestCase $
  let c = While (RelOp Gt (Var 1) (Const 0))
                (Assign 1 (BinOp Sub (Var 1) (Const 1)))
      σ0 = actualizar idState 1 3  -- estado inicial con x1 = 3
      σf = semComm c σ0
  in assertEqual "While should reduce var 1 to 0" 0 (σf 1)

-- Test de while anidado con contador:
-- x1 := 4, x2 := 0
-- mientras x1 > 0 hacer { x2 := x2 + 1; x1 := x1 - 1 }
-- al final x2 debe valer 4
test_while_nested = TestCase $
  let body = Concat (Assign 2 (BinOp Add (Var 2) (Const 1)))
                    (Assign 1 (BinOp Sub (Var 1) (Const 1)))
      loop = While (RelOp Gt (Var 1) (Const 0)) body
      σ0 = actualizar (actualizar idState 1 4) 2 0  -- x1 = 4, x2 = 0
      σf = semComm loop σ0
  in assertEqual "Var 2 should count 4 times" 4 (σf 2)

-- Agrupamos todos los tests
tests :: Test
tests = TestList
  [ TestLabel "Assignment" test_assign
  , TestLabel "Concat" test_concat
  , TestLabel "IfThenElse True" test_if_true
  , TestLabel "IfThenElse False" test_if_false
  , TestLabel "While loop" test_while
  , TestLabel "While with counter" test_while_nested
  ]

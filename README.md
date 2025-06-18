# LIS Interpreter en Haskell

Intérprete básico de LIS en Haskell. Se define la sintaxis abstracta con las
reglas de construcción tomadas del apunte. Se define una semántica basada en
estados y la evalución de dicha semántica.

This project implements a small imperative language in Haskell, including its **abstract syntax**, **state-based semantics**, and **evaluation**.

# Como iniciar

Desde el directorio del repo:

```bash
ghci src/Main.hs src/state.hs src/syntax.hs src/semantics.hs src/TestSuite.hs
```

# Ejemplo en GHCI

En la primera linea, definimos un while `w` que, mientras $\sigma v_1$ (la
primera var) sea mayor a cero, le resta 1.

La segunda línea define $\sigma_0$ como el estado identidad (el estado que
asigna a $v_1$ el valor $1$, a $v_2$ el valor $2$, ...), pero modificado de
manera tal que la primera variable valga 3.

La tercera línea define el estado resultante de ejecutar `w` a partir del estado
$\sigma_0$.

```haskell
> let w = While (RelOp Gt (Var 1) (Const 0)) (Assign 1 (BinOp Sub (Var 1) (Const 1)))
> let σ0 = actualizar idState 1 3
> let σFinal = semComm w σ0
```

Deberíamos ver, después de esto, que `σFinal 1` imprime `0` (), porque la primera
variable debería ahora valer cero.



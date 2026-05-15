module Primitives where

import Value

-- Извлекает Integer из runtime-значения.
--
-- Вызывает ошибку, если значение не является числом.
unpackNumber :: Value -> Integer

unpackNumber (NumberV n) =
    n

unpackNumber _ =
    error "Expected number"

-- Сложение.
--
-- Примеры:
-- (+ 1 2 3) -> 6
-- (+)       -> 0
primitivePlus :: [Value] -> Value
primitivePlus args =
    NumberV
        (sum (map unpackNumber args))

-- Вычитание.
--
-- Примеры:
-- (- 10 3 2) -> 5
-- (- 5)      -> -5
primitiveMinus :: [Value] -> Value

primitiveMinus [] =
    error "Expected at least one argument"

primitiveMinus [x] =
    NumberV
        (- unpackNumber x)

primitiveMinus (x:xs) =
    NumberV
        (unpackNumber x - sum (map unpackNumber xs))

-- Умножение.
--
-- Примеры:
-- (* 2 3 4) -> 24
-- (*)       -> 1
primitiveMultiply :: [Value] -> Value
primitiveMultiply args =
    NumberV
        (product (map unpackNumber args))

-- Целочисленное деление.
--
-- Примеры:
-- (/ 20 2 2) -> 5
-- (/ 5 2)    -> 2
primitiveDivide :: [Value] -> Value

primitiveDivide [] =
    error "Expected at least one argument"

primitiveDivide [x] =
    NumberV
        (unpackNumber x)

primitiveDivide (x:xs) =
    NumberV
        (foldl div (unpackNumber x)
            (map unpackNumber xs))

-- Проверка чисел на равенство.
--
-- Примеры:
-- (= 5 5) -> #t
-- (= 5 3) -> #f
--
-- Функция принимает ровно 2 аргумента.
primitiveEq :: [Value] -> Value

primitiveEq [x, y] =
    BooleanV
        (unpackNumber x == unpackNumber y)

primitiveEq _ =
    error "= expects exactly 2 arguments"

-- Проверка отношения "меньше".
--
-- Примеры:
-- (< 2 5) -> #t
-- (< 7 3) -> #f
--
-- Функция принимает ровно 2 аргумента.
primitiveLess :: [Value] -> Value

primitiveLess [x, y] =
    BooleanV
        (unpackNumber x < unpackNumber y)

primitiveLess _ =
    error "< expects exactly 2 arguments"

-- Проверка отношения "больше".
--
-- Примеры:
-- (> 10 3) -> #t
-- (> 1 8)  -> #f
--
-- Функция принимает ровно 2 аргумента.
primitiveGreater :: [Value] -> Value

primitiveGreater [x, y] =
    BooleanV
        (unpackNumber x > unpackNumber y)

primitiveGreater _ =
    error "> expects exactly 2 arguments"

-- Начальное окружение интерпретатора
-- со встроенными функциями.
primitiveEnv :: Env
primitiveEnv =

    defineVar ">" (PrimitiveFunc primitiveGreater) $

    defineVar "<" (PrimitiveFunc primitiveLess) $

    defineVar "=" (PrimitiveFunc primitiveEq) $

    defineVar "/" (PrimitiveFunc primitiveDivide) $

    defineVar "*" (PrimitiveFunc primitiveMultiply) $

    defineVar "-" (PrimitiveFunc primitiveMinus) $

    defineVar "+" (PrimitiveFunc primitivePlus) $

    emptyEnv
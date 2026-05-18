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
primitivePlus :: [Value] -> Value
primitivePlus args =
    NumberV
        (sum (map unpackNumber args))

-- Вычитание.
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
primitiveMultiply :: [Value] -> Value
primitiveMultiply args =
    NumberV
        (product (map unpackNumber args))

-- Целочисленное деление.
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

primitiveAbs :: [Value] -> Value

primitiveAbs [NumberV n] =
    NumberV (abs n)

primitiveAbs _ =
    error "abs expects exactly 1 number"

-- Проверка равенства значений.
primitiveEq :: [Value] -> Value

primitiveEq [] =
    BooleanV True

primitiveEq [_] =
    BooleanV True

primitiveEq (x:xs) =
    BooleanV (all (== x) xs)

-- Проверка отношения "меньше".
primitiveLess :: [Value] -> Value

primitiveLess [x, y] =
    BooleanV
        (unpackNumber x < unpackNumber y)

primitiveLess _ =
    error "< expects exactly 2 arguments"

-- Проверка отношения "больше".
primitiveGreater :: [Value] -> Value

primitiveGreater [x, y] =
    BooleanV
        (unpackNumber x > unpackNumber y)

primitiveGreater _ =
    error "> expects exactly 2 arguments"

-- Логическое отрицание.
primitiveNot :: [Value] -> Value

primitiveNot [BooleanV False] =
    BooleanV True

primitiveNot [_] =
    BooleanV False

primitiveNot _ =
    error "not expects exactly 1 argument"

-- Создает список из переданных значений.
primitiveList :: [Value] -> Value

primitiveList = ListV

-- Возвращает первый элемент списка.
primitiveCar :: [Value] -> Value

primitiveCar [ListV (x:_)] =
    x

primitiveCar [ListV []] =
    error "car: empty list"

primitiveCar _ =
    error "car expects exactly 1 non-empty list"

-- Возвращает хвост списка.
primitiveCdr :: [Value] -> Value

primitiveCdr [ListV (_:xs)] =
    ListV xs

primitiveCdr [ListV []] =
    error "cdr: empty list"

primitiveCdr _ =
    error "cdr expects exactly 1 non-empty list"

-- Добавляет элемент в начало списка.
primitiveCons :: [Value] -> Value

primitiveCons [value, ListV values] =
    ListV (value : values)

primitiveCons _ =
    error "cons expects value and list"

-- Проверяет список на пустоту.
primitiveNull :: [Value] -> Value

primitiveNull [ListV []] =
    BooleanV True

primitiveNull [ListV _] =
    BooleanV False

primitiveNull _ =
    error "null? expects exactly 1 list"

-- Проверяет,
-- что все элементы списка различны.
allDistinct :: [Value] -> Bool

allDistinct [] =
    True

allDistinct (x:xs) =
    notElem x xs
        && allDistinct xs

-- Проверка уникальности элементов списка.
--
-- Примеры:
-- (distinct? (list 1 2 3)) -> #t
-- (distinct? (list 1 2 1)) -> #f
primitiveDistinct :: [Value] -> Value

primitiveDistinct [ListV values] =
    BooleanV (allDistinct values)

primitiveDistinct _ =
    error "distinct? expects exactly 1 list"

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

    defineVar "list" (PrimitiveFunc primitiveList) $

    defineVar  "car" (PrimitiveFunc primitiveCar) $

    defineVar  "cdr" (PrimitiveFunc primitiveCdr) $

    defineVar  "cons" (PrimitiveFunc primitiveCons) $

    defineVar  "null?" (PrimitiveFunc primitiveNull) $

    defineVar "abs" (PrimitiveFunc primitiveAbs) $

    defineVar "distinct?" (PrimitiveFunc primitiveDistinct) $

    defineVar "not" (PrimitiveFunc primitiveNot) emptyEnv
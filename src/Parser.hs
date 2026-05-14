module Parser where

import Data.Char
import AST

-- ============================================================
-- Token definition
-- ============================================================

-- Тип токенов Lisp-подобного языка.
data Token
    = LParen
    | RParen
    | TokNumber Integer
    | TokBoolean Bool
    | TokSymbol String
    deriving (Eq, Show)

-- ============================================================
-- Tokenizer helpers
-- ============================================================

-- Проверяет, может ли символ входить
-- в состав токена.
--
-- Пробелы и скобки завершают токен.
isTokenChar :: Char -> Bool
isTokenChar c =
    not (isSpace c) && c /= '(' && c /= ')'

-- Считывает последовательность символов,
-- образующих один токен.
--
-- Например:
-- "define x" -> ("define", " x")
readToken :: String -> (String, String)
readToken =
    span isTokenChar

-- Преобразует строковое представление
-- токена в Token.
parseAtom :: String -> Token
parseAtom str
    | all isDigit str =
        TokNumber (read str)

    | otherwise =
        TokSymbol str

-- ============================================================
-- Tokenizer
-- ============================================================

-- Выполняет лексический анализ исходного кода.
--
-- Преобразует строку программы
-- в список токенов.
tokenize :: String -> [Token]
tokenize [] = []

tokenize (c:cs)

    -- Игнорирование пробельных символов
    | isSpace c =
        tokenize cs

    -- Открывающая скобка
    | c == '(' =
        LParen : tokenize cs

    -- Закрывающая скобка
    | c == ')' =
        RParen : tokenize cs

    -- Boolean literals
    | c == '#' =
        case cs of

            ('t':rest) ->
                TokBoolean True : tokenize rest

            ('f':rest) ->
                TokBoolean False : tokenize rest

            _ ->
                error "Invalid boolean literal"

    -- Symbols / numbers
    | otherwise =
        let
            (tok, rest) = readToken (c:cs)
        in
            parseAtom tok : tokenize rest

-- ============================================================
-- Parser
-- ============================================================

-- Парсит содержимое списка
-- до соответствующей закрывающей скобки.
parseList :: [Token] -> ([Expr], [Token])

-- Незакрытый список
parseList [] =
    error "Unexpected end of list"

-- Конец списка
parseList (RParen : rest) =
    ([], rest)

-- Рекурсивный разбор элементов списка
parseList tokens =
    let
        (firstExpr, rest1) =
            parseExpr tokens

        (otherExprs, rest2) =
            parseList rest1

    in
        (firstExpr : otherExprs, rest2)

-- Парсит одно выражение Lisp-подобного языка.
--
-- Возвращает:
-- 1. Построенное AST
-- 2. Оставшиеся токены
parseExpr :: [Token] -> (Expr, [Token])

-- Число
parseExpr (TokNumber n : rest) =
    (Number n, rest)

-- Boolean
parseExpr (TokBoolean b : rest) =
    (Boolean b, rest)

-- Символ
parseExpr (TokSymbol s : rest) =
    (Symbol s, rest)

-- Список
parseExpr (LParen : rest) =
    let
        (exprs, remaining) =
            parseList rest

    in
        (List exprs, remaining)

-- Неожиданный конец ввода
parseExpr [] =
    error "Unexpected end of input"

-- Лишняя закрывающая скобка
parseExpr (RParen : _) =
    error "Unexpected )"

-- ============================================================
-- Top-level parser
-- ============================================================

-- Парсит всю программу целиком.
--
-- Проверяет, что после выражения
-- не осталось лишних токенов.
parse :: [Token] -> Expr
parse tokens =
    case parseExpr tokens of

        -- Все токены успешно обработаны
        (expr, []) ->
            expr

        -- Остались лишние токены
        (_, remaining) ->
            error ("Unexpected tokens: " ++ show remaining)
module Parser where

import Data.Char

-- Тип токенов Lisp-подобного языка
data Token
    = LParen
    | RParen
    | TokNumber Integer
    | TokBoolean Bool
    | TokSymbol String
    deriving (Eq, Show)

-- Проверяет, может ли символ входить в состав токена.
-- Символы пробела и скобки завершают токен.
isTokenChar :: Char -> Bool
isTokenChar c =
    not (isSpace c) && c /= '(' && c /= ')'

-- Считывает последовательность символов,
-- образующих один токен.
readToken :: String -> (String, String)
readToken = span isTokenChar

-- Преобразует строковое представление токена
-- в соответствующий тип Token.
parseAtom :: String -> Token
parseAtom str
    | all isDigit str = TokNumber (read str)
    | otherwise = TokSymbol str

-- Основной tokenizer.
-- Преобразует исходный текст программы
-- в список токенов.
tokenize :: String -> [Token]
tokenize [] = []

tokenize (c:cs)
    | isSpace c = tokenize cs

    | c == '(' =
        LParen : tokenize cs

    | c == ')' =
        RParen : tokenize cs

    | c == '#' =
        case cs of
            ('t':rest) ->
                TokBoolean True : tokenize rest

            ('f':rest) ->
                TokBoolean False : tokenize rest

            _ ->
                error "Invalid bool"

    | otherwise =
        let (tok, rest) = readToken (c:cs)
        in parseAtom tok : tokenize rest
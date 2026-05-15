module Environment where

import qualified Data.Map as Map
import Value

-- Таблица переменных интерпретатора.
--
-- String  -> имя переменной
-- Value   -> значение переменной
type Env = Map.Map String Value

-- Создает пустое окружение.
emptyEnv :: Env
emptyEnv = Map.empty

-- Ищет значение переменной в окружении.
lookupVar :: String -> Env -> Maybe Value
lookupVar = Map.lookup

-- Добавляет переменную в окружение.
defineVar :: String -> Value -> Env -> Env
defineVar = Map.insert
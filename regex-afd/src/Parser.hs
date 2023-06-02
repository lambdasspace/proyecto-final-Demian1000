module Parser
    ( --Regex(..)
    --, parser
    ) where

-- import Lexer (Token)

-- data Regex
--     = Null
--     | Eps -- ε
--     | Pun -- .
--     | Sim Char -- Símbolo
--     | Lis [Char] -- [...]
--     | Dis Regex Regex -- |
--     | Sec Regex Regex -- αβ
--     | Mas Regex -- +
--     | Rep Regex -- *
--     | Int Regex -- ?
--     deriving (Eq)

-- parser :: Maybe Regex -> [Token] -> Maybe Regex
-- parser Nothing _ = Nothing
-- parser acc [] = acc
-- parser (Just acc) (c:cs) = case (acc, c) of
--     (Null, SIMBOLO s) -> parser (Sim s) cs
--     (Null, PUNTO) -> parser Pun cs
--     (Null, EPSILON) -> parser Eps cs
--     (Null, LCOR) ->
--     (Null, RCOR) ->
--     (Null, ESTRELLA) -> Nothing
--     (Null, MAS) -> Nothing
--     (Null, INTERROGACION) -> Nothing
--     (Null, DISYUNCION) -> Nothing
--     (Null, NEGACION) ->
--     (Null, LPAR) -> parser (parser (Just Null) cs) --
--     (Null, RPAR) -> Nothing --

--     (x, SIMBOLO s) ->
--     (x, PUNTO) ->
--     (x, EPSILON) ->
--     (x, LCOR) ->
--     (x, RCOR) ->
--     (x, ESTRELLA) -> parser (Rep acc) cs
--     (x, MAS) -> parser (Mas acc) cs
--     (x, INTERROGACION) -> parser (Int acc) cs
--     (x, DISYUNCION) ->
--     (x, NEGACION) ->
--     (x, LPAR) -> parser []
--     (x, RPAR) -> acc


-- ap :: int -> [Token] -> [Token] -> Regex
-- ap estado pila entrada = 
 
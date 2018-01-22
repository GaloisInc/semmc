module SemMC.Architecture.ARM.Components
    (
     -- * Parsing
     Parser
    )
    where


import Text.Megaparsec
-- import Text.Megaparsec.Char
-- import Text.Megaparsec.Char.Lexer

type Parser = Parsec String String

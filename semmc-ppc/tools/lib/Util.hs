module Util (
  matchConstructor,
  toIORelFP
  ) where

import qualified Data.List as L
import Text.Printf ( printf )
import qualified Dismantle.Testing.Regex as RE

import qualified Data.Parameterized.Classes as P
import qualified Dismantle.PPC as PPC

toIORelFP :: PPC.Opcode PPC.Operand sh -> FilePath
toIORelFP op = printf "%s.iorel" (P.showF op)

matchConstructor :: String -> Bool
matchConstructor s = and [ notElem '8' s
                         , RE.hasMatches s rx
                         , not (RE.hasMatches s erx)
                         ]
  where
    Right erx = RE.mkRegex (L.intercalate "|" exclude)
    exclude = [ "^DIV.*E.*$"
              , "^ATTN$"
              , "^CMPEQB$"
              , "^CMPRB$"
              , "^CNTT.*"
              ]
    Right rx = RE.mkRegex (L.intercalate "|" matchers)
    matchers = [ "^A.*"
               , "^CNT.*"
               , "^DIV.*[^E]$"
               , "^DS.*"
               , "^EQ.*"
               , "^F.*"
               , "^MUL.*"
               , "^N.*"
               , "^OR.*"
               , "^POP.*"
               , "^SR.*"
               , "^SL.*"
               , "^SUB.*"
               ]

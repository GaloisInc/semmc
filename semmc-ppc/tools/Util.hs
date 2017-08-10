module Util (
  matchConstructor,
  toFP
  ) where

import qualified Data.List as L
import Text.Printf ( printf )
import qualified Text.RE.TDFA as RE

import qualified Data.Parameterized.Classes as P
import qualified Dismantle.PPC as PPC

toFP :: PPC.Opcode PPC.Operand sh -> FilePath
toFP op = printf "%s.iorel" (P.showF op)

matchConstructor :: String -> Bool
matchConstructor s = and [ notElem '8' s
                         , RE.anyMatches (s RE.*=~ rx)
                         , not (RE.anyMatches (s RE.*=~ erx))
                         ]
  where
    Just erx = RE.compileRegex (L.intercalate "|" exclude)
    exclude = [ "^DIV.*E.*$"
              , "^ATTN$"
              , "^CMPEQB$"
              , "^CMPRB$"
              , "^CNTT.*"
              ]
    Just rx = RE.compileRegex (L.intercalate "|" matchers)
    matchers = [ "^A.*"
               , "^CMP.*"
               , "^CNT.*"
               , "^CR.*"
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

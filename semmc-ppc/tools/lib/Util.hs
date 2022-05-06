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
    erx = case RE.mkRegex (L.intercalate "|" exclude) of
            Left err   -> error $ "matchConstructor: Failed to build exclude regex " ++ err
            Right erx' -> erx'
    exclude = [ "^DIV.*E.*$"
              , "^ATTN$"
              , "^CMPEQB$"
              , "^CMPRB$"
              , "^CNTT.*"
              ]
    rx = case RE.mkRegex (L.intercalate "|" matchers) of
           Left err  -> error $ "matchConstructor: Failed to build matchers regex: " ++ err
           Right rx' -> rx'
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

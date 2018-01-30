module SemMC.Architecture.ARM.BaseSemantics
    ( semdefs
    , numGPR
    )
    where

import Data.Word ( Word8 )
import SemMC.Architecture.ARM.BaseSemantics.Arithmetic
import SemMC.Architecture.ARM.BaseSemantics.Branch
import SemMC.Architecture.ARM.BaseSemantics.Memory
import SemMC.DSL


numGPR :: Word8
numGPR = 16

semdefs :: [ (String, [(String, Definition)]) ]
semdefs = [ ("memory", memory)
          , ("arithmetic", arithmetic)
          , ("branches", branches)
          ]

memory :: [(String, Definition)]
memory = runSem $ do
            manualMemory
            return ()

arithmetic :: [(String, Definition)]
arithmetic = runSem $ do
             manualArithmetic
             return ()

branches :: [(String, Definition)]
branches = runSem $ do
             manualBranches
             return ()

module SemMC.Architecture.ARM.BaseSemantics
    ( semdefs
    , numGPR
    )
    where

import Data.Word ( Word8 )
import SemMC.Architecture.ARM.BaseSemantics.Branch
import SemMC.Architecture.ARM.BaseSemantics.Memory
import SemMC.Architecture.ARM.BaseSemantics.Registers
import SemMC.DSL


numGPR :: Word8
numGPR = 16

semdefs :: [ (String, [(String, Definition)]) ]
semdefs = [ ("memory", memory)
          , ("branches", branches)
          ]

memory :: [(String, Definition)]
memory = runSem $ do
            manualMemory
            return ()

branches :: [(String, Definition)]
branches = runSem $ do
             manualBranches
             return ()

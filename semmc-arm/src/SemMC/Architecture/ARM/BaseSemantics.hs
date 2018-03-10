module SemMC.Architecture.ARM.BaseSemantics
    ( semdefs
    )
    where

import SemMC.Architecture.ARM.BaseSemantics.Arithmetic
import SemMC.Architecture.ARM.BaseSemantics.Branch
import SemMC.Architecture.ARM.BaseSemantics.Memory
import SemMC.DSL


semdefs :: [ (String, [(String, Definition)]) ]
semdefs = [ ("memory", memory)
          , ("arithmetic", arithmetic)
          , ("bitwise", bitwise)
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

bitwise :: [(String, Definition)]
bitwise = runSem $ do
            manualBitwise
            return ()

branches :: [(String, Definition)]
branches = runSem $ do
             manualBranches
             return ()

module SemMC.Architecture.ARM.BaseSemantics
    ( semdefs
    )
    where

import SemMC.Architecture.ARM.BaseSemantics.Memory
import SemMC.DSL


semdefs :: [ (String, [(String, Definition)]) ]
semdefs = [ ("memory", memory)
          ]

memory :: [(String, Definition)]
memory = runSem $ do
            manualMemory
            return ()

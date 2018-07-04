module SemMC.Architecture.ARM.BaseSemantics
    ( semdefs, fundefs
    )
    where

import SemMC.Architecture.ARM.BaseSemantics.Arithmetic
import SemMC.Architecture.ARM.BaseSemantics.Branch
import SemMC.Architecture.ARM.BaseSemantics.Memory
import SemMC.Architecture.ARM.BaseSemantics.Misc
import SemMC.DSL


semdefs :: [ (String, [(String, Definition)]) ]
fundefs :: [ (String, FunctionDefinition) ]
(semdefs, [], fundefs) = evalSem $ do
  -- We expect to match the empty list above because all the formulas
  -- here are generated underneath calls to gather.
  memory <- gather manualMemory
  arithmetic <- gather manualArithmetic
  bitwise <- gather manualBitwise
  branches <- gather manualBranches
  misc <- gather miscSemantics
  return [ ("memory", memory)
         , ("arithmetic", arithmetic)
         , ("bitwise", bitwise)
         , ("branches", branches)
         , ("miscellaneous", misc)
         ]

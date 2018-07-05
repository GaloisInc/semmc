module SemMC.Architecture.ARM.BaseSemantics
    ( semdefs, fundefs
    )
    where

import qualified Data.Map as Map
import SemMC.Architecture.ARM.BaseSemantics.Arithmetic
import SemMC.Architecture.ARM.BaseSemantics.Branch
import SemMC.Architecture.ARM.BaseSemantics.Memory
import SemMC.Architecture.ARM.BaseSemantics.Misc
import SemMC.DSL


semdefs :: [ (String, [(String, Definition)]) ]
semdefs = [ ("memory", pkgFormulas memory)
          , ("arithmetic", pkgFormulas arithmetic)
          , ("bitwise", pkgFormulas bitwise)
          , ("branches", pkgFormulas branches)
          , ("miscellaneous", pkgFormulas misc)
          ]

fundefs :: [ (String, FunctionDefinition) ]
fundefs =
  -- Map.unions will keep the first instance of each function it finds
  Map.toList $ Map.unions $ map (Map.fromList . pkgFunctions) $
    [ memory, arithmetic, bitwise, branches, misc ]

memory :: Package
memory = runSem manualMemory

arithmetic :: Package
arithmetic = runSem manualArithmetic

bitwise :: Package
bitwise = runSem manualBitwise

branches :: Package
branches = runSem manualBranches

misc :: Package
misc = runSem miscSemantics

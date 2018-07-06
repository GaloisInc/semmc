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


semdefs :: [ (String, [Definition]) ]
semdefs = [ (name, pkgFormulas pkg) | (name, pkg) <- packages ]

fundefs :: [ FunctionDefinition ]
fundefs =
  -- Map.unions will keep the first instance of each function it finds
  Map.elems $ Map.unions [ functionMap pkg | (_, pkg) <- packages ]
  where
    functionMap pkg =
      Map.fromList [ (fdName fd, fd) | fd <- pkgFunctions pkg ]

packages :: [ (String, Package) ]
packages = [ ("memory", memory)
           , ("arithmetic", arithmetic)
           , ("bitwise", bitwise)
           , ("branches", branches)
           , ("miscellaneous", misc)
           ]

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

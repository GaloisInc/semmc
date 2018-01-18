module SemMC.Architecture.ARM.BaseSemantics
    ( base
    , pseudo
    , manual
    )
    where

import SemMC.Architecture.ARM.BaseSemantics.Memory
import SemMC.DSL


base :: [(String, Definition)]
base = runSem $ do
         return ()

pseudo :: [(String, Definition)]
pseudo = runSem $ do
           return ()

manual :: [(String, Definition)]
manual = runSem $ do
           manualMemory
           return ()

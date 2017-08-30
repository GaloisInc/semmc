module Tests.LoadTest (loadTests) where

import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector.Sized as V
import           Data.Word (Word32)

import qualified SemMC.Stochastic.Remote as R
import SemMC.ARM ( MachineState(..), Instruction )

loadTests :: [R.TestCase MachineState Instruction]
loadTests = [testLDM1,testLDM2]


defaultTest :: R.TestCase MachineState Instruction
defaultTest = R.TestCase { R.testNonce = 0
                         , R.testContext = ctx
                         -- add r1, r2?
                         , R.testProgram = [LB.pack [0x02, 0x10, 0x81, 0xE0]]
                         }
  where
    ctx = MachineState { gprs = grs
                       , gprs_mask = mask
                       , fprs = frs
                       , cpsr = cpsr_reg
                       , mem1 = m1
                       , mem2 = m1
                       }
    Just grs = V.fromList [ 0, 0, 0, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0
                          ]
    Just mask = V.fromList (replicate 16 0)
    Just frs  = V.fromList (replicate 32 0)
    Just m1   = V.fromList (replicate 32 0)
    cpsr_reg  = (16 :: Word32)


--------------------------------------------------------------------------------------------
-- LDM (Exception return)
--------------------------------------------------------------------------------------------

-- | Executes ldm r0, {r1, r2, r3, r4}. Loads contents of first 4 memory locations into
-- | r1, r2, r3, r4. Updates PC
testLDM1 :: R.TestCase MachineState Instruction
testLDM1 = defaultTest { R.testNonce = 21
                       , R.testContext = (R.testContext defaultTest) { gprs_mask = mask, mem1 = m1 }
                       , R.testProgram = [LB.pack [0x1E,0x00,0x90,0xE8]]
                       }
  where
    Just mask = V.fromList [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    Just m1  = V.fromList (replicate 32 13)


-- | Executes ldm r0!, {r3, r5, r9, r10}
testLDM2 :: R.TestCase MachineState Instruction
testLDM2 = defaultTest { R.testNonce = 22
                       , R.testContext = (R.testContext defaultTest) { gprs_mask = mask, mem1 = m1 }
                       , R.testProgram = [LB.pack [0x28,0x06,0xB0,0xE8]]
                       }
  where
    Just mask = V.fromList [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    Just m1   = V.fromList (replicate 32 13)

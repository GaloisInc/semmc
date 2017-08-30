module Tests.CMNTest (cmnTests) where

import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector.Sized as V
import           Data.Word (Word32)

import qualified SemMC.Stochastic.Remote as R
import SemMC.ARM ( MachineState(..), Instruction )

cmnTests :: [R.TestCase MachineState Instruction]
cmnTests = [ testCMN1,testCMN2,testCMN3,testCMN4
           , testCMN5,testCMN6,testCMN7,testCMN8,testCMN9 ]


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
    Just grs = V.fromList [ 11, 15, 25 , 0, 0
                          , 20, 0, 0, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0
                          ]
    Just mask = V.fromList (replicate 16 0)
    Just frs  = V.fromList (replicate 32 0)
    Just m1   = V.fromList (replicate 32 0)
    cpsr_reg  = (16 :: Word32)

----------------------------------------------------------------------------
-- TESTS FOR CMN_Imm INSTRUCTION
----------------------------------------------------------------------------

-- | Test CMN_Imm r2 #-20, where r2 = 20
-- | should set CPSR_Z = 1 and all else to zero
-- | TODO: Why does CPSR_C get set to 1
testCMN1 :: R.TestCase MachineState Instruction
testCMN1 = defaultTest { R.testNonce = 01
                       , R.testContext = (R.testContext defaultTest) { gprs = grs }
                       , R.testProgram = [LB.pack [0x14,0x00,0x52,0xE3]]
                       }
  where
    Just grs = V.fromList [ 0, 0, 20, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0
                          ]


-- | Test CMN_Imm r2 #0, where r2 = 2^31
-- | should set CPSR_N = 1 and all else to zero
testCMN2 :: R.TestCase MachineState Instruction
testCMN2 = defaultTest { R.testNonce = 02
                       , R.testContext = (R.testContext defaultTest) { gprs = grs }
                       , R.testProgram = [LB.pack [0x00,0x00,0x72,0xE3]]
                       }
  where
    Just grs = V.fromList [ 0, 0, 2 ^ (31 :: Integer), 0, 0
                          , 0, 0, 0, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0
                          ]


-- | Test CMN_Imm r1 #1, where r1 = 2^32 - 1
-- | should set CPSR_C = 1 and CPSR_Z = 1 and all else to zero
testCMN3 :: R.TestCase MachineState Instruction
testCMN3 = defaultTest { R.testNonce = 03
                       , R.testContext = (R.testContext defaultTest) { gprs = grs }
                       , R.testProgram = [LB.pack [0x01,0x00,0x71,0xE3]]
                       }
  where
    Just grs = V.fromList [ 0, 2 ^ (32 :: Word32) - 1, 0, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0
                          ]


-----------------------------------------------------------------------------
-- Tests for CMN_Reg instruction
-----------------------------------------------------------------------------

-- | Tests CMN_Reg r1, r2, where r1 = 5 and r2 = #-5
-- | should set CPSR_Z = 1 and all else to zero
-- | TODO: Why does CPSR_C get set to 1?
testCMN4 :: R.TestCase MachineState Instruction
testCMN4 = defaultTest { R.testNonce = 05
                       , R.testContext = (R.testContext defaultTest) { gprs = grs }
                       , R.testProgram = [LB.pack [0x02,0x00,0x71,0xE1]]
                       }
  where
    Just grs = V.fromList [ 0, 5, -5, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0
                          ]


-- | Tests CMN_Reg r1, r2, where where r1 = 2^31 and r2 = 0
-- | should set CPSR_N = 1 and all else to zero
testCMN5 :: R.TestCase MachineState Instruction
testCMN5 = defaultTest { R.testNonce = 06
                       , R.testContext = (R.testContext defaultTest) { gprs = grs }
                       , R.testProgram = [LB.pack [0x02,0x00,0x71,0xE1]]
                       }
  where
    Just grs = V.fromList [ 0, 2 ^ (31 :: Integer), 0, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0
                          ]


-- | Test CMN_Reg r1, r2, where r1 = 2^32 - 1 and r2 = 3
-- | should set CPSR_C = 1 and all else to zero
testCMN6 :: R.TestCase MachineState Instruction
testCMN6 = defaultTest { R.testNonce = 07
                       , R.testContext = (R.testContext defaultTest) { gprs = grs }
                       , R.testProgram = [LB.pack [0x02,0x00,0x71,0xE1]]
                       }
  where
    Just grs = V.fromList [ 0, 2 ^ (32 :: Integer) - 1, 3, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0
                          ]

-----------------------------------------------------------------------------
-- Tests for CMN_Shifted (register shifted register) instruction
-----------------------------------------------------------------------------


-- | Execute CMN r0, r1, LSL r2, where r1 = 2^32 - 5, r1 = 2, r2 = 1
-- | Should set CPSR_N = 1 and all else to zero
testCMN7 :: R.TestCase MachineState Instruction
testCMN7 = defaultTest { R.testNonce = 08
                       , R.testContext = (R.testContext defaultTest) { gprs = grs }
                       , R.testProgram = [LB.pack [0x11,0x02,0x70,0xE1]]
                       }
  where
    Just grs = V.fromList [ 2 ^ (32 :: Integer) - 6, 2, 1, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0
                          ]

-- | Execute CMN r0, r1, LSL r2, where r1 = 2^32 - 5, r1 = 2, r2 = 4
-- | Should set CPSR_C = 1 and all else to zero
testCMN8 :: R.TestCase MachineState Instruction
testCMN8 = defaultTest { R.testNonce = 09
                       , R.testContext = (R.testContext defaultTest) { gprs = grs }
                       , R.testProgram = [LB.pack [0x11,0x02,0x70,0xE1]]
                       }
  where
    Just grs = V.fromList [ 2 ^ (32 :: Integer) - 6, 2, 4, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0
                          ]

-- | Execute CMN r7, r3, ASR r0, where r7 = 2^31 - 9, r3 = 8, r0 = 1
-- | Should set entire CPSR to zero
testCMN9 :: R.TestCase MachineState Instruction
testCMN9 = defaultTest { R.testNonce = 10
                       , R.testContext = (R.testContext defaultTest) { gprs = grs }
                       , R.testProgram = [LB.pack [0x53,0x00,0x77,0xE1]]
                       }
  where
    Just grs = V.fromList [ 1, 0, 0, 8, 0
                          , 0, 0, 2 ^ (31 :: Integer) - 9, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0
                          ]

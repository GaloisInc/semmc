module Tests.StoreTest (storeTests) where

import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector.Sized as V
import           Data.Word (Word32)

import qualified SemMC.Stochastic.Remote as R
import SemMC.ARM ( MachineState(..), Instruction )


storeTests :: [R.TestCase MachineState Instruction]
storeTests = [ testSTR1,testSTR2,testSTRB1,testSTRB2
             , testSTRB3,testSTRB4,testSTRB5,testSTRBT ]


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


------------------------------------------------------------------------------------
-- STR_IMM tests
------------------------------------------------------------------------------------

-- | Executes STR_Imm r0, [r5, #5]. Should load 13 into the 5th mem location in mem1
testSTR1 :: R.TestCase MachineState Instruction
testSTR1 = defaultTest { R.testNonce = 11
                       , R.testContext = (R.testContext defaultTest) { gprs = grs, gprs_mask = mask }
                       , R.testProgram = [LB.pack [0x05,0x00,0x85,0xE5]]
                       }
  where
    Just mask = V.fromList [ 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    Just grs = V.fromList [ 13, 10, 0, 0, 0
                          , 15, 0, 0, 0, 0
                          , 0, 0, 0, 0, 0
                          , 0
                          ]


-- | Executes STR_Imm r3, [r0, #7]. Should load 12 into the 7th mem location in mem2
testSTR2 :: R.TestCase MachineState Instruction
testSTR2 = defaultTest { R.testNonce = 12
                       , R.testContext = (R.testContext defaultTest) { gprs = grs, gprs_mask = mask }
                       , R.testProgram = [LB.pack [0x07,0x30,0x80,0xE5]]
                       }
  where
    Just mask = V.fromList [2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    Just grs  = V.fromList [ 1, 0, 0, 12, 0
                           , 0, 0, 0, 0, 0
                           , 0, 0, 0, 0, 0
                           , 0
                           ]

------------------------------------------------------------------------------------
-- STRB_Imm
------------------------------------------------------------------------------------

-- | Executes STRB_Imm r0, [r1], #2. Should load 15 into the 0th mem location in mem1 (post indexed)
-- | Should enable write back
testSTRB1 :: R.TestCase MachineState Instruction
testSTRB1 = defaultTest { R.testNonce = 21
                        , R.testContext = (R.testContext defaultTest) { gprs = grs, gprs_mask = mask }
                        , R.testProgram = [LB.pack [0x02,0x00,0xC1,0xE4]]
                        }
  where
    Just mask = V.fromList [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    Just grs  = V.fromList [ 15, 1, 0, 0, 0
                           , 0, 0, 0, 0, 0
                           , 0, 0, 0, 0, 0
                           , 0
                           ]

-- | Executes STRB_Imm r3, [r5, #4]. Should load 27 into the 4th mem location in mem1 (offset)
-- | Should disable writeback
testSTRB2 :: R.TestCase MachineState Instruction
testSTRB2 = defaultTest { R.testNonce = 22
                        , R.testContext = (R.testContext defaultTest) { gprs = grs, gprs_mask = mask }
                        , R.testProgram = [LB.pack [0x04,0x30,0xC5,0xE5]]
                        }
  where
    Just mask = V.fromList [ 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    Just grs  = V.fromList [ 0, 0, 0, 27, 0
                           , 0, 0, 0, 0, 0
                           , 0, 0, 0, 0, 0
                           , 0
                           ]

-- | Executes STRB_Imm r3, [r5, #4]!. Should load 27 into the 4th mem location in mem1 (pre-indexed)
-- | Should enable write back
testSTRB3 :: R.TestCase MachineState Instruction
testSTRB3 = defaultTest { R.testNonce = 23
                        , R.testContext = (R.testContext defaultTest) { gprs = grs, gprs_mask = mask }
                        , R.testProgram = [LB.pack [0x04,0x30,0xE5,0xE5]]
                        }
  where
    Just mask = V.fromList [ 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    Just grs  = V.fromList [ 0, 0, 0, 27, 0
                           , 0, 0, 0, 0, 0
                           , 0, 0, 0, 0, 0
                           , 0
                           ]

------------------------------------------------------------------------------------
-- STRB_Reg
------------------------------------------------------------------------------------

-- | Executes STRB_Reg r4, [r3, r8, lsr #2]!
-- | where r4 = 5, r3 = 0, and r8 = 4. Should set mem1[1] = 5 and wb to r3
testSTRB4 :: R.TestCase MachineState Instruction
testSTRB4 = defaultTest { R.testNonce = 24
                        , R.testContext = (R.testContext defaultTest) { gprs = grs, gprs_mask = mask }
                        , R.testProgram = [LB.pack [0x28,0x41,0xE3,0xE7]]
                        }
  where
    Just mask = V.fromList [ 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    Just grs  = V.fromList [ 0, 0, 0, 0, 5
                           , 0, 0, 0, 4, 0
                           , 0, 0, 0, 0, 0
                           , 0
                           ]


-- | Executes STRB_Reg r1, [r2, r3, lsl #3]
-- | where r1 = 29, r2 = 0, and r3 = 3. Should set mem2[24] = 29 and disable wb
testSTRB5 :: R.TestCase MachineState Instruction
testSTRB5 = defaultTest { R.testNonce = 25
                        , R.testContext = (R.testContext defaultTest) { gprs = grs, gprs_mask = mask }
                        , R.testProgram = [LB.pack [0x83,0x11,0xC2,0xE7]]
                        }
  where
    Just mask = V.fromList [ 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    Just grs  = V.fromList [ 0, 29, 0, 3, 0
                           , 0, 0, 0, 0, 0
                           , 0, 0, 0, 0, 0
                           , 0
                           ]

------------------------------------------------------------------------------------
-- STRBT (Encoding A2)
------------------------------------------------------------------------------------

-- | Executes strbt r1, [r2], r3, lsl #3
-- | where r1 = 29, r2 = 0, and r3 = 3. Should set mem2[0] = 29 and enable wb
testSTRBT :: R.TestCase MachineState Instruction
testSTRBT = defaultTest { R.testNonce = 25
                        , R.testContext = (R.testContext defaultTest) { gprs = grs, gprs_mask = mask }
                        , R.testProgram = [LB.pack [0x83,0x11,0xE2,0xE6]]
                        }
  where
    Just mask = V.fromList [ 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    Just grs  = V.fromList [ 0, 29, 0, 3, 0
                           , 0, 0, 0, 0, 0
                           , 0, 0, 0, 0, 0
                           , 0
                           ]

-- | The worklist managing the list of work items to process
module SemMC.Worklist (
  Worklist,
  fromList,
  null,
  takeWork,
  putWork
  ) where

import qualified Data.Sequence as Seq

import Prelude hiding ( null )

newtype Worklist a =
  Worklist { unWorklist :: Seq.Seq a }

fromList :: [a] -> Worklist a
fromList = Worklist . Seq.fromList

null :: Worklist a -> Bool
null (Worklist s) = Seq.null s

-- | Take an item off of the front of the worklist
takeWork :: Worklist a -> Maybe (a, Worklist a)
takeWork wl = do
  case Seq.viewl (unWorklist wl) of
      Seq.EmptyL -> Nothing
      someWork Seq.:< rest -> Just (someWork, Worklist rest)

-- | Put an item back onto the end of the worklist
putWork :: a -> Worklist a -> Worklist a
putWork work wl = wl { unWorklist = unWorklist wl Seq.|> work }

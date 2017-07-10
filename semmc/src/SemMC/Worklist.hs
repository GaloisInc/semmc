-- | The worklist managing the list of work items to process
module SemMC.Worklist (
  Worklist,
  fromList,
  takeWork,
  putWork
  ) where

import qualified Data.Sequence as Seq

newtype Worklist a =
  Worklist { unWorklist :: Seq.Seq a }

fromList :: [a] -> Worklist a
fromList = Worklist . Seq.fromList

-- | Take an item off of the front of the worklist
takeWork :: Worklist a -> Maybe (a, Worklist a)
takeWork wl = do
  case Seq.viewl (unWorklist wl) of
      Seq.EmptyL -> Nothing
      someWork Seq.:< rest -> Just (someWork, Worklist rest)

-- | Put an item back onto the end of the worklist
putWork :: a -> Worklist a -> Worklist a
putWork work wl = wl { unWorklist = unWorklist wl Seq.|> work }

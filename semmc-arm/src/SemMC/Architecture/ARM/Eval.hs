-- | Evaluators for location functions in formula definitions (e.g., memri_reg)

module SemMC.Architecture.ARM.Eval
    ( createSymbolicEntries
    )
    where


-- | Uninterpreted function names are mangled in SimpleBuilder, so we need to
-- create extra entries to match their mangled names.
--
-- In particular, periods in names are converted to underscores.
--
-- This function creates copies of entries with periods in their names with the
-- escaped version as it appears in a SimpleBuilder symbolic function.  For
-- example, if there is an entry with the name @arm.foo@, this function retains
-- that entry in the input list and adds an additional entry under @arm_foo@.
createSymbolicEntries :: [(String, a)] -> [(String, a)]
createSymbolicEntries = foldr duplicateIfDotted []
  where
    duplicateIfDotted elt@(s, e) acc =
      case '.' `elem` s of
        False -> acc
        True ->
          let newElt = (map (\c -> if c == '.' then '_' else c) s, e)
          in newElt : elt : acc

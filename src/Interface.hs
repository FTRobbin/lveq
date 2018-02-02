module Interface where

import Prot.Lang.Analyze

import Equivalence

-- | All exposed interfaces

-- | Same as leavesEquiv in SMT.hs
leafDagsEqv :: [LeafDag ret] -> [LeafDag ret] -> IO Bool
leafDagsEqv = fail "not implemented"


-- | Type transformations





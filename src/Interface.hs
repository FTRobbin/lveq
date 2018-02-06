module Interface where

import qualified Data.Set as Set
import Data.List

import qualified Prot.Lang.Expr as PExpr
import qualified Prot.Lang.Analyze as PAnalyze
import qualified Prot.Lang.Command as PCommand

import DAG
import Equivalence


-- | All exposed interfaces

-- | Similar to leavesEquiv in SMT.hs
leavesEqv :: [PAnalyze.Leaf ret] -> [PAnalyze.Leaf ret] -> IO Bool
leavesEqv = fail "not implemented"

-- | Similar to dagEquiv in SMT.hs
leafPairEqv :: PAnalyze.Leaf ret -> PAnalyze.Leaf ret -> Bool
leafPairEqv l r = dagFEqv (extractDag l) (extractDag r) (sampEqvF (extr l) (extr r))
    where extr = PAnalyze._leafSamps

-- | Type transformations

mapS :: (Eq a) => [a] -> a -> Int
mapS l a = 
    case (findIndex (== a) l) of
        Just i -> i + 1
        Nothing -> 0

extractDag :: PAnalyze.Leaf ret -> Dag
extractDag (PAnalyze.Leaf samps _ _)  =
    map (sort . (map (mapS $ map PAnalyze._sampname samps)) . Set.elems . Set.unions. (map PExpr.freeVars) . PAnalyze._sampargs) samps

sampEqvF :: [PAnalyze.Sampling] -> [PAnalyze.Sampling] -> Int -> Int -> Bool
sampEqvF la lb i j =  distEqvF (la !! (i - 1)) (lb !! (j - 1))

distEqvF (PAnalyze.Sampling dl _ _) (PAnalyze.Sampling dr _ _) = PCommand.compareDistr dl dr


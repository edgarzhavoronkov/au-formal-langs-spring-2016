{- |
Module          : NFA
Description     : Definition of Nondeterministic Finite Automata, and closure properties of regular languages
Author          : Daniele Micciancio
Stability       : experimental
-}
module NFA where
import Data.List (nub)

-- |Data type representing a Nondeterministic Finite Automaton, parametrized by a type of states.
-- We use the convention that the transition function delta(q,x) should be defined for all x::(Maybe Char),
-- and output [] when x=(Just a) for some a not in the alphabet Sigma.
type NFA st = ([st], [Char], st->(Maybe Char)->[st], st, st->Bool)


-- |NFA for empty string
epsilonNFA :: NFA () -- () is a special type, called unit, with only one element, also written as ()
epsilonNFA =
  let delta () a = [] -- There are no transitions
      inF () = True   -- Accepts empty string
  in ([()],[],delta,(),inF)

-- |NFA for empty language
emptyNFA :: NFA ()
emptyNFA =
  let delta () a = []
      inF () = False  -- Rejects all strings
  in ([()],[],delta,(),inF)

-- |NFA for singleton language
symbolNFA :: Char -> NFA (Either () ())
symbolNFA a =
  let qs = [Left (),Right()] -- There are just two states
      sigma = [a]
      q0 = Left ()
      inF (Left  ()) = False
      inF (Right ()) = True
      delta (Left ()) (Just x) = if (x == a) then  [Right ()] else []
      delta _ _ = []
  in  (qs,sigma,delta,q0,inF)

-- |Closure under star. Compare to the proof of Theorem 1.50
starNFA :: NFA st -> NFA (Either () st)
starNFA (qs,sigma,delta,q1,inF) =
  let q0 = Left ()
      qs1 = [q0] ++ (map Right qs)
      inF1 (Left ()) = True
      inF1 (Right q) = inF q
      delta1 (Left ()) Nothing  = [Right q1]              -- eps transition from the new start state to the old start state
      delta1 (Right q) Nothing | (inF q) = map Right (delta q Nothing ++ [q1])
      delta1 (Right q) x = map Right (delta q x)       -- otherswise, just like in original NFA
      delta1 _ _ = []
  in (qs1,sigma,delta1,q0,inF1)

-- |Closure under concatenation. Compare to proof of Theorem 1.47 in the textbook
concatNFA :: NFA st1 -> NFA st2 -> NFA (Either st1 st2)
concatNFA (qs1,sigma1,delta1,q1,inF1) (qs2,sigma2,delta2,q2,inF2) =
  let qs = (map Left qs1) ++ (map Right qs2)
      q0 = Left q1
      sigma = nub (sigma1 ++ sigma2) -- union of the alphabets. nub removes duplicates
      inF (Left  r) = False
      inF (Right r) = inF2 r
      delta (Right q) x = map Right (delta2 q x)
      delta (Left  q) Nothing | inF1 q = (map Left (delta1 q Nothing)) ++ [Right q2]
      delta (Left  q) x = map Left (delta1 q x) -- for all other q and x
  in (qs,sigma,delta,q0,inF)

-- |Closure under union. Compare to proof of Theorem 1.45 in the textbook
unionNFA :: NFA st1 -> NFA st2 -> NFA (Either () (Either st1 st2))
unionNFA (qs1,sigma1,delta1,q1,inF1) (qs2,sigma2,delta2,q2,inF2) =
  let q0 = Left ()
      qs = [q0] ++ (map Right (map Left qs1 ++ map Right qs2))
      sigma = nub (sigma1 ++ sigma2) -- union of the alphabets, with duplicates removed
      inF (Left ()) = False
      inF (Right (Left q))  = inF1 q
      inF (Right (Right q)) = inF2 q
      delta (Left ()) Nothing = [Right (Left q1), Right (Right q2)]   -- transitions from the new start state "Nothing"
      delta (Right (Left  q)) x = map (Right . Left)  (delta1 q x) -- transitions from the first NFA
      delta (Right (Right q)) x = map (Right . Right) (delta2 q x) -- transitions from the second NFA
      delta _ _ = [] -- Default: there are not other transitions
  in (qs,sigma,delta,q0,inF)

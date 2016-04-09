module FST where
import DFA

type FST st = ([st], [Char], [Char], st->Char->(st,[Char]), st)

evalFST :: FST st -> [Char] -> [Char]
evalFST fst "" = ""
evalFST (qs, sigma, gamma, delta, s) (a:w) =
  let (q,u) = delta s a
      v = evalFST (qs, sigma, gamma, delta, q) w
  in u ++ v

type NFST st = ([st], [Char], [Char], st->Char->[(st,[Char])], st)

composeNFST :: NFST st1 -> NFST st2 -> NFST (st1,st2)
composeNFST (qs1, sigma1, gamma1, delta1, s1)
            (qs2, sigma2, gamma2, delta2, s2) | sigma2==gamma1 =
  let qs = [(q1,q2) | q1 <- qs1, q2 <- qs2]
      s = (s1,s2)
      delta (q1,q2) a = do
        (r1,w) <- delta1 q1 a
        (r2,u) <- delta2Star q2 w
        return ((r1,r2),u)
      delta2Star q "" = return (q,"")
      delta2Star q (x:xs) = do
        (r,v) <- delta2 q x
        (t,z) <- delta2Star r xs
        return (t, v++z)
  in (qs, sigma1, gamma2, delta, s)

fst2nfst :: FST st -> NFST st
fst2nfst (qs, sigma, gamma, delta, s) =
  let delta1 q a  =[delta q a]
  in (qs, sigma, gamma, delta1, s)

{- |
Module          : JFF
Description     : Import/Export Deterministic Finite Automata from/to JFLAP xml format
Author          : Daniele Micciancio
Stability       : experimental
-}

module JFF (readDFA, writeDFA, writeNFA, readFST, readNFST, writeNFST) where
import Text.XML.Light
import Data.Maybe (isJust, mapMaybe)
import Data.List  (nub,sort,concatMap,findIndex)
import DFA
import NFA
import FST

getElems name = findElements (QName name Nothing Nothing)
getChild name = findChild (QName name Nothing Nothing)
getAttr  name = findAttr (QName name Nothing Nothing)

data State = State { stateIndex :: Int,  stateName :: String, initial :: Bool, final :: Bool }
     deriving Show

data Trans = Trans { fromState :: Int, toState :: Int, input :: String, output :: Maybe String }
     deriving Show

readState :: Element -> Maybe State
readState e = do
  i <- getAttr "id" e
  n <- getAttr "name" e
  let start = isJust (getChild "initial" e)
  let final = isJust (getChild "final" e)
  return $ State (read i) n start final

readTrans :: Element -> Maybe Trans
readTrans e = do
  s1 <- fmap (read . strContent) (getChild "from" e)
  s2 <- fmap (read . strContent) (getChild "to" e)
  x  <- fmap strContent (getChild "read" e)
  let y  = fmap strContent (getChild "transout" e)
  return $ Trans s1 s2 x y

-- |Parse input string containing the description of a DFA in JFLAP xml format (typically from a .jff file), and output
-- * (Just (DFA ...)) if parsing succeeds
-- * (Nothing) if input string is invalid
readDFA :: String -> Maybe (DFA Int)
readDFA dfa = do
  xmlDoc <- parseXMLDoc dfa
  states <- mapM readState (getElems "state" xmlDoc)
  trans  <- mapM readTrans (getElems "transition" xmlDoc)
  let delta q x = toState tr where
        [tr] = filter (\t -> (fromState t == q) && (input t == [x])) trans
  let s = let [st] = filter initial states in stateIndex st
  let qs = sort (map stateIndex states)
  let sigma = sort (nub (concatMap input trans))
  let fs q = any (\st -> stateIndex st == q && final st) states
  return (qs, sigma, delta, s, fs)

makeState :: Int -> Bool -> Element
makeState st f =
 let body = if f then [unode "final" ()] else []
 in add_attr (Attr (unqual "id") (show st)) (unode "state" body)

makeStartState :: Int -> Bool -> Element
makeStartState st f =
 let body = map (flip unode ()) (if f then ["final","initial"] else ["initial"])
 in add_attr (Attr (unqual "id") (show st)) (unode "state"  body)

makeTrans :: Int -> Char -> Int -> Maybe String -> Element
makeTrans q x p Nothing =
  unode "transition"
   [ unode "from" (show q),
     unode "to" (show p),
     unode "read" [x]]
makeTrans q x p (Just out) =
  unode "transition"
   [ unode "from" (show q),
     unode "to" (show p),
     unode "read" [x],
     unode "transout" out ]

makeEpsTrans :: Int -> Int -> Element
makeEpsTrans q p  =
  unode "transition"
   [ unode "from" (show q),
     unode "to" (show p),
     unode "read" ""]

-- |Converts a DFA into a string containing its JFLAP xml description to be written in a .jff file
writeDFA :: (Eq a) => DFA a -> String
writeDFA dfa =
  let (qs, sigma, delta, start, isFinal) = intDFA dfa
      states = makeStartState start (isFinal start) : [makeState q (isFinal q) | q <- qs, q /= start]
      transitions = [makeTrans q a (delta q a) Nothing | q <- qs, a <- sigma]
      xml = unode "structure" [ unode "type" "fa", unode "automaton" (states ++ transitions)]
  in showTopElement xml

writeNFA :: (Eq a) => NFA a -> String
writeNFA dfa =
  let (qs, sigma, delta, start, isFinal) = intNFA dfa
      states = makeStartState start (isFinal start) : [makeState q (isFinal q) | q <- qs, q /= start]
      transitions = [makeEpsTrans q q1 | q <- qs,       q1 <- delta q Nothing] ++
                    [makeTrans  q a q1 Nothing | q <- qs, a <- sigma, q1 <- delta q (Just a)]
      xml = unode "structure" [ unode "type" "fa", unode "automaton" (states ++ transitions)]
  in showTopElement xml

intDFA :: (Eq a) => DFA a -> DFA Int
intDFA (qs, alphabet, delta, s, isF) =
  let qs1 = [0 .. length qs - 1]
      intState n = maybe (error "Invalid DFA") id (findIndex (==n) qs)
      s1 = intState s
      isF1 n = isF (qs!!n)
      delta1 n x = intState (delta (qs!!n) x)
  in (qs1, alphabet, delta1, s1, isF1)

intNFA :: (Eq a) => NFA a -> NFA Int
intNFA (qs, alphabet, delta, s, isF) =
  let qs1 = [0 .. length qs - 1]
      intState n = maybe (error "Invalid NFA") id (findIndex (==n) qs)
      s1 = intState s
      isF1 n = isF (qs!!n)
      delta1 n x = map intState (delta (qs!!n) x)
  in (qs1, alphabet, delta1, s1, isF1)

------- FST -------

readFST :: String -> Maybe (FST Int)
readFST jff = do
  xmlDoc <- parseXMLDoc jff
  states <- mapM readState (getElems "state" xmlDoc)
  trans  <- mapM readTrans (getElems "transition" xmlDoc)
  let delta q x = (toState tr, out) where
        [tr] = filter (\t -> (fromState t == q) && (input t == [x])) trans
        Just out = output tr
  let s = let [st] = filter initial states in stateIndex st
  let qs = sort (map stateIndex states)
  let sigma = sort (nub (concatMap input trans))
  let gamma = sort (nub (concat (mapMaybe output trans)))
  return (qs, sigma, gamma, delta, s)

readNFST :: String -> Maybe (NFST Int)
readNFST jff = do
  xmlDoc <- parseXMLDoc jff
  states <- mapM readState (getElems "state" xmlDoc)
  trans  <- mapM readTrans (getElems "transition" xmlDoc)
  let delta q x = do
        tr <- [t | t<-trans, fromState t == q, input t == [x]]
        let Just out = output tr
        return (toState tr, out)
  let s = let [st] = filter initial states in stateIndex st
  let qs = sort (map stateIndex states)
  let sigma = sort (nub (concatMap input trans))
  let gamma = sort (nub (concat (mapMaybe output trans)))
  return (qs, sigma, gamma, delta, s)


intNFST :: (Eq a) => NFST a -> NFST Int
intNFST (qs, sigma, gamma, delta, s) =
  let qs1 = [0 .. length qs - 1]
      intState n = maybe (error "Invalid NFST") id (findIndex (==n) qs)
      s1 = intState s
      delta1 n x = [(intState q,w) | (q,w) <- (delta (qs!!n) x)]
  in (qs1, sigma, gamma, delta1, s1)


writeNFST :: (Eq st) => NFST st -> String
writeNFST nfst =
  let (qs, sigma, gamma, delta, start) = intNFST nfst
      states = makeStartState start False : [makeState q False | q <- qs, q /= start]
      transitions = [makeTrans  q a q1 (Just out) | q <- qs, a <- sigma, (q1,out) <- delta q a]
      xml = unode "structure" [ unode "type" "mealy", unode "automaton" (states ++ transitions)]
  in showTopElement xml

module TI.State where

import           Types

data Node =
  NAp Addr Addr
  | NSupercomb String [String] Expr
  | NNum Integer
  | NInd Addr
  | NPrim String Primitive
  | NData Integer [Addr]
  deriving Show

data Primitive =
  Neg
  | Add
  | Sub
  | Mul
  | Div
  | Construct Integer Integer
  | If
  | Less
  | LessEq
  | Greater
  | GreaterEq
  | Eq
  | NotEq
  | CasePair
  deriving Show

extraPreludeDefs :: Program
extraPreludeDefs =
  [ ( "and"
    , ["x", "y"]
    , EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "y")) (EVar "False")
    )
  , ( "or"
    , ["x", "y"]
    , EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "True")) (EVar "y")
    )
  , ( "not"
    , ["x"]
    , EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "False")) (EVar "True")
    )
  , ("fst", ["p"], EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K"))
  , ("snd", ["p"], EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K1"))
  , ( "head"
    , ["ls"]
    , EAp (EAp (EAp (EVar "caseList") (EVar "ls")) (EVar "abort")) (EVar "K")
    )
  , ( "tail"
    , ["ls"]
    , EAp (EAp (EAp (EVar "caseList") (EVar "ls")) (EVar "abort")) (EVar "K1")
    )
  ]

primitives :: [(String, Primitive)]
primitives =
  [ ("negate"  , Neg)
  , ("+"       , Add)
  , ("-"       , Sub)
  , ("*"       , Mul)
  , ("/"       , Div)
  , ("if"      , If)
  , (">"       , Greater)
  , (">="      , GreaterEq)
  , ("<"       , Less)
  , ("<="      , LessEq)
  , ("=="      , Eq)
  , ("/="      , NotEq)
  , ("casePair", CasePair)
  , ("True"    , Construct 0 0)
  , ("False"   , Construct 1 0)
  , ("MkPair"  , Construct 2 2)
  , ("Nil"     , Construct 3 0)
  , ("Cons"    , Construct 4 2)
  ]

isValueNode :: Node -> Bool
isValueNode node = isDataNode node || isNumNode node

isDataNode :: Node -> Bool
isDataNode (NData _ _) = True
isDataNode _           = False

isNumNode :: Node -> Bool
isNumNode (NNum _) = True
isNumNode _        = False

isTrueNode :: Node -> Bool
isTrueNode (NData 0 []) = True
isTrueNode _            = False

isFalseNode :: Node -> Bool
isFalseNode (NData 1 []) = True
isFalseNode _            = False

isPairNode (NData 2 [_, _]) = True
isPairNode _                = False

pairApply heap (NData 2 [x, y]) f = (heap2, app)
 where
  (heap1, addr ) = hAlloc heap (NAp f x)
  (heap2, addr1) = hAlloc heap1 (NAp addr y)
  app            = hLookup heap2 addr1
pairApply _ _ _ = error "Function expects a pair"

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

-- TiStack
type Addr = Integer
type TiStack = [Addr]

-- TiDump
type TiDump = [TiStack]

initialTiDump :: TiDump
initialTiDump = []

-- TiHeap
type TiHeap = Heap Node

data Heap a = Heap Addr [Addr] [(Addr, a)]


hInitial :: Heap a
hInitial = Heap 0 [1 ..] []

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (Heap size (next : free) xs) x =
  (Heap (size + 1) free ((next, x) : xs), next)
hAlloc (Heap _ [] _) _ = error "Heap.hs:hAlloc - Empty free list"

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (Heap size free xs) a x = Heap size free ((a, x) : remove xs a)

remove :: [(Integer, a)] -> Integer -> [(Integer, a)]
remove [] adr = error
  ("Heap.remove - Attemot to update or free nonexistent address" ++ show adr)
remove ((a, x) : xs) adr | a == adr  = xs
                         | otherwise = (a, x) : remove xs adr

hLookup :: Heap a -> Addr -> a
hLookup (Heap _ _ xs) a =
  aLookup xs a (error ("Heap.hLokup - can't find address " ++ show a))

hAddresses :: Heap a -> [Addr]
hAddresses (Heap _ _ xs) = [ addr | (addr, _) <- xs ]

-- TiGlobal
type TiGlobals = Assoc String Addr

type Assoc a b = [(a, b)]

aLookup :: (Eq k) => Assoc k v -> k -> v -> v
aLookup [] _ def = def
aLookup ((k', v) : as) k def | k == k'   = v
                             | otherwise = aLookup as k def

-- TiStats
data TiStats = TiStats Integer

tiStatInitial :: TiStats
tiStatInitial = TiStats 0

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps (TiStats n) = TiStats $ n + 1

tiStatGetSteps :: TiStats -> Integer
tiStatGetSteps (TiStats n) = n

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats statsFun (stack, dump, heap, scDefs, stats) =
  (stack, dump, heap, scDefs, statsFun stats)

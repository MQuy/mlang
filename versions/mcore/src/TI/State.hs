module TI.State where

import           Types

data Node =
  NAp Addr Addr
  | NSupercomb String [String] Expr
  | NNum Integer
  | NPrim String Primitive
  deriving Show

data Primitive = Neg | Add | Sub | Mul | Div deriving Show

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

isNumNode :: Node -> Bool
isNumNode (NNum _) = True
isNumNode _        = False

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

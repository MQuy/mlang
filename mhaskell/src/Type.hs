module Type where

import qualified Data.Map                      as M

-- Expression
data Expr =
  EVar String
  | ENum Int
  | EConst Int Int
  | EAp Expr Expr
  | ECase Expr [EAlter]
  | ELet Bool [EDef] Expr
  | ELam [String] Expr
  deriving Show

type EAlter = (Int, [String], Expr)
type EDef = (String, Expr)

type Program = [ScDefn]
type ScDefn = (String, [String], Expr)

preludeDefs :: Program
preludeDefs =
  [ ("I" , ["x"]     , EVar "x")
  , ("K" , ["x", "y"], EVar "x")
  , ("K1", ["x", "y"], EVar "y")
  , ( "S"
    , ["f", "g", "x"]
    , EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))
    )
  , ("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x")))
  , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
  ]

primitives :: Program
primitives =
  [ ("+"     , ["x", "y"], EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))
  , ("-"     , ["x", "y"], EAp (EAp (EVar "-") (EVar "x")) (EVar "y"))
  , ("*"     , ["x", "y"], EAp (EAp (EVar "*") (EVar "x")) (EVar "y"))
  , ("/"     , ["x", "y"], EAp (EAp (EVar "/") (EVar "x")) (EVar "y"))
  , ("negate", ["x"]     , EAp (EVar "negate") (EVar "x"))
  , ("=="    , ["x", "y"], EAp (EAp (EVar "==") (EVar "x")) (EVar "y"))
  , ("!="    , ["x", "y"], EAp (EAp (EVar "!=") (EVar "x")) (EVar "y"))
  , (">"     , ["x", "y"], EAp (EAp (EVar ">") (EVar "x")) (EVar "y"))
  , (">="    , ["x", "y"], EAp (EAp (EVar ">=") (EVar "x")) (EVar "y"))
  , ("<"     , ["x", "y"], EAp (EAp (EVar "<") (EVar "x")) (EVar "y"))
  , ("<="    , ["x", "y"], EAp (EAp (EVar "<=") (EVar "x")) (EVar "y"))
  , ( "if"
    , ["c", "t", "f"]
    , EAp (EAp (EAp (EVar "if") (EVar "c")) (EVar "t")) (EVar "f")
    )
  ]

preludeDcs =
  [(show cTrue, [], EConst cTrue 0), (show cFalse, [], EConst cFalse 0)]

-- Infor which is used during parsing {currentTag, {name: (tag, arity)}}
data PEInfo = PEInfo {
  currentTag :: Int,
  tName :: M.Map String (Int, Int)
}

-- MQ 2018-10-24
-- Supercombinator (top level function)'s first letter has to be character
-- therefore, we use data constructor's tag as sc's name
peInfo :: PEInfo
peInfo = PEInfo
  { currentTag = 16
  , tName      = M.fromList [("True", (cTrue, 0)), ("False", (cFalse, 1))]
  }

peTagInc :: String -> Int -> PEInfo -> PEInfo
peTagInc dName arity PEInfo { currentTag = tag, tName = tName } =
  PEInfo { currentTag = tag + 1, tName = M.insert dName (tag, arity) tName }

-- A function to find out whether we have an atomic expression
isAtomicExpr :: Expr -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _        = False

-- G-Machine State
type GMState = (GMOutput, GMCode, GMStack, GMVStack, GMDump, GMHeap, GMGlobal, GMStats)

-- Node
type Addr = Int
data Node =
  NVar String
  | NNum Int
  | NAp Addr Addr
  | NGlobal Int GMCode
  | NInd Addr
  | NConst Int [Addr]
  deriving Show

cTrue = 1 :: Int
cFalse = 2 :: Int

-- Output
type GMOutput = String

getOutput :: GMState -> GMOutput
getOutput (o, i, s, v, dump, heap, globals, stats) = o

putOutput :: GMOutput -> GMState -> GMState
putOutput o1 (o, i, s, v, dump, heap, globals, stats) =
  (o1, i, s, v, dump, heap, globals, stats)

-- Code
type GMCode = [Instruction]

data Instruction =
  Pushint Int
  | Pushglobal String
  | Push Int
  | Pop Int
  | Update Int
  | Slide Int
  | Alloc Int
  | Split Int
  | Pushbasic Int
  | Pack Int Int
  | Casejump [(Int, GMCode)]
  | Cond GMCode GMCode
  | Add | Sub | Mul | Div | Neg
  | Eq | Ne | Lt | Le | Gt | Ge
  | Mkint
  | Mkbool
  | Get
  | Mkap
  | Eval
  | Unwind
  | Print
  deriving Show

codeInitial = [Pushglobal "main", Eval, Print]

builtInDyadic :: [(String, Instruction)]
builtInDyadic =
  [ ("+" , Add)
  , ("-" , Sub)
  , ("*" , Mul)
  , ("/" , Div)
  , ("==", Eq)
  , ("!=", Ne)
  , (">=", Ge)
  , (">" , Gt)
  , ("<=", Le)
  , ("<" , Lt)
  ]

binaryOpBox :: [(String, Instruction)]
binaryOpBox =
  [ ("+" , Mkint)
  , ("-" , Mkint)
  , ("*" , Mkint)
  , ("/" , Mkint)
  , ("==", Mkbool)
  , ("!=", Mkbool)
  , (">=", Mkbool)
  , (">" , Mkbool)
  , ("<=", Mkbool)
  , ("<" , Mkbool)
  ]

getCode :: GMState -> GMCode
getCode (o, i, s, v, dump, heap, global, stats) = i

putCode :: GMCode -> GMState -> GMState
putCode i1 (o, i, s, v, dump, heap, globals, stats) =
  (o, i1, s, v, dump, heap, globals, stats)

-- Stack
type GMStack = [Addr]

getStack :: GMState -> GMStack
getStack (o, i, s, v, dump, heap, globals, stats) = s

putStack :: GMStack -> GMState -> GMState
putStack s1 (o, i, s, v, dump, heap, globals, stats) =
  (o, i, s1, v, dump, heap, globals, stats)

rearrange :: Int -> GMHeap -> GMStack -> GMStack
rearrange n heap as = take n as1 ++ drop n as
  where as1 = map (getArg . hLookup heap) (tail as)

getArg :: Node -> Addr
getArg (NAp a1 a2) = a2

-- VStack
type GMVStack = [Int]

getVStack :: GMState -> GMVStack
getVStack (o, i, s, v, dump, heap, globals, stats) = v

putVStack :: GMVStack -> GMState -> GMState
putVStack v1 (o, i, s, v, dump, heap, globals, stats) =
  (o, i, s, v1, dump, heap, globals, stats)

-- Dump
type GMDump = [(GMCode, GMStack)]

getDump :: GMState -> GMDump
getDump (o, i, s, v, dump, heap, globals, stats) = dump

putDump :: GMDump -> GMState -> GMState
putDump d1 (o, i, s, v, dump, heap, globals, stats) =
  (o, i, s, v, d1, heap, globals, stats)
-- Heap
data Heap a = Heap Int [Addr] [(Addr, a)]
type GMHeap = Heap Node

hInitial :: GMHeap
hInitial = Heap 0 [1 ..] []

hNull = 0 :: Addr

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (Heap size (next : free) xs) x =
  (Heap (size + 1) free ((next, x) : xs), next)
hAlloc (Heap _ [] _) _ = error "Heap.hs:hAlloc - Empty free list"

hLookup :: Heap a -> Addr -> a
hLookup (Heap _ _ xs) a =
  aLookup xs a (error ("Heap.hLokup - can't find address " ++ show a))

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (Heap size free xs) a x = Heap size free ((a, x) : remove xs a)

remove :: [(Int, a)] -> Int -> [(Int, a)]
remove [] adr = error
  ("Heap.remove - Attemot to update or free nonexistent address" ++ show adr)
remove ((a, x) : xs) adr | a == adr  = xs
                         | otherwise = (a, x) : remove xs adr

allocNodes :: Int -> GMHeap -> (GMHeap, [Addr])
allocNodes 0 heap = (heap, [])
allocNodes n heap = (heap2, a : as)
 where
  (heap1, as) = allocNodes (n - 1) heap
  (heap2, a ) = hAlloc heap1 (NInd hNull)

getHeap :: GMState -> GMHeap
getHeap (o, i, s, v, dump, heap, globals, stats) = heap

putHeap :: GMHeap -> GMState -> GMState
putHeap h1 (o, i, s, v, dump, heap, globals, stats) =
  (o, i, s, v, dump, h1, globals, stats)

-- Globals
type GMGlobal = [(String, Addr)]

aDomain :: GMGlobal -> [String]
aDomain alist = [ key | (key, val) <- alist ]

aLookup :: (Eq k) => [(k, v)] -> k -> v -> v
aLookup [] _ def = def
aLookup ((k', v) : as) k def | k == k'   = v
                             | otherwise = aLookup as k def

getGlobals :: GMState -> GMGlobal
getGlobals (o, i, s, v, dump, heap, globals, stats) = globals

-- GMStats
type GMStats = Int

statsInitial = 0 :: Int

getStats :: GMState -> Int
getStats (o, i, s, v, dump, heap, global, stats) = stats

replaceStats :: GMStats -> GMState -> GMState
replaceStats stats (o, i, s, v, dump, heap, global, _) =
  (o, i, s, v, dump, heap, global, stats)

statGetSteps :: Int -> Int
statGetSteps s = s

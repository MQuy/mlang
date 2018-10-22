module G.State where

import           Types

type GMState = (GMOutput, GMCode, GMStack, GMDump, GMVStack, GMHeap, GMGlobals, GMStats)

-- GMOutput
type GMOutput = String

getOutput :: GMState -> GMOutput
getOutput (o, i, stack, dump, vstack, heap, globals, stats) = o

putOutput :: GMOutput -> GMState -> GMState
putOutput o' (o, i, stack, dump, vstack, heap, globals, stats) =
  (o', i, stack, dump, vstack, heap, globals, stats)

-- GMCode
type GMCode = [Instruction]

data Instruction
  = Unwind
  | Pushglobal String
  | Pushint Int
  | Push Int
  | Mkap
  | Update Int
  | Pop Int
  | Slide Int
  | Alloc Int
  | Eval
  | Add | Sub | Mul | Div | Neg
  | Eq | Ne | Lt | Le | Gt | Ge
  | Cond GMCode GMCode
  | Pack Int Int
  | Casejump [(Int, GMCode)]
  | Split Int
  | Print
  | Pushbasic Int
  | Get
  | Mkint
  | Mkbool
  deriving Show

initialCode :: GMCode
initialCode = [Pushglobal "main", Eval, Print]

getCode :: GMState -> GMCode
getCode (o, i, stack, dump, vstack, heap, globals, stats) = i

putCode :: GMCode -> GMState -> GMState
putCode i1 (o, i, stack, dump, vstack, heap, globals, stats) =
  (o, i1, stack, dump, vstack, heap, globals, stats)

-- GMStack
type Addr = Int

type GMStack = [Addr]

getStack :: GMState -> GMStack
getStack (o, i, stack, dump, vstack, heap, globals, stats) = stack

putStack :: GMStack -> GMState -> GMState
putStack stack1 (o, i, stack, dump, vstack, heap, globals, stats) =
  (o, i, stack1, dump, vstack, heap, globals, stats)

-- GMDump
type GMDump = [(GMCode, GMStack)]

getDump :: GMState -> GMDump
getDump (o, i, stack, dump, vstack, heap, globals, stats) = dump

putDump :: GMDump -> GMState -> GMState
putDump dump1 (o, i, stack, dump, vstack, heap, globals, stats) =
  (o, i, stack, dump1, vstack, heap, globals, stats)

-- GMVStack
type GMVStack = [Int]

getVStack :: GMState -> GMVStack
getVStack (o, i, stack, dump, vstack, heap, globals, stats) = vstack

putVStack :: GMVStack -> GMState -> GMState
putVStack newVstack (o, i, stack, dump, vstack, heap, globals, stats) =
  (o, i, stack, dump, newVstack, heap, globals, stats)

-- GMHeap
data Node
  = NNum Int
  | NAp Addr Addr
  | NGlobal Int GMCode
  | NInd Addr
  | NConstr Int [Addr]
  deriving Show

cTrue = 1 :: Int
cFalse = 2 :: Int

data Heap a = Heap Int [Addr] [(Addr, a)]

type GMHeap = Heap Node

getHeap :: GMState -> GMHeap
getHeap (o, i, stack, dump, vstack, heap, globals, stats) = heap

putHeap :: GMHeap -> GMState -> GMState
putHeap heap1 (o, i, stack, dump, vstack, heap, globals, stats) =
  (o, i, stack, dump, vstack, heap1, globals, stats)

hInitial :: Heap a
hInitial = Heap 0 [1 ..] []

hNull :: Addr
hNull = 0

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (Heap size (next : free) xs) x =
  (Heap (size + 1) free ((next, x) : xs), next)
hAlloc (Heap _ [] _) _ = error "Heap.hs:hAlloc - Empty free list"

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (Heap size free xs) a x = Heap size free ((a, x) : remove xs a)

remove :: [(Int, a)] -> Int -> [(Int, a)]
remove [] adr = error
  ("Heap.remove - Attemot to update or free nonexistent address" ++ show adr)
remove ((a, x) : xs) adr | a == adr  = xs
                         | otherwise = (a, x) : remove xs adr

hLookup :: Heap a -> Addr -> a
hLookup (Heap _ _ xs) a =
  aLookup xs a (error ("Heap.hLokup - can't find address " ++ show a))

hAddresses :: Heap a -> [Addr]
hAddresses (Heap _ _ xs) = [ addr | (addr, _) <- xs ]

getArg :: Node -> Addr
getArg (NAp a1 a2) = a2

rearrange :: Int -> GMHeap -> GMStack -> GMStack
rearrange n heap as = take n as1 ++ drop n as
  where as1 = map (getArg . hLookup heap) (tail as)

allocNodes :: Int -> GMHeap -> (GMHeap, [Addr])
allocNodes 0 heap = (heap, [])
allocNodes n heap = (heap2, a : as)
 where
  (heap1, as) = allocNodes (n - 1) heap
  (heap2, a ) = hAlloc heap1 (NInd hNull)

-- GMGlobals
type Assoc a b = [(a, b)]

type GMGlobals = Assoc String Addr

getGlobals :: GMState -> GMGlobals
getGlobals (o, i, stack, dump, vstack, heap, globals, stats) = globals

aLookup :: (Eq k) => Assoc k v -> k -> v -> v
aLookup [] _ def = def
aLookup ((k', v) : as) k def | k == k'   = v
                             | otherwise = aLookup as k def

aDomain :: Assoc a b -> [a]
aDomain alist = [ key | (key, val) <- alist ]

-- GMStats
type GMStats = Int

statInitial :: GMStats
statInitial = 0

statIncSteps :: Int -> Int
statIncSteps s = s + 1

statGetSteps :: Int -> Int
statGetSteps s = s

getStats :: GMState -> GMStats
getStats (o, i, stack, dump, vstack, heap, globals, stats) = stats

putStats :: GMStats -> GMState -> GMState
putStats stats1 (o, i, stack, dump, vstack, heap, globals, stats) =
  (o, i, stack, dump, vstack, heap, globals, stats1)

-- Environment
type GMCompiledSC = (String, Int, GMCode)

type GMEnvironment = Assoc String Int

type GMCompiler = Expr -> GMEnvironment -> GMCode

primitives :: Program
primitives =
  [ ("+"     , ["x", "y"], EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))
  , ("-"     , ["x", "y"], EAp (EAp (EVar "-") (EVar "x")) (EVar "y"))
  , ("*"     , ["x", "y"], EAp (EAp (EVar "*") (EVar "x")) (EVar "y"))
  , ("/"     , ["x", "y"], EAp (EAp (EVar "/") (EVar "x")) (EVar "y"))
  , ("negate", ["x"]     , EAp (EVar "negate") (EVar "x"))
  , ("=="    , ["x", "y"], EAp (EAp (EVar "==") (EVar "x")) (EVar "y"))
  , ("~="    , ["x", "y"], EAp (EAp (EVar "˜=") (EVar "x")) (EVar "y"))
  , (">="    , ["x", "y"], EAp (EAp (EVar ">=") (EVar "x")) (EVar "y"))
  , (">"     , ["x", "y"], EAp (EAp (EVar ">") (EVar "x")) (EVar "y"))
  , ("<="    , ["x", "y"], EAp (EAp (EVar "<=") (EVar "x")) (EVar "y"))
  , ("<"     , ["x", "y"], EAp (EAp (EVar "<") (EVar "x")) (EVar "y"))
  , ( "if"
    , ["c", "t", "f"]
    , EAp (EAp (EAp (EVar "if") (EVar "c")) (EVar "t")) (EVar "f")
    )
  , ("True" , [], EConst 1 0)
  , ("False", [], EConst 2 0)
  ]

compiledPrimitives :: [GMCompiledSC]
compiledPrimitives =
  [ ("+"     , 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind])
  , ("-"     , 2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind])
  , ("*"     , 2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind])
  , ("/"     , 2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind])
  , ("negate", 1, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind])
  , ("=="    , 2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind])
  , ("~="    , 2, [Push 1, Eval, Push 1, Eval, Ne, Update 2, Pop 2, Unwind])
  , ("<"     , 2, [Push 1, Eval, Push 1, Eval, Lt, Update 2, Pop 2, Unwind])
  , ("<="    , 2, [Push 1, Eval, Push 1, Eval, Le, Update 2, Pop 2, Unwind])
  , (">"     , 2, [Push 1, Eval, Push 1, Eval, Gt, Update 2, Pop 2, Unwind])
  , (">="    , 2, [Push 1, Eval, Push 1, Eval, Ge, Update 2, Pop 2, Unwind])
  , ("if", 3, [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])
  ]

builtInDyadic :: Assoc String Instruction
builtInDyadic =
  [ ("+"  , Add)
  , ("-"  , Sub)
  , ("*"  , Mul)
  , ("div", Div)
  , ("==" , Eq)
  , ("~=" , Ne)
  , (">=" , Ge)
  , (">"  , Gt)
  , ("<=" , Le)
  , ("<"  , Lt)
  ]

binaryOpBox :: Assoc String Instruction
binaryOpBox =
  [ ("+" , Mkint)
  , ("-" , Mkint)
  , ("*" , Mkint)
  , ("/" , Mkint)
  , ("==", Mkbool)
  , ("/=", Mkbool)
  , (">=", Mkbool)
  , (">" , Mkbool)
  , ("<=", Mkbool)
  , ("<" , Mkbool)
  ]

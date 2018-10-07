module Template where


import           Data.List                      ( mapAccumL
                                                , sort
                                                )
import           Text.Parsec                    ( parse )
import           Types
import           Parser
import           Print

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

data Node = NAp Addr Addr
          | NSupercomb String [String] Expr
          | NNum Integer
          | NPrim String Primitive

data Primitive = Neg | Add | Sub | Mul | Div deriving Show

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

-- Compile
compile :: Program -> TiState
compile program =
  (initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
 where
  initialStack           = [addressOfMain]
  (initialHeap, globals) = buildInitialHeap scDefs
  addressOfMain          = aLookup globals "main" (error "main is not defined")
  scDefs                 = program ++ preludeDefs ++ extraPreludeDefs

extraPreludeDefs :: [a]
extraPreludeDefs = []

primitives :: [(String, Primitive)]
primitives = [("negate", Neg), ("+", Add), ("-", Sub), ("*", Mul), ("/", Div)]

buildInitialHeap :: [ScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs = (heap1, sc_addrs ++ pri_addrs)
 where
  (heap , sc_addrs ) = mapAccumL allocateSc hInitial scDefs
  (heap1, pri_addrs) = mapAccumL allocatePrim heap primitives

allocateSc :: TiHeap -> ScDefn -> (TiHeap, (String, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
  where (heap', addr) = hAlloc heap (NSupercomb name args body)

allocatePrim :: TiHeap -> (String, Primitive) -> (TiHeap, (String, Addr))
allocatePrim heap (name, prim) =
  let (heap1, addr) = hAlloc heap (NPrim name prim) in (heap1, (name, addr))

-- Eval
eval :: TiState -> [TiState]
eval state = state : restStates
 where
  restStates | tiFinal state = []
             | otherwise     = eval nextState
  nextState = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin = applyToStats tiStatIncSteps

tiFinal :: TiState -> Bool
tiFinal ([soleAddr], _, heap, _, _) = isDataNode (hLookup heap soleAddr)
tiFinal ([]        , _, _   , _, _) = error "Empty stack"
tiFinal _                           = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

isNumNode :: Node -> Bool
isNumNode (NNum _) = True
isNumNode _        = False

step :: TiState -> TiState
step state@(stack, _, heap, _, _) = dispatch (hLookup heap (head stack))
 where
  dispatch (NNum n                 ) = numStep state n
  dispatch (NAp a1 a2              ) = apStep state a1 a2
  dispatch (NSupercomb sc args body) = scStep state sc args body
  dispatch (NPrim name primitive   ) = primStep state primitive

numStep :: TiState -> Integer -> TiState
numStep _ n = error $ "Number applied as a function " ++ show n

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 _ =
  (a1 : stack, dump, heap, globals, stats)

scStep :: TiState -> String -> [String] -> Expr -> TiState
scStep (stack, dump, heap, globals, stats) sc argNames body =
  (new_stack, dump, new_heap, globals, stats)
 where
  new_stack               = result_addr : (drop (length argNames + 1) stack)
  (new_heap, result_addr) = instantiate body heap env
  env                     = arg_bindings ++ globals
  stackArgs               = getArgs heap stack
  arg_bindings | length stackArgs >= (length argNames) = zip argNames stackArgs
               | otherwise = error $ "Not enough arguments for function " ++ sc

primStep :: TiState -> Primitive -> TiState
primStep state Neg = primNeg state
primStep state Add = primArith state (+)
primStep _     _   = error $ "not supported primitive step"

primNeg :: TiState -> TiState
primNeg (_ : root : stackRest, dump, heap, globals, stats) = state1
 where
  addr = getArg heap root
  node = hLookup heap addr
  state1
    | isNumNode node
    = let (heap1, addr1) = hAlloc heap (fromUnary negate node)
      in  (addr1 : stackRest, dump, heap1, globals, stats)
    | otherwise
    = error "L#171"
primNeg _ = error "L#172"

primArith :: TiState -> (Integer -> Integer -> Integer) -> TiState
primArith (_ : xRoot : yRoot : stackRest, dump, heap, globals, stats) f =
  state1
 where
  (xAddr, yAddr) = (getArg heap xRoot, getArg heap yRoot)
  (x    , y    ) = (hLookup heap xAddr, hLookup heap yAddr)
  state1
    | isNumNode x && isNumNode y
    = let (heap1, addr1) = hAlloc heap (fromBinary f x y)
      in  (addr1 : stackRest, dump, heap1, globals, stats)
    | otherwise
    = error "#L186"
primArith _ _ = error "#L187"

fromUnary :: (Integer -> Integer) -> Node -> Node
fromUnary f (NNum n) = NNum $ f n
fromUnary _ _        = error "#L191"

fromBinary :: (Integer -> Integer -> Integer) -> Node -> Node -> Node
fromBinary f (NNum x) (NNum y) = NNum $ f x y
fromBinary _ _        _        = error "#L195"

getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (_ : stack) = map (getArg heap) stack
getArgs _    []          = []

getArg :: Heap Node -> Addr -> Addr
getArg heap addr = arg where (NAp _ arg) = hLookup heap addr

--
instantiate
  :: Expr                   -- Body of supercombinator
  -> TiHeap                 -- Heap before instantiation
  -> TiGlobals              -- Association of names to addresses
  -> (TiHeap, Addr)         -- Heap after instantiation, and address of root of instance
instantiate (ENum n   ) heap _   = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env = hAlloc heap2 (NAp a1 a2)
 where
  (heap1, a1) = instantiate e1 heap env
  (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env =
  (heap, aLookup env v (error ("Undefined name " ++ show v)))
instantiate (EConst tag arity) heap env = instantiateConstr tag arity heap env
instantiate (ELet isrec defs body) heap env =
  instantiateLet isrec defs body heap env
instantiate (ECase _ _) _ _ = error "Can't instantiate case exprs"
instantiate (ELam  _ _) _ _ = error "Can't instantiate lambda exprs"

instantiateConstr :: p1 -> p2 -> p3 -> p4 -> a
-- instantiateConstr tag arity heap env = undefined
instantiateConstr _ _ _ _ = error "Can't instantiate construtors yet"

instantiateLet
  :: Bool -> [(String, Expr)] -> Expr -> TiHeap -> TiGlobals -> (TiHeap, Addr)
instantiateLet _ defs body heap env =
  let (heap1, env1) = foldl extendEnvLet (heap, env) defs
      (heap2, addr) = instantiate body heap1 env1
  in  hAlloc heap (hLookup heap2 addr)

extendEnvLet :: (TiHeap, TiGlobals) -> (String, Expr) -> (TiHeap, TiGlobals)
extendEnvLet (heap, env) (name, expr) =
  let (heap1, addr) = instantiate expr heap env
  in  (heap1, env ++ [(name, addr)])

-- Show results
showResults :: [TiState] -> String
showResults states =
  iDisplay (iConcat [iLayn (map showState states), showStats (last states)])

showState :: TiState -> Iseq
showState (stack, _, heap, _, _) = iConcat [showStack heap stack, iNewline]

showStack :: TiHeap -> TiStack -> Iseq
showStack heap stack = iConcat
  [ iStr "Stk ["
  , iIndent (iInterleave iNewline (map showStackItem stack))
  , iStr " ]"
  ]
 where
  showStackItem addr =
    iConcat [showFWAddr addr, iStr ": ", showStkNode heap (hLookup heap addr)]

showStkNode :: TiHeap -> Node -> Iseq
showStkNode heap (NAp funAddr argAddr) = iConcat
  [ iStr "NAp "
  , showFWAddr funAddr
  , showDetail funAddr
  , iStr " "
  , showFWAddr argAddr
  , showDetail argAddr
  ]
 where
  showDetail addr = iConcat [iStr " (", showNode (hLookup heap addr), iStr ")"]
showStkNode _ node = showNode node

showNode :: Node -> Iseq
showNode (NAp a1 a2) =
  iConcat [iStr "NAp ", showAddr a1, iStr " ", showAddr a2]
showNode (NSupercomb name _ _) = iStr ("NSupercomb " ++ name)
showNode (NNum n             ) = (iStr "NNum ") `iAppend` (iNum n)
showNode (NPrim name _       ) = (iStr "NPrim ") `iAppend` (iStr name)

showAddr :: Addr -> Iseq
showAddr addr = iStr (show addr)

showFWAddr :: Addr -> Iseq
showFWAddr addr = iStr (genSpaces (4 - toInteger (length str)) ++ str)
  where str = show addr

showStats :: TiState -> Iseq
showStats (_, _, heap, _, stats) = iConcat
  [ iStr "Total number of steps = "
  , iNum (tiStatGetSteps stats)
  , iNewline
  , iNewline
  , showHeap heap
  ]

-- | Print out the contents of the heap
showHeap :: TiHeap -> Iseq
showHeap heap = iConcat
  [iStr "Heap:", iIndent (iConcat (map showHeapAddr addrs))]
 where
  addrs = sort $ hAddresses heap
  showHeapAddr addr =
    iConcat [iNewline, iNum addr, iStr " ", showNode (hLookup heap addr)]

runTemplate :: String -> IO ()
runTemplate s = case parse pProgram "core" s of
  Right p -> putStrLn $ showResults $ eval $ compile p
  Left  _ -> putStrLn "no"

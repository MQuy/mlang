module TI.Eval where

import           Types
import           TI.State

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

fromUnary :: (Integer -> Integer) -> Node -> Node
fromUnary f (NNum n) = NNum $ f n

fromBinary :: (Integer -> Integer -> Integer) -> Node -> Node -> Node
fromBinary f (NNum x) (NNum y) = NNum $ f x y

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

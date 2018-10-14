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
tiFinal ([soleAddr], [], heap, _, _) = isValueNode (hLookup heap soleAddr)
tiFinal ([]        , [], _   , _, _) = error "Empty stack"
tiFinal _                            = False

step :: TiState -> TiState
step state@(stack, _, heap, _, _) = dispatch (hLookup heap (head stack))
 where
  dispatch (NNum n                 ) = numStep state n
  dispatch (NAp a1 a2              ) = apStep state a1 a2
  dispatch (NSupercomb sc args body) = scStep state sc args body
  dispatch (NPrim name primitive   ) = primStep state primitive
  dispatch (NInd addr              ) = scInd state addr
  dispatch (NData tag _            ) = dataStep state tag

dataStep :: TiState -> Integer -> TiState
dataStep (stack@(_ : stackRest), dump, heap, globals, stats) tag = case dump of
  d : ds -> (d ++ stackRest, ds, heap, globals, stats)
  _      -> error $ "Data constructor applied as a function " ++ show tag

numStep :: TiState -> Integer -> TiState
numStep (stack@(_ : stackRest), dump, heap, globals, stats) n = case dump of
  d : ds -> (d ++ stackRest, ds, heap, globals, stats)
  _      -> error $ "Number applied as a function " ++ show n

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack@(top : _), dump, heap, globals, stats) a1 a2 =
  case hLookup heap a2 of
    NInd a3 -> (a1 : stack, dump, hUpdate heap top (NAp a1 a3), globals, stats)
    _       -> (a1 : stack, dump, heap, globals, stats)

scInd :: TiState -> Addr -> TiState
scInd (stack, dump, heap, globals, stats) addr =
  (addr : tail stack, dump, heap, globals, stats)

scStep :: TiState -> String -> [String] -> Expr -> TiState
scStep (stack@(stackTop : stackRest), dump, heap, globals, stats) sc argNames body
  = (stack1, dump, heap1, globals, stats)
 where
  root      = stack !! length argNames
  stack1    = root : drop (length argNames + 1) stack
  heap1     = instantiateAndUpdate body heap env root
  env       = arg_bindings ++ globals
  stackArgs = getArgs heap stack
  arg_bindings | length stackArgs >= length argNames = zip argNames stackArgs
               | otherwise = error $ "Not enough arguments for function " ++ sc

primStep :: TiState -> Primitive -> TiState
primStep state Neg                   = primUnary state negate
primStep state Add                   = primBinary state (fromBinary (+))
primStep state Sub                   = primBinary state (fromBinary (-))
primStep state Mul                   = primBinary state (fromBinary (*))
primStep state Div                   = primBinary state (fromBinary div)
primStep state Less                  = primBinary state (fromRelational (<))
primStep state LessEq                = primBinary state (fromRelational (<=))
primStep state Greater               = primBinary state (fromRelational (>))
primStep state GreaterEq             = primBinary state (fromRelational (>=))
primStep state Eq                    = primBinary state (fromRelational (==))
primStep state NotEq                 = primBinary state (fromRelational (/=))
primStep state (Construct tag arity) = primConstruct state tag arity
primStep state If                    = primIf state
primStep state CasePair              = primCasePair state

primCasePair :: TiState -> TiState
primCasePair state@(_ : p : f : stackRest, dump, heap, globals, stats) = state1
 where
  (pAddr, fAddr) = (getArg heap p, getArg heap f)
  pair           = hLookup heap pAddr
  state1
    | isPairNode pair
    = let (heap1, app) = pairApply heap pair fAddr
      in  (f : stackRest, dump, hUpdate heap1 f app, globals, stats)
    | isDataNode pair
    = error "Expected a pair as argument to casePair"
    | otherwise
    = (pAddr : f : stackRest, [p] : dump, heap, globals, stats)

primCasePair _ = error "Malformed casePair-expression"

primIf :: TiState -> TiState
primIf (stack@(_ : c : t : e : restStack), dump, heap, globals, stats) =
  let
    (condAddr, thenAddr, elseAddr) =
      (getArg heap c, getArg heap t, getArg heap e)
    cond = hLookup heap condAddr
    state1
      | isTrueNode cond
      = (e : restStack, dump, hUpdate heap e (NInd thenAddr), globals, stats)
      | isFalseNode cond
      = (e : restStack, dump, hUpdate heap e (NInd elseAddr), globals, stats)
      | isValueNode cond
      = error "number in if condition"
      | otherwise
      = ([condAddr], stack : dump, heap, globals, stats)
  in
    state1

primConstruct :: TiState -> Integer -> Integer -> TiState
primConstruct (stack@(root : restStack), dump, heap, globals, stats) tag arity
  = let args   = take (fromIntegral arity) (getArgs heap stack)
        stack1 = drop (length args) stack
        heap1  = hUpdate heap (stack !! length args) (NData tag args)
    in  (stack1, dump, heap1, globals, stats)

primUnary :: TiState -> (Integer -> Integer) -> TiState
primUnary (stack@(_ : root : stackRest), dump, heap, globals, stats) f = state1
 where
  addr = getArg heap root
  node = hLookup heap addr
  state1
    | isNumNode node
    = let heap1 = hUpdate heap root (fromUnary f node)
      in  (root : stackRest, dump, heap1, globals, stats)
    | otherwise
    = ([addr], [root] : dump, heap, globals, stats)

primBinary :: TiState -> (Node -> Node -> Node) -> TiState
primBinary (stack@(_ : xRoot : yRoot : stackRest), dump, heap, globals, stats) f
  = state1
 where
  (xAddr, yAddr) = (getArg heap xRoot, getArg heap yRoot)
  (x    , y    ) = (hLookup heap xAddr, hLookup heap yAddr)
  state1
    | isNumNode x && isNumNode y
    = let heap1 = hUpdate heap yRoot (f x y)
      in  (yRoot : stackRest, dump, heap1, globals, stats)
    | not (isNumNode x)
    = ([xAddr], [yRoot] : dump, heap, globals, stats)
    | not (isNumNode y)
    = ([yAddr], [yRoot] : dump, heap, globals, stats)

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
instantiate (ELet isrec defs body) heap env =
  instantiateLet isrec defs body heap env
instantiate (EConst tag arity) heap env =
  hAlloc heap (NPrim "Pack" (Construct tag arity))
instantiate (ECase _ _) _ _ = error "Can't instantiate case exprs"
instantiate (ELam  _ _) _ _ = error "Can't instantiate lambda exprs"

instantiateAndUpdate :: Expr -> TiHeap -> TiGlobals -> Addr -> TiHeap
instantiateAndUpdate (ENum n) heap _ addr = hUpdate heap addr (NNum n)
instantiateAndUpdate (EVar v) heap env addr =
  hUpdate heap addr (NInd (aLookup env v (error ("Undefined name " ++ show v))))
instantiateAndUpdate (EAp e1 e2) heap env addr =
  let (heap1, a1) = instantiate e1 heap env
      (heap2, a2) = instantiate e2 heap1 env
  in  hUpdate heap2 addr (NAp a1 a2)
instantiateAndUpdate (EConst tag arity) heap _ addr =
  hUpdate heap addr (NPrim "Pack" (Construct tag arity))

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


-- Helper

fromUnary :: (Integer -> Integer) -> Node -> Node
fromUnary f (NNum n) = NNum $ f n

fromBinary :: (Integer -> Integer -> Integer) -> Node -> Node -> Node
fromBinary f (NNum x) (NNum y) = NNum $ f x y

fromRelational :: (Integer -> Integer -> Bool) -> (Node -> Node -> Node)
fromRelational f (NNum x) (NNum y) | f x y     = NData 0 []
                                   | otherwise = NData 1 []
fromRelational _ _ _ = error "Expected numeric argument(s)"

getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (_ : stack) = map (getArg heap) stack
getArgs _    []          = []

getArg :: Heap Node -> Addr -> Addr
getArg heap addr = arg where (NAp _ arg) = hLookup heap addr

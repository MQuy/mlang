module G.Eval where

import           G.State
import           Data.List

eval :: GMState -> [GMState]
eval state = state : restStates
 where
  restStates | gmFinal state = []
             | otherwise     = eval nextState
  nextState = doAdmin (step state)

doAdmin :: GMState -> GMState
doAdmin s = putStats (statIncSteps (getStats s)) s

gmFinal :: GMState -> Bool
gmFinal s = case getCode s of
  [] -> True
  _  -> False

step :: GMState -> GMState
step (o, i : is, stack, dump, vstack, heap, globals, stats) =
  dispatch i (o, is, stack, dump, vstack, heap, globals, stats)

dispatch :: Instruction -> GMState -> GMState
dispatch (Pushglobal f)  = pushglobal f
dispatch (Pushint    n)  = pushint n
dispatch Mkap            = mkap
dispatch (Push   n)      = push n
dispatch (Update n)      = update n
dispatch (Pop    n)      = pop n
dispatch Unwind          = unwind
dispatch (Slide n)       = slide n
dispatch (Alloc n)       = alloc n
dispatch Eval            = evalop
dispatch Add             = binaryArithmetric (+)
dispatch Sub             = binaryArithmetric (-)
dispatch Mul             = binaryArithmetric (*)
dispatch Div             = binaryArithmetric div
dispatch Neg             = unaryArithmetric negate
dispatch Eq              = comparison (==)
dispatch Ne              = comparison (/=)
dispatch Lt              = comparison (<)
dispatch Le              = comparison (<=)
dispatch Gt              = comparison (>)
dispatch Ge              = comparison (>=)
dispatch (Cond i1 i2   ) = cond i1 i2
dispatch (Pack t  a    ) = pack t a
dispatch (Casejump alts) = casejump alts
dispatch (Split    n   ) = split n
dispatch Print           = gmprint
dispatch (Pushbasic n)   = pushbasic n
dispatch Mkint           = mkint
dispatch Mkbool          = mkbool
dispatch Get             = get

pushglobal :: String -> GMState -> GMState
pushglobal f (o, i, stack, dump, vstack, heap, globals, stats)
  | isPrefixOf "Pack " f && notElem f (aDomain globals)
  = (o, i, a1 : stack, dump, vstack, heap1, (f, a1) : globals, stats)
  | otherwise
  = (o, i, a : stack, dump, vstack, heap, globals, stats)
 where
  a           = aLookup globals f (error ("Undeclared global " ++ f))
  (heap1, a1) = hAlloc heap (NGlobal n [Pack t n, Update 0, Unwind])
  [_, st, sn] = words f
  t           = read st :: Int
  n           = read sn :: Int

pushint :: Int -> GMState -> GMState
pushint n (o, i, stack, dump, vstack, heap, globals, stats) =
  case aLookup globals (show n) 0 of
    0 -> (o, i, a : stack, dump, vstack, heap1, globals1, stats)
     where
      (heap1, a) = hAlloc heap (NNum n)
      globals1   = globals ++ [(show n, a)]
    a -> (o, i, a : stack, dump, vstack, heap, globals, stats)

mkap :: GMState -> GMState
mkap (o, i, a1 : a2 : as, dump, vstack, heap, globals, stats) =
  (o, i, a : as, dump, vstack, heap1, globals, stats)
  where (heap1, a) = hAlloc heap (NAp a1 a2)

push :: Int -> GMState -> GMState
push n (o, i, stack, dump, vstack, heap, globals, stats) =
  (o, i, (stack !! n) : stack, dump, vstack, heap, globals, stats)

update :: Int -> GMState -> GMState
update n (o, i, a : as, dump, vstack, heap, globals, stats) =
  (o, i, as, dump, vstack, heap1, globals, stats)
  where heap1 = hUpdate heap (as !! n) (NInd a)

pop :: Int -> GMState -> GMState
pop n (o, i, stack, dump, vstack, heap, globals, stats) =
  (o, i, drop n stack, dump, vstack, heap, globals, stats)

slide :: Int -> GMState -> GMState
slide n (o, i, a : as, dump, vstack, heap, globals, stats) =
  (o, i, a : drop n as, dump, vstack, heap, globals, stats)

alloc :: Int -> GMState -> GMState
alloc n (o, i, stack, dump, vstack, heap, globals, stats) =
  (o, i, as ++ stack, dump, vstack, heap1, globals, stats)
  where (heap1, as) = allocNodes n heap

pack :: Int -> Int -> GMState -> GMState
pack tag arity (o, i, stack, dump, vstack, heap, globals, stats) =
  (o, i, a : drop arity stack, dump, vstack, heap1, globals, stats)
 where
  as         = take arity stack
  (heap1, a) = hAlloc heap (NConstr tag as)

casejump :: [(Int, GMCode)] -> GMState -> GMState
casejump alts (o, i, a : s, dump, vstack, heap, globals, stats) =
  (o, i1 ++ i, a : s, dump, vstack, heap, globals, stats)
 where
  (NConstr t as) = hLookup heap a
  i1             = aLookup alts t (error ("No case for constructor" ++ show t))

split :: Int -> GMState -> GMState
split j (o, i, a : s, dump, vstack, heap, globals, stats) =
  (o, i, as ++ s, dump, vstack, heap, globals, stats)
  where (NConstr t as) = hLookup heap a

gmprint :: GMState -> GMState
gmprint state = newState (hLookup (getHeap state) a) state
 where
  newState (NConstr t as) =
    putOutput ("Pack{" ++ show t ++ "," ++ show (length as) ++ "}")
      . -- KH
        putCode (printcode (length as) ++ getCode state)
      . putStack (as ++ s)
  newState (NNum n) = putOutput (show n) . putStack s
  newState n        = error "Print of non data structure"
  (a : s) = getStack state

printcode :: Int -> [Instruction]
printcode 0 = []
printcode n = Eval : Print : printcode (n - 1)

pushbasic :: Int -> GMState -> GMState
pushbasic n (o, i, s, dump, vstack, heap, globals, stats) =
  (o, i, s, dump, n : vstack, heap, globals, stats)

mkint :: GMState -> GMState
mkint (o, i, s, dump, n : vstack, heap, globals, stats) =
  (o, i, a : s, dump, vstack, heap1, globals, stats)
  where (heap1, a) = hAlloc heap (NNum n)

mkbool :: GMState -> GMState
mkbool (o, i, s, dump, t : vstack, heap, globals, stats) =
  (o, i, a : s, dump, vstack, heap1, globals, stats)
  where (heap1, a) = hAlloc heap (NConstr t [])

get :: GMState -> GMState
get (o, i, a : s, dump, vstack, heap, globals, stats) =
  (o, i, s, dump, v : vstack, heap, globals, stats)
 where
  node = hLookup heap a
  v    = case node of
    (NNum n      ) -> n
    (NConstr t []) -> t

unwind :: GMState -> GMState
unwind state@(o, i, a : as, dump, vstack, heap, globals, status) =
  newState (hLookup heap a) state

newState :: Node -> GMState -> GMState
newState (NNum n) state@(_, _, a : s, dump, vstack, heap, globals, stats) =
  updateFromDump a dump state
newState (NAp a1 a2) (o, i, a : as, dump, vstack, heap, globals, stats) =
  (o, [Unwind], a1 : a : as, dump, vstack, heap, globals, stats)
newState (NGlobal n c) state@(o, i, a : as, dump, vstack, heap, globals, stats)
  | length as >= n = (o, c, rs, dump, vstack, heap, globals, stats)
  | otherwise = (o, i1, last (a : as) : s1, d1, vstack, heap, globals, stats)
 where
  ((i1, s1) : d1) = dump
  rs              = rearrange n heap (a : as)
newState (NInd a1) (o, i, a : as, dump, vstack, heap, globals, stats) =
  (o, [Unwind], a1 : as, dump, vstack, heap, globals, stats)
newState (NConstr tag arity) state@(_, _, a : as, dump, vstack, heap, globals, stats)
  = updateFromDump a dump state

updateFromDump :: Addr -> GMDump -> GMState -> GMState
updateFromDump address dump state = case dump of
  []           -> state
  ((i, s) : d) -> putDump d $ putCode i $ putStack (address : s) state

evalop :: GMState -> GMState
evalop (o, i, a : s, dump, v, heap, globals, stats) =
  (o, [Unwind], [a], (i, s) : dump, v, heap, globals, stats)

boxInteger :: Int -> GMState -> GMState
boxInteger = pushint

unboxInteger :: Addr -> GMState -> Int
unboxInteger a state@(_, _, _, _, _, heap, _, _) = ub (hLookup heap a)
 where
  ub (NNum i) = i
  ub n        = error "Unboxing a non-integer"

unaryArithmetric :: (Int -> Int) -> (GMState -> GMState)
unaryArithmetric op (o, i, s, dump, v1 : v, heap, globals, stats) =
  (o, i, s, dump, op v1 : v, heap, globals, stats)

binaryArithmetric :: (Int -> Int -> Int) -> (GMState -> GMState)
binaryArithmetric op (o, i, s, dump, v1 : v2 : v, heap, globals, stats) =
  (o, i, s, dump, op v1 v2 : v, heap, globals, stats)

boxBoolean :: Bool -> GMState -> GMState
boxBoolean b state = putStack (a : getStack state) (putHeap h1 state)
 where
  (h1, a) = hAlloc (getHeap state) (NConstr b1 [])
  -- 1 <-> True, 2 <-> False
  b1 | b         = cTrue
     | otherwise = cFalse

comparison :: (Int -> Int -> Bool) -> GMState -> GMState
comparison op (o, i, s, dump, v1 : v2 : v, heap, globals, stats) =
  (o, i, s, dump, nBool : v, heap, globals, stats)
 where
  nBool | op v1 v2  = 1
        | otherwise = 2

cond :: GMCode -> GMCode -> GMState -> GMState
cond t f (o, i, s, dump, b : vstack, heap, globals, stats) =
  (o, c ++ i, s, dump, vstack, heap, globals, stats)
 where
  c | b == cTrue = t
    | otherwise  = f

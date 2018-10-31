module GEval where

import           Type

eval :: GMState -> [GMState]
eval state = state : restStates
 where
  restStates | gmFinal state = []
             | otherwise     = eval nextState
  nextState = doAdmin (step state)

gmFinal :: GMState -> Bool
gmFinal state = case getCode state of
  [] -> True
  _  -> False

doAdmin :: GMState -> GMState
doAdmin state = replaceStats (getStats state + 1) state

step :: GMState -> GMState
step (o, i : is, s, v, dump, heap, global, stats) =
  dispatch i (o, is, s, v, dump, heap, global, stats)

dispatch :: Instruction -> GMState -> GMState
dispatch (Pushbasic  n)    = pushbasic n
dispatch (Pushglobal f)    = pushglobal f
dispatch (Pushint    n)    = pushint n
dispatch (Push       n)    = push n
dispatch (Update     n)    = update n
dispatch (Pop        n)    = pop n
dispatch (Slide      n)    = slide n
dispatch (Alloc      n)    = alloc n
dispatch (Split      n)    = split n
dispatch Add               = binaryArithmetric (+)
dispatch Sub               = binaryArithmetric (-)
dispatch Mul               = binaryArithmetric (*)
dispatch Div               = binaryArithmetric div
dispatch Neg               = unaryArithemetric negate
dispatch Eq                = comparison (==)
dispatch Ne                = comparison (/=)
dispatch Lt                = comparison (<)
dispatch Le                = comparison (<=)
dispatch Gt                = comparison (>)
dispatch Ge                = comparison (>=)
dispatch (Cond i1 i2     ) = cond i1 i2
dispatch (Casejump alters) = casejump alters
dispatch (Pack tag arity ) = pack tag arity
dispatch Mkint             = mkint
dispatch Mkbool            = mkbool
dispatch Get               = get
dispatch Mkap              = mkap
dispatch Eval              = evalope
dispatch Unwind            = unwind
dispatch Print             = gmprint

gmprint :: GMState -> GMState
gmprint state@(o, i, a : s, v, dump, heap, global, stats) = newState
  (hLookup heap a)
  state
 where
  newState (NConst t as) =
    putOutput ("Pack{" ++ show t ++ "," ++ show (length as) ++ "}")
      . -- KH
        putCode (printcode (length as) ++ getCode state)
      . putStack (as ++ s)
  newState (NNum n) = putOutput (show n) . putStack s
  newState n        = error "Print of non data structure"

unwind :: GMState -> GMState
unwind state@(o, i, a : ss, v, dump, heap, global, stats) =
  newState (hLookup heap a) state

newState :: Node -> GMState -> GMState
newState (NNum n) state@(o, i, s1 : ss, v, dump, heap, global, stats) =
  updateFromDump s1 dump state
newState (NAp e1 e2) state@(o, i, s, v, dump, heap, global, stats) =
  (o, [Unwind], e1 : s, v, dump, heap, global, stats)
newState (NInd a) state@(o, i, _ : ss, v, dump, heap, global, stats) =
  (o, [Unwind], a : ss, v, dump, heap, global, stats)
newState (NConst tag as) state@(o, i, s1 : ss, v, dump, heap, globals, stats) =
  updateFromDump s1 dump state
newState (NGlobal n c) state@(o, i, s1 : ss, v, dump, heap, global, stats)
  | length ss >= n = (o, c, rs, v, dump, heap, global, stats)
  | otherwise      = (o, i1, last (s1 : ss) : s2, v, d1, heap, global, stats)
 where
  ((i1, s2) : d1) = dump
  rs              = rearrange n heap (s1 : ss)

updateFromDump :: Addr -> GMDump -> GMState -> GMState
updateFromDump a dump state@(o, _, _, v, _, heap, global, stats) = case dump of
  []             -> state
  ((i1, s1) : d) -> (o, i1, a : s1, v, d, heap, global, stats)

pack :: Int -> Int -> GMState -> GMState
pack tag arity (o, i, s, v, dump, heap, global, stats) =
  (o, i, a : drop arity s, v, dump, heap1, global, stats)
  where (heap1, a) = hAlloc heap (NConst tag $ take arity s)

evalope :: GMState -> GMState
evalope (o, i, s1 : ss, v, dump, heap, global, stats) =
  (o, [Unwind], [s1], v, (i, ss) : dump, heap, global, stats)

casejump :: [(Int, GMCode)] -> GMState -> GMState
casejump alters (o, i, s1 : ss, v, dump, heap, global, stats) =
  (o, i1 ++ i, s1 : ss, v, dump, heap, global, stats)
 where
  (NConst t as) = hLookup heap s1
  i1 = aLookup alters t (error "Pattern match(es) are non-exhaustive")

split :: Int -> GMState -> GMState
split n (o, i, s1 : ss, v, dump, heap, global, stats) =
  (o, i, as ++ ss, v, dump, heap, global, stats)
  where (NConst tag as) = hLookup heap s1

alloc :: Int -> GMState -> GMState
alloc n (o, i, s, v, dump, heap, global, stats) =
  (o, i, as ++ s, v, dump, heap1, global, stats)
  where (heap1, as) = allocNodes n heap

slide :: Int -> GMState -> GMState
slide n (o, i, s1 : ss, v, dump, heap, global, stats) =
  (o, i, s1 : drop n ss, v, dump, heap, global, stats)

pop :: Int -> GMState -> GMState
pop n (o, i, s, v, dump, heap, global, stats) =
  (o, i, drop n s, v, dump, heap, global, stats)

update :: Int -> GMState -> GMState
update n (o, i, s1 : ss, v, dump, heap, global, stats) =
  (o, i, ss, v, dump, heap1, global, stats)
  where heap1 = hUpdate heap (ss !! n) (NInd s1)

push :: Int -> GMState -> GMState
push n (o, i, s, v, dump, heap, global, stats) =
  (o, i, s !! n : s, v, dump, heap, global, stats)

mkap :: GMState -> GMState
mkap (o, i, s1 : s2 : ss, v, dump, heap, global, stats) =
  (o, i, a : ss, v, dump, heap1, global, stats)
  where (heap1, a) = hAlloc heap (NAp s1 s2)

pushint :: Int -> GMState -> GMState
pushint n (o, i, s, v, dump, heap, global, stats) =
  (o, i, a : s, v, dump, heap1, global, stats)
  where (heap1, a) = hAlloc heap (NNum n)

pushglobal :: String -> GMState -> GMState
pushglobal name (o, i, s, v, dump, heap, global, stats) =
  (o, i, a : s, v, dump, heap, global, stats)
  where a = aLookup global name (error $ name ++ " doesn't exist")

get :: GMState -> GMState
get (o, i, s1 : ss, v, dump, heap, global, stats) =
  (o, i, ss, n1 : v, dump, heap, global, stats)
 where
  n1 = case hLookup heap s1 of
    (NNum n    ) -> n
    (NConst t _) -> t

mkbool :: GMState -> GMState
mkbool (o, i, s, t : vs, dump, heap, global, stats) =
  (o, i, a : s, vs, dump, heap1, global, stats)
  where (heap1, a) = hAlloc heap (NConst t [])

mkint :: GMState -> GMState
mkint (o, i, s, n : vs, dump, heap, global, stats) =
  (o, i, a : s, vs, dump, heap1, global, stats)
  where (heap1, a) = hAlloc heap (NNum n)

cond :: GMCode -> GMCode -> GMState -> GMState
cond i1 i2 (o, i, s, v1 : vs, dump, heap, global, stats) =
  (o, i3 ++ i, s, vs, dump, heap, global, stats)
 where
  i3 | v1 == cTrue  = i1
     | v1 == cFalse = i2
     | otherwise    = error "cannot happen"

pushbasic :: Int -> GMState -> GMState
pushbasic n (o, i, s, v, dump, heap, global, stats) =
  (o, i, s, n : v, dump, heap, global, stats)

-- Helper
binaryArithmetric :: (Int -> Int -> Int) -> GMState -> GMState
binaryArithmetric f (o, i, s, v1 : v2 : vs, dump, heap, global, stats) =
  (o, i, s, f v1 v2 : vs, dump, heap, global, stats)

unaryArithemetric :: (Int -> Int) -> GMState -> GMState
unaryArithemetric f (o, i, s, v1 : vs, dump, heap, global, stats) =
  (o, i, s, f v1 : vs, dump, heap, global, stats)

comparison :: (Int -> Int -> Bool) -> GMState -> GMState
comparison f (o, i, s, v1 : v2 : vs, dump, heap, global, stats) =
  (o, i, s, cond : vs, dump, heap, global, stats)
 where
  cond | f v1 v2   = cTrue
       | otherwise = cFalse

printcode :: Int -> [Instruction]
printcode 0 = []
printcode n = Eval : Print : printcode (n - 1)

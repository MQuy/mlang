module G.Compile where

import           Data.List                      ( mapAccumL )
import           Types
import           G.State

compile :: Program -> GMState
compile program = ("", initialCode, [], [], [], heap, globals, statInitial)
  where (heap, globals) = buildInitialHeap program

buildInitialHeap :: Program -> (GMHeap, GMGlobals)
buildInitialHeap program = mapAccumL allocateSc hInitial compiled
  where compiled = map compileSc (preludeDefs ++ program ++ primitives)

allocateSc :: GMHeap -> GMCompiledSC -> (GMHeap, (String, Addr))
allocateSc heap (name, nargs, instructions) = (newHeap, (name, addr))
  where (newHeap, addr) = hAlloc heap (NGlobal nargs instructions)

compileSc :: (String, [String], Expr) -> GMCompiledSC
compileSc (name, env, body) = (name, d, compileR d body (zip env [0 ..]))
  where d = length env

compileR :: Int -> GMCompiler
compileR d (ELet recursive defs e) env
  | recursive = compileLetrec (compileR (d + length defs)) defs e env
  | otherwise = compileLet (compileR (d + length defs)) defs e env
compileR d (EAp (EAp (EAp (EVar "if") e1) e2) e3) env =
  compileB e1 env ++ [Cond (compileR d e2 env) (compileR d e3 env)]
compileR d (ECase e alts) env =
  compileE e env ++ [Casejump (compileAlts (compileAR d) alts env)]
compileR d e env = compileE e env ++ [Update d, Pop d, Unwind]

compileE :: GMCompiler
compileE (ENum n) env = [Pushint (fromIntegral n)]
compileE (ELet recursive defs e) env
  | recursive = compileLetrec compileE defs e env ++ [Slide (length defs)]
  | otherwise = compileLet compileE defs e env ++ [Slide (length defs)]
compileE (ECase e alters) env =
  compileE e env ++ [Casejump (compileAlts compileAE alters env)]
compileE expr@(EAp (EAp (EVar op) e1) e2) env | op `elem` binaryOps =
  compileB expr env ++ [inst]
 where
  binaryOps = map fst builtInDyadic
  inst      = aLookup binaryOpBox op (error "This can't happen")
compileE (EAp (EVar "negate") e) env = compileB e env ++ [Mkint]
compileE (EAp (EAp (EAp (EVar "if") e1) e2) e3) env =
  compileB e1 env ++ [Cond (compileE e2 env) (compileE e3 env)]
compileE e env = compileC e env ++ [Eval]

compileB :: GMCompiler
compileB (ENum i) _ = [Pushbasic (fromIntegral i)]
compileB (ELet recursive defs e) env
  | recursive = compileLetrec compileB defs e env ++ [Pop (length defs)]
  | otherwise = compileLet compileB defs e env ++ [Pop (length defs)]
compileB (EAp (EAp (EVar op) e1) e2) env | op `elem` binaryOps =
  compileB e2 env ++ compileB e1 env ++ [inst]
 where
  binaryOps = map fst builtInDyadic
  inst      = aLookup builtInDyadic op (error "This can't happen")
compileB (EAp (EVar "negate") e) env = compileB e env ++ [Neg]
compileB (EAp (EAp (EAp (EVar "if") predicate) e1) e2) env =
  compileB predicate env ++ [Cond (compileB e1 env) (compileB e2 env)]
compileB e env = compileE e env ++ [Get]

compileC :: GMCompiler
compileC (EConst t 0) _ = [Pack (fromIntegral t) 0]
compileC (EConst t a) _ = [Pushglobal $ "Pack " ++ show t ++ " " ++ show a]
compileC (EVar v) env | v `elem` aDomain env = [Push n]
                      | otherwise            = [Pushglobal v]
  where n = aLookup env v (error "Can't happen")
compileC (ENum n) env = [Pushint (fromIntegral n)]
compileC (EAp e1 e2) env =
  compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [Mkap]
compileC (ELet recursive defs e) env
  | recursive = compileLetrec compileC defs e env ++ [Slide (length defs)]
  | otherwise = compileLet compileC defs e env ++ [Slide (length defs)]
compileC (ECase e alters) env =
  compileE e env ++ [Casejump (compileAlts compileAE alters env)]

compileLet :: GMCompiler -> [(String, Expr)] -> GMCompiler
compileLet comp defs expr env = compiled defs env ++ comp expr env1
 where
  env1 = compileArgs defs env
  compiled [] env = []
  compiled ((name, expr) : defs) env =
    compileC expr env ++ compiled defs (argOffset 1 env)

compileArgs :: [(String, Expr)] -> GMEnvironment -> GMEnvironment
compileArgs defs env =
  zip (map fst defs) [n - 1, n - 2 .. 0] ++ argOffset n env
  where n = length defs

compileLetrec comp defs e env =
  [Alloc n] ++ compiled defs (n - 1) ++ comp e env1
 where
  env1 = compileArgs defs env
  n    = length defs
  compiled [] i = []
  compiled ((name, expr) : ds) i =
    compileC expr env1 ++ [Update i] ++ compiled ds (i - 1)

argOffset :: Int -> GMEnvironment -> GMEnvironment
argOffset n env = [ (v, n + m) | (v, m) <- env ]

compileAlts
  :: (Int -> GMCompiler)     -- compiler for alternative bodies
  -> [Alter]            -- the list of alternatives
  -> GMEnvironment        -- the current environment
  -> [(Int, GMCode)]      -- list of alternative code sequences
compileAlts comp alts env =
  [ ( fromIntegral tag
    , comp (length names)
           body
           (zip names [0 ..] ++ argOffset (length names) env)
    )
  | (tag, names, body) <- alts
  ]

compileAE :: Int -> GMCompiler
compileAE offset expr env =
  [Split offset] ++ compileE expr env ++ [Slide offset]

compileAR :: Int -> Int -> GMCompiler
compileAR d offset expr env =
  [Split offset] ++ compileR (d + offset) expr env ++ [Slide offset]


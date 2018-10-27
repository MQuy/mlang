module GCompile where

import           Type
import           Data.List                      ( mapAccumL )
import           Data.Char

type GMCompiledSC = (String, Int, GMCode)
type GMEnvironment = [(String, Addr)]
type GMCompiler = Expr -> GMEnvironment -> GMCode

compile :: Program -> GMState
compile program = ("", codeInitial, [], [], [], heap, global, statsInitial)
  where (heap, global) = buildInitialHeap program

buildInitialHeap :: Program -> (GMHeap, GMGlobal)
buildInitialHeap program = mapAccumL allocateSc hInitial compiled
 where
  compiled =
    map compileSc (preludeDefs ++ primitives)
      ++ map compileDc (preludeDcs ++ dataDeclarations)
      ++ map compileSc supercombinators
  dataDeclarations = filter (\(name, _, _) -> all isDigit name) program
  supercombinators = filter (\(name, _, _) -> not (all isDigit name)) program

allocateSc :: GMHeap -> GMCompiledSC -> (GMHeap, (String, Addr))
allocateSc heap (name, nargs, instructions) = (newHeap, (name, addr))
  where (newHeap, addr) = hAlloc heap (NGlobal nargs instructions)

compileSc :: ScDefn -> GMCompiledSC
compileSc (name, env, body) = (name, d, compileR d body (zip env [0 ..]))
  where d = length env

compileR :: Int -> GMCompiler
compileR d (ELet recursive defs e) env
  | recursive = compileLetrec (compileR (d + length defs)) defs e env
  | otherwise = compileLet (compileR (d + length defs)) defs e env
compileR d (EAp (EAp (EAp (EVar "if") e1) e2) e3) env =
  compileB e1 env ++ [Cond (compileR d e2 env) (compileR d e3 env)]
compileR d (ECase e alters) env =
  compileE e env
    ++ [ Casejump
           [ (tag, compileAlter (compileR (d + length names)) names body env)
           | (tag, names, body) <- alters
           ]
       ]
compileR d e env = compileE e env ++ [Update d, Pop d, Unwind]

compileE :: GMCompiler
compileE (ENum n) env = [Pushint n]
compileE e@(EAp (EAp (EVar op) e1) e2) env | op `elem` binaryOps =
  compileB e env ++ [inst]
 where
  binaryOps = map fst builtInDyadic
  inst      = aLookup binaryOpBox op (error "This can't happen")
compileE e@(EAp (EVar "negate") e1) env = compileB e1 env ++ [Mkint]
compileE (EAp (EAp (EAp (EVar "if") e1) e2) e3) env =
  compileB e1 env ++ [Cond (compileE e2 env) (compileE e3 env)]
compileE (ELet recursive defs e) env
  | recursive = compileLetrec compileE defs e env ++ [Slide (length defs)]
  | otherwise = compileLet compileE defs e env ++ [Slide (length defs)]
compileE (ECase e alters) env =
  compileE e env
    ++ [ Casejump
           [ (tag, compileAlter compileE names body env)
           | (tag, names, body) <- alters
           ]
       ]
compileE e env = compileC e env ++ [Eval]

compileB :: GMCompiler
compileB (ENum n) env = [Pushbasic n]
compileB (EAp (EAp (EVar op) e1) e2) env | op `elem` binaryOps =
  compileB e2 env ++ compileB e1 env ++ [inst]
 where
  inst      = aLookup builtInDyadic op (error "This can't happen")
  binaryOps = map fst builtInDyadic
compileB (EAp (EVar "negate") e) env = compileB e env ++ [Neg]
compileB (EAp (EAp (EAp (EVar "if") e1) e2) e3) env =
  compileB e1 env ++ [Cond (compileB e2 env) (compileE e3 env)]
compileB (ELet recursive defs e) env
  | recursive = compileLetrec compileB defs e env ++ [Pop (length defs)]
  | otherwise = compileLet compileB defs e env ++ [Pop (length defs)]
compileB e env = compileE e env ++ [Get]

compileC :: GMCompiler
compileC (ENum n) env = [Pushint n]
compileC (EVar v) env | v `elem` aDomain env = [Push n]
                      | otherwise            = [Pushglobal v]
  where n = aLookup env v (error "Can't happen")
compileC (EConst t a) env = [Pushglobal $ show t]
compileC (EAp e1 e2) env =
  compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [Mkap]
compileC (ELet recursive defs e) env
  | recursive = compileLetrec compileC defs e env ++ [Slide (length defs)]
  | otherwise = compileLet compileC defs e env ++ [Slide (length defs)]
compileC (ECase e alters) env =
  compileC e env
    ++ [ Casejump
           [ (tag, compileAlter compileC names body env)
           | (tag, names, body) <- alters
           ]
       ]

compileAlter :: GMCompiler -> [String] -> Expr -> GMEnvironment -> GMCode
compileAlter compile names expr env =
  [Split n] ++ compile expr env ++ [Slide n]
 where
  n    = length names
  env1 = zip names [0 ..] ++ argOffset n env

compileLet :: GMCompiler -> [EDef] -> GMCompiler
compileLet compile defs e env = compiled defs env ++ compileC e env2
 where
  env2 = compileArgs defs env
  compiled [] env1 = []
  compiled ((name, e1) : ds) env1 =
    compileC e1 env1 ++ compiled ds (argOffset 1 env1)

compileLetrec :: GMCompiler -> [EDef] -> GMCompiler
compileLetrec compile defs e env =
  [Alloc n] ++ compiled defs (n - 1) ++ compileC e env1
 where
  n    = length defs
  env1 = compileArgs defs env
  compiled [] _ = []
  compiled ((name, e1) : ds) i =
    compileC e1 env1 ++ [Update i] ++ compiled ds (i - 1)

argOffset :: Int -> GMEnvironment -> GMEnvironment
argOffset n env = [ (name, addr + n) | (name, addr) <- env ]

compileArgs :: [EDef] -> GMEnvironment -> GMEnvironment
compileArgs defs env =
  zip (map fst defs) [n - 1, n - 2 .. 0] ++ argOffset n env
  where n = length defs

-- Special compiler foor data constructor
compileDc :: ScDefn -> GMCompiledSC
compileDc (name, env, body) =
  (name, d, compileD body (zip env [0 ..]) ++ [Update d, Pop d, Unwind])
  where d = length env

compileD :: GMCompiler
compileD (ENum n) env = [Pushint n]
compileD (EVar v) env | v `elem` aDomain env = [Push n]
                      | otherwise            = [Pushglobal v]
  where n = aLookup env v (error "Can't happen")
compileD (EConst t a) env = [Pack t a]
compileD (EAp e1 e2) env =
  compileD e2 env ++ compileD e1 (argOffset 1 env) ++ [Mkap]


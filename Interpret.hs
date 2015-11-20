module Interpret(
run
)where

import Grammar
import Utils
import Runtime
import Value
import Definition
import Type
import Storage
import Parse
import Control.Applicative
import Control.Monad hiding (void)
import qualified Data.Map.Strict as M
import Data.List
import Data.Maybe

import Control.Exception.Base

type SymbolTable = M.Map Identifier Id

data State = State Memory (Stack SymbolTable) deriving Show

declareVar :: Program -> State -> VariableDeclaration -> IO State
declareVar p s (VarDecl type' name init') = do
    (v, newS) <-
        if isJust init'
        then initialize p s $ fromJust init'
        else return (defaultVal type', s)
    return $ newVar newS name v

declareArr :: Program -> State -> VariableDeclaration -> IO State
declareArr p s@(State mem _) (ArrDecl type' name size init') = do
    (RVal _ size', State newMem table) <- computeExp p s size
    let sizeVal = read size' :: Int
    let (newNewMem, ptr) = blockAllocate newMem (defaultVal type') sizeVal
    let (Type typeName ptrLv) = type'
    let newState = newVar (State newNewMem table) name (RVal (Type typeName (ptrLv + 1)) (show ptr))
    -- TODO: initialization
    return newState

pushScope :: State -> State
pushScope (State m s) = State m $ push (M.empty) s

popScope :: State -> State
popScope (State m s) = State (M.foldl (\m i -> free m i) m $ top s) $ pop s

toRVal :: State -> Value -> Value
toRVal s@(State m _) (LVal i) = toRVal s $ readMem m i
toRVal _ x = x

isLVal (LVal _) = True
isLVal _ = False

isRVal = not . isLVal

-- Return left value
getVar :: State -> Identifier -> Value
getVar (State m s) x = LVal $ work s x where
    work s' x' =
        if x' `M.member` top s'
        then (top s') M.! x'
        else work (pop s') x'

-- Follow left value link
follow :: Memory -> Id -> Id
follow m t = case readMem m t of
    LVal k -> follow m k
    _ -> t

-- If identifier is a left value, follow the link
modifyVar :: State -> Identifier -> (Value -> Value) -> State
modifyVar state@(State m s) x f = State (modify m i f) s where
    i = let LVal k = getVar state x in follow m k

-- Return right value
getVal :: State -> Identifier -> Value
getVal s = toRVal s . getVar s

newVar :: State -> Identifier -> Value -> State
newVar state@(State m s) n v =
    let (newM, i) = allocate m v
    in State newM $ updateTop (M.insert n i) s

data StructureRt = Breaked | Normal | Continued | Returned Value deriving Eq

rt2Value :: StructureRt -> Value
rt2Value (Returned v) = v
rt2Value _ = void


-- Run a program
-- Input: Program, Entry function, Args
-- run :: Program -> Identifier -> [Value] -> IO Value
-- run p@(Program _ _ vars) entry args = fst <$> computeFunc p initialState (getFunc p entry) args where
run p@(Program fs ss vars) entry args = computeFunc newP initialState (getFunc newP entry) args where
    newP = Program (fs ++ compileDarkMagicBook darkMagicBook ++ magicBook) ss vars
    initialState = State newMem empty


getFunc :: Program -> Identifier -> FunctionDefinition
getFunc (Program fs _ _) n = fromJust $ find (\(FuncDef x _ _ _) -> x == n) fs

computeFunc :: Program -> State -> FunctionDefinition -> [Value] -> IO (Value, State)
computeFunc p s (FuncDef name rtType paras ins) args = snd' popScope <$> fst' rt2Value <$> runStructure p state ins where
    state = foldl (uncurry . newVar) (pushScope s) $ zipWith (\(_, n) v -> (n, v)) paras $ lrVal args
    lrVal =
        if isDarkMagic name
        then id
        else map (toRVal s)
    -- Really smelly hack, but I doubt if I can bypass it without thorough modifications

computeArgs :: Program -> State -> [Exp] -> IO ([Value], State)
computeArgs _ s [] = return ([], s)
computeArgs p s (x:xs) = do
    (v, newS) <- computeExp p s x
    fst' (v:) <$> computeArgs p newS xs

-- Return Left Value if Possible
computeExp :: Program -> State -> Exp -> IO (Value, State)
computeExp p s (Constant x)
    | x `elem` ["true", "false"] = return (RVal boolType x, s)
    | isValidIdentifier x = return (getVar s x, s)
    | otherwise = return (RVal Polymorphism x, s)

computeExp p s (Exp "?" args) = undefined -- lazy trinary
computeExp p s (Exp f args) = do
    (para, newS) <- computeArgs p s args
    computeFunc p newS (getFunc p f) para

runStructure :: Program -> State -> [Structure] -> IO (StructureRt, State)
runStructure _ s [] = return (Normal, s)
runStructure p _s (t:ts) = handler _s t where
    handler :: State -> Structure -> IO (StructureRt, State)
    handler = case t of
        IfBlock _ _ _ -> runIfBlock
        WhileBlock _ _ -> runWhileBlock
        ForBlock _ _ _ _ -> runForBlock
        Expression _ -> runExpression
        Declaration _ -> runDeclaration
        UCJump _ -> runUCJump
        DarkMagic _ -> runDarkMagic
        where
            runIfBlock s (IfBlock con pri alt) = do
                (conVal, newS) <- computeExp p (pushScope s) con
                (rt, newS) <-
                    if isTrue $ toRVal newS conVal
                    then snd' popScope <$> runStructure p newS pri
                    else runStructure p newS alt
                if rt `elem` [Normal, Breaked]
                then snd' popScope <$> runStructure p newS ts
                else return (rt, popScope newS)

            runWhileBlock s (WhileBlock con body) = snd' popScope <$> (loop $ pushScope s) where
                loop :: State -> IO (StructureRt, State)
                loop state = do
                    (conVal, newS) <- computeExp p state con
                    if isTrue conVal
                    then do
                        (rt, newS) <- runStructure p newS body
                        case rt of
                            Normal -> loop newS
                            Breaked -> runStructure p newS ts
                            k@(Returned _) -> return (k, newS)
                    else runStructure p newS ts

            runForBlock s (ForBlock init' con delta body) = do
                (init', newS) <- computeExp p s init'
                runWhileBlock newS $ WhileBlock con (body ++ [Expression delta])

            runExpression s (Expression exp) = do
                (_, newS) <- computeExp p s exp
                runStructure p newS ts

            runDeclaration s (Declaration decl) = do
                newS <- (case decl of
                    (VarDecl _ _ _) -> declareVar
                    (ArrDecl _ _ _ _) -> declareArr) p s decl
                runStructure p newS ts

            runUCJump s (UCJump Break) = return (Breaked, s)
            runUCJump s (UCJump Continue) = return (Continued, s)
            runUCJump s (UCJump (Return exp)) = do
                (v, newS) <- computeExp p s exp
                return (Returned $ toRVal newS v, newS)

            runDarkMagic s (DarkMagic curse) = fst' Returned <$> getDarkMagic curse s

initialize :: Program -> State -> VarInit -> IO (Value, State)
initialize p s (InitExp exp) = computeExp p s exp

type Magic = State -> IO (Value, State)

uselessMagic s = return (void, s)

getDarkMagic :: Identifier -> Magic
getDarkMagic = (M.!) _darkMagic

_darkMagic = M.fromList $ map (\(n, it ,im) -> (n, im)) darkMagicBook

isDarkMagic s = (not $ isValidIdentifier s) || s `M.member` _darkMagic

-- (Name, Interface, Implement)
darkMagicBook :: [(Identifier, FunctionDefinition, Magic)]
darkMagicBook = [("/", divide, divideMagic), ("+", add, addMagic), ("-neg", neg, negMagic), ("=", assign, assignMagic), ("debuginfo", debugInfo, debugInfoMagic), ("debugprint", debugPrint, debugPrintMagic), ("&addr", addr, addrMagic), ("*deref", deref, derefMagic), ("print", print_, printMagic), ("input", input, inputMagic), ("*", multi, multiMagic), ("==", eq, eqMagic), ("<", less, lessMagic), ("!", logNeg, logNegMagic), ("&&", logAnd, logAndMagic), ("||", logOr, logOrMagic), ("-=", minusAssign, uselessMagic), ("+=", plusAssign, uselessMagic), ("*=", multiAssign, uselessMagic), ("/=", divideAssign, uselessMagic), ("--suf", suffixDec, uselessMagic), ("++suf", suffixInc, uselessMagic), ("--pre", prefixDec, uselessMagic), ("++pre", prefixInc, uselessMagic)]

compileDarkMagicBook = map (\(n, it, im) -> it)

add = FuncDef "+" Polymorphism [(Polymorphism, "a"), (Polymorphism, "b")] [DarkMagic "+"]
addMagic s =
    let RVal _ a = getVal s "a"; RVal _ b = getVal s "b"
    in return (RVal intType $ show (read a + read b :: Integer), s)

divide = FuncDef "/" Polymorphism [(Polymorphism, "a"), (Polymorphism, "b")] [DarkMagic "/"]
divideMagic s =
    let RVal _ a = getVal s "a"; RVal _ b = getVal s "b"
    in return (RVal intType $ show (read a `div` read b :: Integer), s)
-- TODO: support float

plusAssign = FuncDef "+=" Polymorphism [(Polymorphism, "a"), (Polymorphism, "b")] $ parseFuncBody "a=a+b;return a;"
minusAssign = FuncDef "-=" Polymorphism [(Polymorphism, "a"), (Polymorphism, "b")] $ parseFuncBody "a=a-b;return a;"
multiAssign = FuncDef "*=" Polymorphism [(Polymorphism, "a"), (Polymorphism, "b")] $ parseFuncBody "a=a*b;return a;"
divideAssign = FuncDef "/=" Polymorphism [(Polymorphism, "a"), (Polymorphism, "b")] $ parseFuncBody "a=a/b;return a;"

prefixInc = FuncDef "++pre" Polymorphism [(Polymorphism, "a")] $ parseFuncBody "a+=1;return a;"
prefixDec = FuncDef "--pre" Polymorphism [(Polymorphism, "a")] $ parseFuncBody "a-=1;return a;"
suffixInc = FuncDef "++suf" Polymorphism [(Polymorphism, "a")] $ parseFuncBody "a+=1;return a-1;"
suffixDec = FuncDef "--suf" Polymorphism [(Polymorphism, "a")] $ parseFuncBody "a-=1;return a+1;"

multi = FuncDef "*" Polymorphism [(Polymorphism, "a"), (Polymorphism, "b")] [DarkMagic "*"]
multiMagic s =
    let RVal _ a = getVal s "a"; RVal _ b = getVal s "b"
    in return (RVal intType $ show (read a * read b :: Integer), s)

eq = FuncDef "==" Polymorphism [(Polymorphism, "a"), (Polymorphism, "b")] [DarkMagic "=="]
eqMagic s =
    let RVal _ a = getVal s "a"; RVal _ b = getVal s "b"
    in return (if a == b then true else false, s)

less = FuncDef "<" Polymorphism [(Polymorphism, "a"), (Polymorphism, "b")] [DarkMagic "<"]
lessMagic s =
    let RVal _ a = getVal s "a"; RVal _ b = getVal s "b"
    in return (if (read a :: Integer) < (read b :: Integer) then true else false, s)

neg = FuncDef "-neg" Polymorphism [(Polymorphism, "a")] [DarkMagic "-neg"]
negMagic s =
    let RVal _ a = getVal s "a"
    in return (RVal intType $ show (- read a :: Integer), s)

logNeg = FuncDef "!" Polymorphism [(Polymorphism, "a")] [DarkMagic "!"]
logNegMagic s =
    let b = toBool $ getVal s "a"
    in return (if b == true then false else true, s)

logAnd = FuncDef "&&" Polymorphism [(Polymorphism, "a"), (Polymorphism, "b")] [DarkMagic "&&"]
logAndMagic s =
    let a = toBool $ getVal s "a"; b = toBool $ getVal s "b"
    in return (if a == true && b == true then true else false, s)

logOr = FuncDef "||" Polymorphism [(Polymorphism, "a"), (Polymorphism, "b")] [DarkMagic "||"]
logOrMagic s =
    let a = toBool $ getVal s "a"; b = toBool $ getVal s "b"
    in return (if a == true || b == true then true else false, s)

assign = FuncDef "=" Polymorphism [(Polymorphism, "a"), (Polymorphism, "b")] [DarkMagic "="]
assignMagic s =
    let var = getVar s "a"; val@(RVal _ _) = getVal s "b"
    in return (var, modifyVar s "a" (return val))

debugInfo = FuncDef "debuginfo" voidType [] [DarkMagic "debuginfo"]
debugInfoMagic s = (print $ popScope s) >> return (void, s)

debugPrint = FuncDef "debugprint" voidType [(Polymorphism, "a")] [DarkMagic "debugprint"]
debugPrintMagic s@(State m t) = (print $ getVar s "a") >> return (void, s)

addr = FuncDef "&addr" Polymorphism [(Polymorphism, "a")] [DarkMagic "&addr"]
addrMagic s@(State m _) =
    let LVal i = getVar s "a"
    in return (RVal (makePtr "void" 1) $ show $ follow m i, s)

deref = FuncDef "*deref" Polymorphism [(Polymorphism, "a")] [DarkMagic "*deref"]
derefMagic s@(State m _) = let RVal _ a = getVal s "a" in return (LVal $ follow m (read a :: Id), s)

print_ = FuncDef "print" voidType [(Polymorphism, "a")] [DarkMagic "print"]
printMagic s = let RVal _ x = getVal s "a" in putStrLn x >> return (void, s)

input = FuncDef "input" Polymorphism [] [DarkMagic "input"]
inputMagic s = do
    x <- getString
    return (RVal Polymorphism x, s)

magicBook = writeMagicBook theStandardBookOfSpell

writeMagicBook :: [(Identifier, String)] -> [FunctionDefinition]
writeMagicBook spells = (\(name, s) -> let Program [f] _ _ = parse s in let FuncDef _ rt args ins = f in FuncDef name rt args ins) <$> spells

theStandardBookOfSpell = [("-", "int f(int a,int b){return a+(-b);}"), ("!=", "int f(int a,int b){return !(a==b);}"), ("<=", "int f(int a,int b){return (a<b)||(a==b);}"), (">", "int f(int a,int b){return !(a<=b);}"), (">=", "int f(int a,int b){return (a>b)||(a==b);}"), (",", "int f(int a,int b){return b;}")]

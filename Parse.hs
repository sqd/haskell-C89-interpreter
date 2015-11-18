module Parse(
parse,
parseFuncBody
)where

import Utils
import Grammar
import Type
import Data.List
import Data.Char
import Data.List.Split
import Control.Applicative

-- Find a bracket (in a wide term)
-- Input: left bracket, right bracket, a list starts with the left bracket
-- Output: (Content_in_Bracket_with_the_Brackets, Rest)
-- eg. spanBracket "(" ")" "(abc)def" = ("(abc)", "def")
spanBracket :: Eq a => a -> a -> [a] -> ([a], [a])
spanBracket l r s = fst' (l:) $ work 1 $ tail s where
    work 0 x = ([], x)
    work i (x:xs) = fst' (x:) $ work (i + if x == l then 1 else if x == r then -1 else 0) xs

-- Similiar to `splitOn`, but do not split inside a bracket
-- eg. splitTopLevelOn "," $ split2w "abc(,),def" = [split2w "abc(,)", split2w "def"]
-- Input: left bracket, right bracket, splitter
splitTopLevelOn :: Eq a => a -> a -> a -> [a] -> [[a]]
splitTopLevelOn _ _ _ [] = [[]]
splitTopLevelOn l r splitter s@(k:ks)
    | k == l = (bracket ++ x):xs
    | k == splitter = []:(splitTopLevelOn l r splitter ks)
    | otherwise = let (t:ts) = splitTopLevelOn l r splitter ks in (k:t):ts
        where
            (bracket, rest) = spanBracket l r s
            (x:xs) = splitTopLevelOn l r splitter rest

-- Delete all brackets in the top level
dropTopLevelBrackets :: Eq a => a -> a -> [a] -> [a]
dropTopLevelBrackets _ _ [] = []
dropTopLevelBrackets left right s@(x:xs) =
    if x == left
    then snd $ spanBracket left right s
    else x:(dropTopLevelBrackets left right xs)

-- Is a string in the top level?
elemTopLevel :: String -> [String] -> Bool
elemTopLevel x l = x `elem` dropTopLevelBrackets "(" ")" l

-- Split only once, searching from the left
-- eg. split ';' "abc;def;ghi" = ("abc", "def;ghi")
splitOnceL :: Eq a => a -> [a] -> ([a], [a])
splitOnceL _ [] = ([], [])
splitOnceL t (x:xs) = if t == x then ([], xs) else fst' (x:) $ splitOnceL t xs

-- Split only once, searching from the right
splitOnceR :: Eq a => a -> [a] -> ([a], [a])
splitOnceR t l = let (x, y) = fst' reverse $ snd' reverse $ splitOnceL t $ reverse l in (y, x)

decomment :: String -> String
decomment "" = ""
decomment str@(x:xs)
    | "//" `isPrefixOf` str = decomment $ dropWhile (/='\n') str
    | "/*" `isPrefixOf` str = decomment $ drop 2 $ head $ filter (isPrefixOf "*/") $ tails str
    | otherwise = x:(decomment xs)

-- Get a string, ignoring escaped quatation mark
-- Input: a string STARTS WITH A STRING
-- Output: (the_String_with_Quotation_Mark, rest)
spanString :: String -> (String, String)
spanString = fst' ('"':) . spanString' . tail where
    spanString' ('\\':c:xs) = fst' (++ ['\\', c]) $ spanString' xs
    spanString' ('"':xs) = ("\"", xs)
    spanString' (x:xs) = fst' (x:) $ spanString' xs

-- Split a string into a list of words
split2w :: String -> [String]
split2w "" = []
split2w str@(x:xs)
    | x `elem` "?!%^&*()-=+[]{};:,<.>/~" = [x]:(split2w xs)
    | isAlpha x || x == '_' = let (w, rest) = span (\c -> isAlphaNum c || c == '_') xs in (x:w):(split2w rest)
    | isNumber x = let (w, rest) = span isNumber str in w:(split2w rest)
    | x == '"' = let (w, rest) = spanString str in w:(split2w rest)
    | x == '\'' = (take 3 str):(split2w $ drop 3 str)
    | isSpace x || x == '\0' = split2w xs

-- Merge 2-char operators
mergeOps :: [String] -> [String]
mergeOps [] = []
mergeOps [x] = [x]
mergeOps (a:b:xs) =
    if a ++ b `elem` list
    then (a ++ b):(mergeOps xs)
    else a:(mergeOps (b:xs))
    where
        list = ["++", "--", "+=", "-=", "*=", "/=", "&&", "||", "==", "!=", ">=", "<=", ">>", "<<", "^=", "|=", "&=", "%=", "->"]
        -- >>= and <<=, shame you for being the only 3-char operators

-- Input: initialization list WITH BOTH BRACES
parseInitList :: [String] -> VarInit
parseInitList x = InitList $ map parseExp $ splitOn [","] $ extract x

-- Parse an expression
-- Input: an expression only
parseExp :: [String] -> Exp
parseExp [x] = Constant x
parseExp l
    | "," `elemTopLevel` l = binaryl ","
    | "*=" `elemTopLevel` l = binaryr "*="
    | "/=" `elemTopLevel` l = binaryr "/="
    | "%=" `elemTopLevel` l = binaryr "%="
    | "+=" `elemTopLevel` l = binaryr "+="
    | "-=" `elemTopLevel` l = binaryr "-="
    | "=" `elemTopLevel` l = binaryr "="
    | "?" `elemTopLevel` l = handleTernary
    | "||" `elemTopLevel` l = binaryl "||"
    | "&&" `elemTopLevel` l = binaryl "&&"
    | "|" `elemTopLevel` l = binaryl "|"
    | "^" `elemTopLevel` l = binaryl "^"
    | (not $ null l) && (head l == "&") = Exp "&addr" [parseExp $ tail l]
    | "&" `elemTopLevel` l = binaryl "&"
    | "==" `elemTopLevel` l = binaryl "=="
    | "!=" `elemTopLevel` l = binaryl "!="
    | ">" `elemTopLevel` l = binaryl ">"
    | ">=" `elemTopLevel` l = binaryl ">="
    | "<" `elemTopLevel` l = binaryl "<"
    | "<=" `elemTopLevel` l = binaryl "<="
    | ">>" `elemTopLevel` l = binaryl ">>"
    | "<<" `elemTopLevel` l = binaryl "<<"
    | (not $ null l) && (head l == "+") = Exp "+pos" [parseExp $ tail l]
    | "+" `elemTopLevel` l = binaryl "+"
    | (not $ null l) && (head l == "-") = Exp "-neg" [parseExp $ tail l]
    | "-" `elemTopLevel` l = binaryl "-"
    | (not $ null l) && (head l == "*") = Exp "*deref" [parseExp $ tail l]
    | "*" `elemTopLevel` l = binaryl "*"
    | "/" `elemTopLevel` l = binaryl "/"
    | "%" `elemTopLevel` l = binaryl "%"
    | (not $ null l) && (head l == "!") = Exp "!" [parseExp $ tail l]
    | (not $ null l) && (head l == "~") = Exp "~" [parseExp $ tail l]
    | (not $ null l) && (head l == "++") = Exp "++pre" [parseExp $ tail l]
    | (not $ null l) && (head l == "--") = Exp "--pre" [parseExp $ tail l]
    | "->" `elemTopLevel` l = binaryl "->"
    | "." `elemTopLevel` l = binaryl "."
    | "[" `elemTopLevel` l = parseExp handleSBrackets
    | (not $ null l) && (isValidIdentifier $ head l) && (l !! 1 == "(") = handleCall
    | (not $ null l) && last l == "++" = Exp "++suf" [parseExp $ init l]
    | (not $ null l) && last l == "--" = Exp "--suf" [parseExp $ init l]
    | (not $ null l) && head l == "(" && last l == ")" = parseExp $ extract l
    where
        binaryl x = let (left, right) = splitOnceL x l in Exp x [parseExp left, parseExp right]
        binaryr x = let (left, right) = splitOnceR x l in Exp x [parseExp left, parseExp right]

        handleTernary = Exp "?" [parseExp condition, parseExp pos, parseExp neg] where
            (condition, branches) = splitOnceR "?" l
            (pos, neg) = fst' extract $ spanBracket "?" ":" ("?":branches)

        handleCall = Exp (head l) $ map parseExp $ let t = splitTopLevelOn "(" ")" "," $ extract $ tail l in if t == [[]] then [] else t
        -- smelly, whatever

        handleSBrackets = front ++ ["(", "*", "(", "("] ++ [name] ++ [")", "+", "("] ++ offset ++ [")", ")", ")"] ++ back where
            (beforeLBracket, fromLBracket) = snd' ("[":) $ splitOnceL "[" l
            (offset, back) = fst' extract $ spanBracket "[" "]" fromLBracket
            front = init beforeLBracket
            name = last beforeLBracket



-- If the input is preceded by a variable declaration
-- Input: An input that may be preceded by a declaration
-- Type [Star*] Name [=,;]
isDeclaration :: [String] -> Bool
isDeclaration (x:xs)
    | x `elem` ["signed", "unsigned", "static"] = isDeclaration xs
isDeclaration ("long":x:xs)
    | x `elem` ["int", "double"] = isDeclaration (x:xs)
isDeclaration ("short":"int":xs) = isDeclaration ("int":xs)
isDeclaration x = isHeadIdentifier && hasName && hasValidFollowers where
    isHeadIdentifier = isValidIdentifier $ head x
    hasName = isValidIdentifier $ head afterStars
    hasValidFollowers = head afterName `elem` ["[", "=", ",", ";"]
    afterStars = dropWhile (== "*") $ tail x
    afterName = tail afterStars

isJump :: [String] -> Bool
isJump ("break":";":_) = True
isJump ("continue":";":_) = True
isJump ("return":_) = True
isJump _ = False

isTypeDef :: [String] -> Bool
isTypeDef ("typedef":"struct":_) = True
isTypeDef _ = False

-- Parse a type
-- Input: a type strings ONLY, such as ["int", "*"]
parseType :: [String] -> Type
parseType (s:t:ss)
    | s `elem` ["struct", "static", "signed", "unsigned"] = parseType (t:ss)
    | s `elem` ["long", "short"] && t == "int" = parseType (t:ss)
    | (s, t) == ("long", "double") = parseType (t:ss)
    | s `elem` ["long", "short"] = parseType ("int":t:ss)
parseType (typeName:stars) = Type typeName $ length stars

-- Extract the type name
-- Input: code starts with a type name
-- Output: (Type_Name, Rest)
spanTypeName :: [String] -> ([String], [String])
spanTypeName (s:t:ss)
    | s `elem` ["struct", "static", "signed", "unsigned"] = spanTypeName (t:ss)
    | s `elem` ["long", "short"] && t == "int" = fst' ("int":) $ getStars ss
    | [s, t] == ["long", "double"] = fst' ("double":) $ getStars ss
    | otherwise = fst' (s:) $ getStars (t:ss)
        where
            getStars = span (=="*")

-- Parse variable declarations
-- Input: ONLY variable declarations (WITHOUT SEMICOLON)
-- Output: A list of declarations
-- We don't distinguish signed and unsigned, short or long integers here,
-- so that the type name would be a lot easier to handle
-- No weird brackets should be included
parseDeclaration :: [String] -> [VariableDeclaration]
parseDeclaration x = [parseVar _x | _x <- variables] where
    (typeName, afterTypeName) = spanTypeName x
    type_ = parseType typeName
    variables = splitTopLevelOn "{" "}" "," afterTypeName
    parseVar t = if isArr t then arrDecl t else varDecl t

    isArr [_:"["] = True
    isArr _ = False

    arrDecl (name:afterName) = ArrDecl type_ name size init_ where
        (size, afterSBrackets) = fst' (parseExp . extract) $ spanBracket "[" "]" afterName
        init_ =
            if null afterSBrackets
            then Nothing
            else Just $ parseInitList $ tail afterSBrackets
    varDecl (name:afterName) = VarDecl type_ name init_ where
        init_ =
            if null afterName
            then Nothing
            else if afterName !! 1 == "{"
                then Just $ parseInitList $ tail afterName
                else Just $ InitExp $ parseExp $ tail afterName

-- If an input is immediately preceded by a subblock
isSubBlock :: [String] -> Bool
isSubBlock x = (not $ null x) && (head x `elem` ["if", "switch", "while", "for", "do"])

-- Parse a subblock
-- Input: An input that IS preceded by a subblock
-- Output: (The_Subblock, Code_after_the_Subblock)
-- Works by entailing specific subblock parser functions
spanParseSubBlock :: [String] -> (Structure, [String])
spanParseSubBlock s@(x:_) =
    case x of
        "if" -> spanParseIfElse s
        "switch" -> parseSwitch s
        "do" -> parseDoWhile s
        "while" -> spanParseWhile s
        "for" -> spanParseFor s
        "struct" -> fst' LocalStructDefinition $ spanParseStruct s

-- Parse an if..else subblock
-- Input: An input that IS preceded by an if..else.. subblock
-- Output: (The_IfElse_Subblock, Code_after_the_Subblock)
spanParseIfElse :: [String] -> (Structure, [String])
spanParseIfElse ("if":afterIf) = (IfBlock (parseExp condition) primaryBranch secondaryBranch, rest) where
    (condition, afterCondition) = spanBracket "(" ")" afterIf
    (primaryBranch, afterPrimaryBranch)
        | head afterCondition == "{" = fst' (parseCtlFlow . extract) $ spanBracket "{" "}" afterCondition
        | isSubBlock afterCondition = fst' return $ spanParseSubBlock afterCondition
        | otherwise = fst' parseCtlFlow $ span' (/= ";") afterCondition
    (secondaryBranch, rest)
        | (not $ null afterPrimaryBranch) && (head afterPrimaryBranch == "else") = hasSecondaryBranch
        | otherwise = ([], afterPrimaryBranch)
        where
            t = tail afterPrimaryBranch
            hasSecondaryBranch
                | afterPrimaryBranch !! 1 == "{" = fst' (parseCtlFlow . extract) $ spanBracket "{" "}" t
                | isSubBlock t = fst' return $ spanParseSubBlock t
                | otherwise = fst' parseCtlFlow $ span' (/= ";") t

parseSwitch = undefined

parseDoWhile = undefined

spanParseStruct :: [String] -> (StructDefinition, [String])
spanParseStruct ("struct":name:x) = (StructDef name vars, rest) where
    (body, rest') = fst' extract $ spanBracket "{" "}" x
    vars = foldl (\l (Declaration (VarDecl type_ n _)) -> (type_, n):l ) [] $ parseCtlFlow body
    rest = case rest' of
        ";":xs -> xs
        t:";":xs -> [name, t, ";"] ++ xs

spanParseFor :: [String] -> (Structure, [String])
spanParseFor ("for":afterFor) = (ForBlock (parseExp init') condition (parseExp delta) body, rest) where
    (bracket, afterBracket) = fst' extract $ spanBracket "(" ")" afterFor
    (body, rest)
        | head afterBracket == "{" = fst' (parseCtlFlow . extract) $ spanBracket "{" "}" afterBracket
        | isSubBlock afterBracket = fst' pure $ spanParseSubBlock afterBracket
        | otherwise = fst' parseCtlFlow $ span' (/= ";") afterBracket
    [init', condition', delta] = splitOn [";"] bracket
    condition = foldl1 (\a b -> Exp "&&" [a, b]) $ map parseExp $ splitTopLevelOn "(" ")" "," condition'

-- Parse a while subblock
-- Input: An input that IS preceded by a while subblock
-- Output: (The_While_Subblock, Code_after_the_Subblock)
spanParseWhile :: [String] -> (Structure, [String])
spanParseWhile ("while":afterWhile) = (WhileBlock (parseExp condition) loopBody, rest) where
    (condition, afterCondition) = spanBracket "(" ")" afterWhile
    (loopBody, rest)
        | head afterCondition == "{" = fst' (parseCtlFlow . extract) $ spanBracket "{" "}" afterCondition
        | isSubBlock afterCondition = fst' return $ spanParseSubBlock afterCondition
        | otherwise = fst' parseCtlFlow $ span' (/= ";") afterCondition


-- Parse a serial of code
-- Such as void function(){__code__}
-- or while(0){__code__}
-- Input: A serial of code
-- Output: A list of parsed structures
parseCtlFlow :: [String] -> [Structure]
parseCtlFlow [] = []
parseCtlFlow (";":xs) = parseCtlFlow xs
parseCtlFlow x
    | isJump x = let (s, rest) = spanParseJump x in s:(parseCtlFlow rest)
parseCtlFlow x
    | isDeclaration x =
        let (decls, ";":rest) = span (/= ";") x
        in foldr (\t l -> (Declaration t):l) (parseCtlFlow rest) (parseDeclaration decls)
parseCtlFlow x = let (s, rest) = f x in s:(parseCtlFlow rest) where
    f
        | isSubBlock x = spanParseSubBlock
        | isTypeDef x = parseTypeDef
        | otherwise = (\s -> let (exp, ";":rest) = span (/= ";") x in (Expression $ parseExp exp, rest))

spanParseJump :: [String] -> (Structure, [String])
spanParseJump ("break":";":xs) = (UCJump Break, xs)
spanParseJump ("continue":";":xs) = (UCJump Continue, xs)
spanParseJump ("return":xs) = let (x, ";":rest) = span (/= ";") xs in (UCJump $ Return $ parseExp x, rest)

parseTypeDef = undefined

-- If the input is preceded by a function definition
-- Input: An input that may be preceded by a definition
-- Assuming that this function would be called only in the top scope
isFunctionDefinition :: [String] -> Bool
isFunctionDefinition x = (not $ isDeclaration x) && (not $ isStruct x)

isStruct :: [String] -> Bool
isStruct ("struct":_) = True
isStruct _ = False

spanParseFuncDef :: [String] -> (FunctionDefinition, [String])
spanParseFuncDef ("struct":xs) = spanParseFuncDef xs
spanParseFuncDef x = (FuncDef name rtType args ins, rest) where
    (typeName, afterTypeName) = spanTypeName x
    (name:afterName) = afterTypeName
    (args', afterArgs) = fst' extract $ spanBracket "(" ")" afterName
    (body, rest) = fst' extract $ spanBracket "{" "}" afterArgs
    rtType = parseType typeName
    args =
        let l = splitOn [","] args'
        in
            if l == [[]]
            then []
            else (\(VarDecl t n _) -> (t, n)) <$> (head . parseDeclaration) <$> splitOn [","] args'
    ins = parseCtlFlow body


parse :: String -> Program
parse = parse' (Program [] [] []) . mergeOps . split2w . decomment

parse' :: Program -> [String] -> Program
parse' p [] = p
parse' (Program funcs types vars) x
    | isStruct x =
        let (s, rest) = spanParseStruct x
        in parse' (Program funcs (s:types) vars) rest
    | isDeclaration x =
        let (vs, ";":rest) = fst' parseDeclaration $ span (/= ";") x
        in parse' (Program funcs types (vs ++ vars)) rest
    | isFunctionDefinition x =
        let (f, rest) = spanParseFuncDef x
        in parse' (Program (f:funcs) types vars) rest

parseFuncBody = parseCtlFlow . split2w

module Grammar(
Exp(..),
VarInit(..),
VariableDeclaration(..),
FunctionDefinition(..),
StructDefinition(..),
Jump(..),
Program(..),
Structure(..),
)where

import Definition
import Type

data Exp = Exp Identifier [Exp] | Constant Literal deriving Show
data VarInit = InitList [Exp] | InitExp Exp deriving Show
data VariableDeclaration = VarDecl Type Identifier (Maybe VarInit) | ArrDecl Type Identifier Exp (Maybe VarInit) deriving Show
data FunctionDefinition = FuncDef Identifier Type [(Type, Identifier)] [Structure]  deriving Show
data StructDefinition = StructDef Identifier [(Type, Identifier)] deriving Show
data Jump = Return Exp | Break | Continue deriving Show
data Program = Program [FunctionDefinition] [StructDefinition] [VariableDeclaration] deriving Show

data Structure =
    IfBlock Exp [Structure] [Structure] |
    SwitchBlock Exp [([Exp], [Structure])] |
    WhileBlock Exp [Structure] |
    DoWhileBlock Exp [Structure] |
    ForBlock Exp Exp Exp [Structure] |
    Expression Exp |
    Declaration VariableDeclaration |
    UCJump Jump |
    LocalStructDefinition StructDefinition |
    DarkMagic Identifier
    deriving Show


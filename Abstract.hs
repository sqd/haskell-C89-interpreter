module Abstract(
makeIf,
makeLoop,
makeJump
)where

import Program

makeIf :: Call -> (Call, Call) -> Call
makeIf guard (pos, neg) = Call "void" f [(0, guard)] where
    f :: Ins
    f (State _ v c) [condition] =
        return (case convert condition "bool" of
            Value "bool" "true" -> State pos v c
            _ -> State neg v c, nullVal)

convert :: Value -> Type -> Value
convert = undefined

nullVal :: Value
nullVal = undefined

makeLoop :: Call -> Call -> Call -> Call
makeLoop guard body esc = makeIf guard (makeLoop guard body esc, esc)

makeJump :: Call -> Call
makeJump target = Call "void" (\(State _ v c) _ -> return (State target v c, nullVal)) []

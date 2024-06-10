module VM where

import Control.Monad.State

import Chunk
import Value

data InterpretResult
    = InterpretOK
    | InterpretCompileError
    | InterpretRuntimeError
    deriving (Show)

data VM = VM {
    chunk :: Chunk,
    ip :: Int,
    stack :: [Value]
}

type VMState = State VM InterpretResult

push :: Value -> State VM ()
push value = modify (\vm -> vm { stack = value : stack vm })

pop :: State VM Value
pop = do
    vm <- get
    case stack vm of
        [] -> error "Stack underflow"
        (x:xs) -> do
            put vm { stack = xs }
            return x

binaryOp :: (Double -> Double -> Double) -> State VM ()
binaryOp operation = do
    v1 <- pop
    v2 <- pop
    case (v1, v2) of
        (Number a, Number b) -> push $ Number (b `operation` a)
        _ -> error "Type error: binaryOp requires Number values"

run :: VMState
run = do
    vm <- get
    let instructions = code (chunk vm)
    let currentIp = ip vm
    if currentIp < length instructions
        then do
            let instruction = instructions !! currentIp
            modify (\vm' -> vm' { ip = currentIp + 1 })
            case instruction of
                OpReturn -> return InterpretOK
                OpConstant value -> do
                    push value
                    run
                OpNegate -> do
                    value <- pop
                    case value of
                        Number num -> push $ Number (-num)
                        _ -> error "Type error in OpNegate"
                    run
                OpAdd -> binaryOp (+) >> run
                OpSubtract -> binaryOp (-) >> run
                OpMultiply -> binaryOp (*) >> run
                OpDivide -> binaryOp (/) >> run
        else return InterpretRuntimeError

interpret :: Chunk -> VMState
interpret chunk' = do
    modify (\vm -> vm { chunk = chunk', ip = 0, stack = [] })
    run
module Chunk where

import Value

data OpCode
    = OpConstant Value
    | OpReturn
    | OpNegate
    | OpAdd
    | OpSubtract
    | OpMultiply
    | OpDivide
    deriving (Show)

data LineInfo = LineInfo {
    lineNumber :: Int,
    count :: Int
}

data Chunk = Chunk {
    code :: [OpCode],
    lineNumbers :: [LineInfo]
}

initChunk :: Chunk
initChunk = Chunk [] []

updateLineNumbers :: Chunk -> Int -> [LineInfo]
updateLineNumbers chunk line =
    let existingLines = lineNumbers chunk
    in case existingLines of
        [] -> [LineInfo line 1]
        (LineInfo currentLine count : xs) ->
            if currentLine == line
                then LineInfo currentLine (count + 1) : xs
                else LineInfo line 1 : existingLines

writeChunk :: Chunk -> OpCode -> Int -> Chunk
writeChunk chunk opcode line = chunk {
    code = code chunk ++ [opcode],
    lineNumbers = updateLineNumbers chunk line
    }

writeConstant :: Chunk -> Value -> Int -> Chunk
writeConstant chunk value line = 
    writeChunk chunk (OpConstant value) line
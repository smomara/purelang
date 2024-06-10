module Main where

import Control.Monad.State

import Chunk
import Value
import VM

main :: IO ()
main = do
    let initialVM = VM initChunk 0 []
    let chunk1 = writeConstant initChunk (Number 12.0) 123
    let chunk2 = writeConstant chunk1 (Number 15.0) 123
    let chunk3 = writeChunk chunk2 OpAdd 123
    let chunk4 = writeConstant chunk3 (Number 8.0) 123
    let chunk5 = writeChunk chunk4 OpDivide 123
    let chunk6 = writeChunk chunk5 OpNegate 123
    let finalChunk = writeChunk chunk6 OpReturn 123
    
    let (result, finalVM) = runState (interpret finalChunk) initialVM
    print result
    print (stack finalVM)
module Main where

import System.Environment
import Control.Monad.State
import qualified Data.ByteString as BS

import Ines
import DisAsm


main :: IO ()
main = do
     romName:_ <- getArgs
     rom <- BS.readFile romName
     let header = parseHeader rom
     let prgBankData = take (prgBanks header*0x4000) (drop 0x10 (BS.unpack rom))
     let disasm = initDisassembler prgBankData
     (instructions, _) <- runStateT (do {disassembleAll}) disasm
     putStrLn $ generateOutput instructions
     return ()

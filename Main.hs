module Main where

import Control.Monad.State
import qualified Data.ByteString as BS

import Ines
import DisAsm


main :: IO ()
main = do
     rom <- BS.readFile "smb.nes"
     let header = parseHeader rom
     let prgBankData = take (prgBanks header*0x4000) (drop 0x10 (BS.unpack rom))
     let disasm = initDisassembler prgBankData
     (instructions, _) <- runStateT (do {disassembleAll}) disasm
     putStrLn $ generateOutput instructions
     return ()

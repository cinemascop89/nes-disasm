module Ines where

import Data.ByteString as BS
import Data.Word
import Data.Char
import Data.Bits
import Control.Monad

data MirroringType = Horizontal | Vertical | FourScreen deriving Show

data InesHeader = InesHeader {
     prgBanks :: Int,
     chrBanks :: Int,
     mirroring :: MirroringType,
     hasBatteryRAM :: Bool,
     hasTrainer :: Bool,
     mapperNumber :: Int,
     ramBanks :: Int
     } deriving Show


newtype Parser a = P {runP :: ByteString -> [(a, ByteString)]}

instance Functor Parser where
         fmap = liftM

instance Applicative Parser where
         pure a = P $ \cs -> [(a, cs)]
         f1 <*> f2 = f1 >>= \v1 -> f2 >>= (pure . v1)
         p *> k = p >>= \_ -> k

instance Monad Parser where
         return = pure
         (>>) = (*>)
         p >>= f = P $ \cs ->
                         Prelude.concat [runP (f a) cs' | (a, cs') <- runP p cs]


pFail :: Parser a
pFail = P $ \_ -> []

item :: Parser Word8
item = P $ \cs -> if cs == BS.empty then [] else [(BS.head cs, BS.tail cs)]


pSat :: (Word8 -> Bool) -> Parser Word8
pSat p = do
     c <- item
     if p c then return c else pFail

pSym :: Char -> Parser Word8
pSym c = pSat (==fromIntegral (ord c))

number8 :: Parser Int
number8 = do
       c <- item
       return $ fromIntegral c

getMirroringType :: Int -> MirroringType
getMirroringType b = if testBit b 3 then
                       FourScreen
                     else if testBit b 0 then
                       Vertical
                     else
                       Horizontal

getMapperNumber b1 b2 = (b1 .&. 0xf0)`shiftR`4 + (b2 .&. 0xf)

parseNES :: Parser InesHeader
parseNES = do
         -- pSym 'n'
         -- pSym 'e'
         -- pSym 's'
         item
         item
         item
         item
         prgBanks <- number8
         chrBanks <- number8
         controlByte1 <- number8
         controlByte2 <- number8
         ramBanks <- number8

         return $ InesHeader prgBanks chrBanks (getMirroringType controlByte1)
                                      (testBit controlByte1 1) -- hasBatteryRam
                                      (testBit controlByte1 2) -- hasTrainer
                                      (getMapperNumber controlByte1 controlByte2)
                                      ramBanks

parseHeader :: ByteString -> InesHeader
parseHeader bs = fst $ Prelude.head (runP parseNES bs)

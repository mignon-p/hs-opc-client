import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import Data.Word
import Network.Socket
import Network.Socket.ByteString
import System.Environment
import System.Exit

data Frame = SetColors
             { fChannel :: !Word8
             , fPixels  :: [Pixel]
             }
           | UnknownCommand
             { fChannel :: !Word8
             , fCommand :: !Word8
             , fData    :: [Word8]
             }

data Pixel =
  Pixel
  { pRed   :: !Word8
  , pGreen :: !Word8
  , pBlue  :: !Word8
  }

cmdSetColors :: Word8
cmdSetColors = 0

instance Binary Frame where
  put f@(SetColors {}) = do
    putWord8 (fChannel f)
    putWord8 cmdSetColors
    let pix = fPixels f
    putWord16be $ fromIntegral (3 * length pix)
    forM_ pix put

  put f@(UnknownCommand {}) = do
    putWord8 (fChannel f)
    putWord8 (fCommand f)
    let d = fData f
    putWord16be $ fromIntegral (length d)
    forM_ d putWord8

  get = do
    ch <- getWord8
    cmd <- getWord8
    len <- getWord16be
    case cmd of
      0 {- cmdSetColors -} -> do
        let (nPix, extra) = len `divMod` 3
        pix <- replicateM (fromIntegral nPix) get
        skip (fromIntegral extra)
        return $ SetColors ch pix
      _ -> do
        d <- replicateM (fromIntegral len) getWord8
        return $ UnknownCommand ch cmd d

instance Binary Pixel where
  put p = do
    putWord8 (pRed p)
    putWord8 (pGreen p)
    putWord8 (pBlue p)

  get = Pixel <$> getWord8 <*> getWord8 <*> getWord8

nLights = 512                   -- max number of lights on a FadeCandy

mkFrame :: Pixel -> Frame
mkFrame c = SetColors 0 $ replicate nLights c

black = Pixel 0 0 0
blue = Pixel 0 0 255
cyan = Pixel 0 255 255
green = Pixel 0 255 0
magenta = Pixel 255 0 255
orange = Pixel 255 128 0
purple = Pixel 192 0 255
red = Pixel 255 0 0
white = Pixel 255 255 255
yellow = Pixel 255 255 0

sendFrame :: Socket -> Frame -> IO ()
sendFrame s f = sendMany s $ L.toChunks $ encode f

main = do
  args <- getArgs
  color <- case args of
             []          -> return white
             ["black"]   -> return black
             ["blue"]    -> return blue
             ["cyan"]    -> return cyan
             ["green"]   -> return green
             ["magenta"] -> return magenta
             ["orange"]  -> return orange
             ["purple"]  -> return purple
             ["red"]     -> return red
             ["white"]   -> return white
             ["yellow"]  -> return yellow
             [r, g, b]   -> return $ Pixel (read r) (read g) (read b)
             _ -> do
               putStrLn "Argument must be one of:"
               putStrLn "    black, blue, cyan, green, magenta, orange, purple, red, white, yellow"
               putStrLn "Or three integer arguments between 0-255"
               exitFailure
  ai <- getAddrInfo Nothing (Just "127.0.0.1" {- "localhost" -}) (Just "7890")
  let addr = addrAddress $ head ai
      fam (SockAddrInet {}) = AF_INET
      fam (SockAddrInet6 {}) = AF_INET6
      fam sa = error $ "Unexpected socket family: " ++ show sa
  s <- socket (fam addr) Stream defaultProtocol
  connect s addr
  let f = mkFrame color
  sendFrame s f
  -- send frame twice to defeat FadeCandy's fade logic; we want immediate results!
  sendFrame s f

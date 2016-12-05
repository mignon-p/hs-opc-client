import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import Data.Word
import Network.Socket
import Network.Socket.ByteString

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

frame :: Frame
frame = SetColors 0 $ replicate 50 $ Pixel 255 255 255

main = do
  ai <- getAddrInfo Nothing (Just "localhost") (Just "7890")
  let addr = addrAddress $ head ai
      fam (SockAddrInet {}) = AF_INET
      fam (SockAddrInet6 {}) = AF_INET6
      fam sa = error $ "Unexpected socket family: " ++ show sa
  s <- socket (fam addr) Stream defaultProtocol
  connect s addr
  let bs = encode frame
  sendMany s (L.toChunks bs)

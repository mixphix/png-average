module Main where

import Codec.Picture
import Control.Monad
import Control.Monad.State.Class
import Control.Monad.State.Strict (runState)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Numeric.Natural (Natural)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "no file specified"
    _ -> for_ args \path -> do
      (png, (rs, gs, bs, as, n)) <- process <$> pngFile path
      print $ PixelRGBA8
        do fromIntegral $ rs `div` n
        do fromIntegral $ gs `div` n
        do fromIntegral $ bs `div` n
        do fromIntegral $ as `div` n
      writePng path png

pngFile :: FilePath -> IO (Image PixelRGBA8)
pngFile = either error rgba8 . decodePng <=< BS.readFile
 where
  rgba8 = \case
    ImageRGBA8 i -> pure i
    ImageRGB8 i -> flip imagePixels i \(PixelRGB8 r g b) -> pure $ PixelRGBA8 r g b 255
    _ -> error "womp womp"

process ::
  Image PixelRGBA8 ->
  (Image PixelRGBA8, (Natural, Natural, Natural, Natural, Natural))
process png = do
  let go (PixelRGBA8 r g b a) = do
        ( rs :: Natural
          , gs :: Natural
          , bs :: Natural
          , as :: Natural
          , n :: Natural
          ) <-
          get
        put
          ( rs + fromIntegral r
          , gs + fromIntegral g
          , bs + fromIntegral b
          , as + fromIntegral a
          , succ n
          )
        pure $ PixelRGBA8
          do fromIntegral $ (rs + fromIntegral r) `div` succ n
          do fromIntegral $ (gs + fromIntegral g) `div` succ n
          do fromIntegral $ (bs + fromIntegral b) `div` succ n
          do fromIntegral $ (as + fromIntegral a) `div` succ n
   in imagePixels go png `runState` (0, 0, 0, 0, 0)

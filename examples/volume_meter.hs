import Sound.Alsa

import Foreign
import Data.Word

bufSize :: Int
bufSize = 4096

inputFormat :: SoundFmt
inputFormat = SoundFmt {
                sampleFmt   = SampleFmtLinear16BitSignedLE,
                sampleFreq  = 8000,
                numChannels = 1
                }


main :: IO ()
main = 
    do let src = alsaSoundSource "plughw:0,0" inputFormat
       h <- soundSourceOpen src
       allocaArray bufSize $ loop src h bufSize
       soundSourceClose src h

-- FIXME: assumes little-endian machine
loop :: SoundSource h -> h -> Int -> Ptr Int16 -> IO ()
loop src h n buf =
    do n' <- soundSourceRead src h (castPtr buf) n
       avg <- avgBuf buf n'
       putStrLn (replicate (avg `div` 20) '*')
       loop src h n buf

avgBuf :: (Storable a, Integral a) => Ptr a -> Int -> IO Int
avgBuf buf n = do xs <- peekArray n buf
                  let xs' = map (fromIntegral . abs) xs :: [Int]
                  return $ sum xs' `div` fromIntegral n

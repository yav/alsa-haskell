import Sound.Alsa

import Foreign
import Data.Word

bufSize :: Int
bufSize = 1000

inputFormat :: SoundFmt
inputFormat = SoundFmt {
                sampleFmt   = SampleFmtLinear16BitSignedLE,
                sampleFreq  = 8000,
                numChannels = 1
                }


main :: IO ()
main = let source = alsaSoundSource "plughw:0,0" inputFormat
        in allocaArray     bufSize $ \buf  -> 
           withSoundSource source  $ \from ->
               loop source from bufSize buf

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

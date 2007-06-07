import Sound.Alsa

import Foreign
import Data.Word

bufSize :: Int
bufSize = 4096

main :: IO ()
main = 
    do h <- pcm_open "default" PcmStreamCapture 0
       p <- pcm_hw_params_malloc
       pcm_hw_params_any h p
       pcm_hw_params_set_access h p PcmAccessRwInterleaved
       pcm_hw_params_set_format h p PcmFormatS16Le
       pcm_hw_params_set_rate h p 44100 0
       pcm_hw_params_set_channels h p 2
       pcm_hw_params h p
       pcm_hw_params_free p
       pcm_prepare h
       allocaArray bufSize $ loop h bufSize
       pcm_close h


loop :: Pcm -> Int -> Ptr Int16 -> IO ()
loop h n buf =
    do pcm_readi h buf n
       avg <- avgBuf buf n
       putStrLn (replicate (avg `div` 20) '*')
       loop h n buf

avgBuf :: (Storable a, Integral a) => Ptr a -> Int -> IO Int
avgBuf buf n = do xs <- peekArray n buf
                  let xs' = map (fromIntegral . abs) xs :: [Int]
                  return $ sum xs' `div` fromIntegral n

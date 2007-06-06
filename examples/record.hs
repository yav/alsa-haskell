import Sound.Alsa

import Foreign
import Data.Word

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
       allocaBytes 256 $ \buf -> do pcm_readi h buf 128
                                    dump (castPtr buf) 128
       pcm_close h

dump :: Ptr Word16 -> Int -> IO ()
dump p n | n == 0 = return ()
         | otherwise =
             do x <- peek p
                print x
                dump (p `plusPtr` 2) (n-1)
              
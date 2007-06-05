module Sound.ALSA where

import Sound.ALSA.C2HS

import Control.Exception (throw)

#include <alsa/asoundlib.h>

{#context prefix = "snd_"#}

{#pointer *snd_pcm_t as PCM newtype #}

instance Storable PCM where
    sizeOf (PCM r) = sizeOf r
    alignment (PCM r) = alignment r
    peek p = fmap PCM (peek (castPtr p))
    poke p (PCM r) = poke (castPtr p) r

{#enum _snd_pcm_stream as PCMStream {underscoreToCase} deriving (Eq,Show)#}


{#fun pcm_open
  { alloca- `PCM' peek*,
    withCString* `String',
    cFromEnum `PCMStream',
    `Int'}
 -> `()' result*- #}
  where result = checkResult_ "pcm_open"



checkResult :: String -> CInt -> IO CInt
checkResult f r | r < 0 = ioError (errnoToIOError f (Errno r) Nothing Nothing)
                | otherwise = return r

checkResult_ :: String -> CInt -> IO ()
checkResult_ f r = checkResult f r >> return ()

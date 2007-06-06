module Sound.ALSA where

import Sound.ALSA.C2HS

import Control.Exception (throw)

#include <alsa/asoundlib.h>

{#context prefix = "snd_"#}

{#pointer *snd_pcm_t as Pcm newtype #}

instance Storable Pcm where
    sizeOf (Pcm r) = sizeOf r
    alignment (Pcm r) = alignment r
    peek p = fmap Pcm (peek (castPtr p))
    poke p (Pcm r) = poke (castPtr p) r

{#pointer *snd_pcm_hw_params_t as PcmHwParams newtype #}

{#enum _snd_pcm_stream as PcmStream {underscoreToCase} deriving (Eq,Show)#}

{#enum _snd_pcm_access as PcmAccess {underscoreToCase} deriving (Eq,Show)#}

{#enum _snd_pcm_format as PcmFormat {underscoreToCase} deriving (Eq,Show)#}


{#fun pcm_open
  { alloca- `Pcm' peek*,
    withCString* `String',
    cFromEnum `PcmStream',
    `Int'}
 -> `()' result*- #}
  where result = checkResult_ "pcm_open"

{#fun pcm_close
  { id `Pcm' }
 -> `()' result*- #}
  where result = checkResult_ "pcm_close"

{#fun pcm_prepare
  { id `Pcm' }
 -> `()' result*- #}
  where result = checkResult_ "pcm_prepare"

{#fun pcm_hw_params
  { id `Pcm',
    id `PcmHwParams' }
 -> `()' result*- #}
  where result = checkResult_ "pcm_hw_params"

{#fun pcm_hw_params_any
  { id `Pcm',
    id `PcmHwParams' }
 -> `()' result*- #}
  where result = checkResult_ "pcm_hw_params_any"

{#fun pcm_hw_params_set_access
  { id `Pcm',
    id `PcmHwParams',
    cFromEnum `PcmAccess'
 }
 -> `()' result*- #}
  where result = checkResult_ "pcm_hw_params_set_access"

{#fun pcm_hw_params_set_format
  { id `Pcm',
    id `PcmHwParams',
    cFromEnum `PcmFormat'
 }
 -> `()' result*- #}
  where result = checkResult_ "pcm_hw_params_set_format"

{#fun pcm_hw_params_set_rate
  { id `Pcm',
    id `PcmHwParams',
    `Int',
    `Int'
 }
 -> `()' result*- #}
  where result = checkResult_ "pcm_hw_params_set_rate"

{#fun pcm_hw_params_set_channels
  { id `Pcm',
    id `PcmHwParams',
    `Int'
 }
 -> `()' result*- #}
  where result = checkResult_ "pcm_hw_params_set_channels"

{-
snd_pcm_hw_params_malloc
snd_pcm_hw_params_free
snd_pcm_readi
-}


checkResult :: String -> CInt -> IO CInt
checkResult f r | r < 0 = ioError (errnoToIOError f (Errno r) Nothing Nothing)
                | otherwise = return r

checkResult_ :: String -> CInt -> IO ()
checkResult_ f r = checkResult f r >> return ()

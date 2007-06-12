module Sound.Alsa.Core where

import Sound.Alsa.C2HS

import Control.Exception (throw)

-- HACK for 32-bit machines.
-- This is only used to be able to parse alsa/pcm.h,
-- since snd_pcm_format_silence_64 use u_int64_t which is not
-- defined on 32-bit machines, AFAICT
#define u_int64_t unsigned long long int

#include <alsa/asoundlib.h>

{#context prefix = "snd_"#}

{#pointer *snd_pcm_t as Pcm newtype #}

instance Storable Pcm where
    sizeOf (Pcm r) = sizeOf r
    alignment (Pcm r) = alignment r
    peek p = fmap Pcm (peek (castPtr p))
    poke p (Pcm r) = poke (castPtr p) r

{#pointer *snd_pcm_hw_params_t as PcmHwParams newtype #}

instance Storable PcmHwParams where
    sizeOf (PcmHwParams r) = sizeOf r
    alignment (PcmHwParams r) = alignment r
    peek p = fmap PcmHwParams (peek (castPtr p))
    poke p (PcmHwParams r) = poke (castPtr p) r

{#pointer *snd_pcm_sw_params_t as PcmSwParams newtype #}

instance Storable PcmSwParams where
    sizeOf (PcmSwParams r) = sizeOf r
    alignment (PcmSwParams r) = alignment r
    peek p = fmap PcmSwParams (peek (castPtr p))
    poke p (PcmSwParams r) = poke (castPtr p) r

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

{#fun pcm_drop
  { id `Pcm' }
 -> `()' result*- #}
  where result = checkResult_ "pcm_drop"

{#fun pcm_drain
  { id `Pcm' }
 -> `()' result*- #}
  where result = checkResult_ "pcm_drain"

{-
-- Only available in 1.0.11rc3 and later
{#fun pcm_set_params
  { id `Pcm',
    cFromEnum `PcmFormat',
    cFromEnum `PcmAccess',
    `Int',
    `Int',
    `Bool',
    `Int' }
 -> `()' result*- #}
  where result = checkResult_ "pcm_set_params"
-}

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

{-
-- Available in 1.0.9rc2 and later
{#fun pcm_hw_params_set_rate_resample
  { id `Pcm',
    id `PcmHwParams',
    `Bool'
 }
 -> `()' result*- #}
  where result = checkResult_ "pcm_hw_params_set_rate_resample"
-}

{#fun pcm_hw_params_set_channels
  { id `Pcm',
    id `PcmHwParams',
    `Int'
 }
 -> `()' result*- #}
  where result = checkResult_ "pcm_hw_params_set_channels"

{#fun pcm_hw_params_set_buffer_size
  { id `Pcm',
    id `PcmHwParams',
    `Int'
 }
 -> `()' result*- #}
  where result = checkResult_ "pcm_hw_params_set_buffer_size"

{#fun pcm_hw_params_get_buffer_size
  { id `PcmHwParams',
    alloca- `Int' peekIntConv*
 }
 -> `()' result*- #}
  where result = checkResult_ "pcm_hw_params_get_buffer_size"


{#fun pcm_hw_params_set_periods
  { id `Pcm',
    id `PcmHwParams',
    `Int',
    `Int'
 }
 -> `()' result*- #}
  where result = checkResult_ "pcm_hw_params_set_periods"

{#fun pcm_hw_params_set_buffer_time_near
  { id `Pcm',
    id `PcmHwParams',
    `Int',
    `Int'
 }
 -> `()' result*- #}
  where result = checkResult_ "pcm_hw_params_set_buffer_time_near"

{#fun pcm_hw_params_get_buffer_time
  { id `PcmHwParams',
    alloca- `Int' peekIntConv*,
    alloca- `Int' peekIntConv*
 }
 -> `()' result*- #}
  where result = checkResult_ "pcm_hw_params_get_buffer_time"

{#fun pcm_sw_params_set_xfer_align
  { id `Pcm',
    id `PcmSwParams',
    `Int' }
 -> `()' result*- #}
  where result = checkResult_ "pcm_sw_params_set_xfer_align"

{#fun pcm_readi
  { id `Pcm',
    castPtr `Ptr a',
    `Int'
 }
 -> `Int' result* #}
  where result = fmap fromIntegral . checkResult "pcm_readi"

{#fun pcm_writei
  { id `Pcm',
    castPtr `Ptr a',
    `Int'
 }
 -> `Int' result* #}
  where result = fmap fromIntegral . checkResult "pcm_writei"

{#fun pcm_hw_params_malloc
  { alloca- `PcmHwParams' peek* }
 -> `()' result*- #}
  where result = checkResult_ "pcm_hw_params_malloc"

{#fun pcm_hw_params_free
  { id `PcmHwParams' }
 -> `()' #}

{#fun pcm_sw_params_malloc
  { alloca- `PcmSwParams' peek* }
 -> `()' result*- #}
  where result = checkResult_ "pcm_sw_params_malloc"

{#fun pcm_sw_params_free
  { id `PcmSwParams' }
 -> `()' #}

{#fun pcm_sw_params
  { id `Pcm',
    id `PcmSwParams' }
 -> `()' result*- #}
  where result = checkResult_ "pcm_sw_params"

{#fun pcm_sw_params_current
  { id `Pcm',
    id `PcmSwParams' }
 -> `()' result*- #}
  where result = checkResult_ "pcm_sw_params_current"

{#fun strerror
  `Integral a' =>
  { cIntConv `a' }
 -> `String' peekCString* #}

--
-- * Marshalling utilities
--

-- FIXME: use throwDyn?
checkResult :: Integral a => String -> a -> IO a
checkResult f r | r < 0 = ioError (errnoToIOError f (Errno (fromIntegral (negate r))) Nothing Nothing)
                | otherwise = return r

checkResult_ :: Integral a => String -> a -> IO ()
checkResult_ f r = checkResult f r >> return ()

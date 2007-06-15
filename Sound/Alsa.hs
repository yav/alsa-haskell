module Sound.Alsa 
    (SampleFmt(..),
     SampleFreq,
     SoundFmt(..),
     SoundSource(..),
     SoundSink(..),
     withSoundSource,
     withSoundSourceRunning,
     withSoundSink,
     withSoundSinkRunning,
     soundFmtMIME,
     audioBytesPerSample,
     audioBytesPerFrame,
     soundSourceBytesPerFrame,
     soundSinkBytesPerFrame,
     soundSourceReadBytes,
     soundSinkWriteBytes,
     copySound,
     alsaSoundSource,
     alsaSoundSink,
     fileSoundSource,
     fileSoundSink
    ) where

import Sound.Alsa.Core


import Control.Exception (bracket, bracket_)
import Control.Monad (liftM,when)
import Foreign
import System.IO

-- 
-- * Generic sound API
--

data SampleFmt = SampleFmtLinear16BitSignedLE
               | SampleFmtMuLaw8Bit
  deriving (Show)

type SampleFreq = Int

data SoundFmt = SoundFmt {
	sampleFmt :: SampleFmt,
	sampleFreq :: SampleFreq,
 	numChannels :: Int
	}
  deriving (Show)


-- | Counts are in samples, not bytes. Multi-channel data is interleaved.
data SoundSource handle = 
    SoundSource {
                 soundSourceFmt   :: SoundFmt,
                 soundSourceOpen  :: IO handle,
                 soundSourceClose :: handle -> IO (),
                 soundSourceStart :: handle -> IO (),
                 soundSourceStop  :: handle -> IO (),
                 soundSourceRead  :: handle -> Ptr () -> Int -> IO Int
                }

data SoundSink handle = 
    SoundSink {
                 soundSinkFmt   :: SoundFmt,
                 soundSinkOpen  :: IO handle,
                 soundSinkClose :: handle -> IO (),
                 soundSinkWrite :: handle -> Ptr () -> Int -> IO (),
                 soundSinkStart :: handle -> IO (),
                 soundSinkStop  :: handle -> IO ()
                }

--
--
--

nullSoundSource :: SoundFmt -> SoundSource h
nullSoundSource fmt =
    SoundSource {
	         soundSourceFmt   = fmt,
                 soundSourceOpen  = return undefined,
                 soundSourceClose = \_ -> return (),
                 soundSourceStart = \_ -> return (),
                 soundSourceStop  = \_ -> return (),
                 soundSourceRead  = \_ _ _ -> return 0
                }

nullSoundSink :: SoundFmt -> SoundSink h
nullSoundSink fmt = 
    SoundSink {
	       soundSinkFmt   = fmt,
               soundSinkOpen  = return undefined,
               soundSinkClose = \_ -> return (),
               soundSinkStart = \_ -> return (),
               soundSinkStop  = \_ -> return (),
               soundSinkWrite = \_ _ _ -> return ()
              }


withSoundSource :: SoundSource h -> (h -> IO a) -> IO a
withSoundSource source = 
    bracket (soundSourceOpen source) (soundSourceClose source)

withSoundSourceRunning :: SoundSource h -> h -> IO a -> IO a
withSoundSourceRunning src h = bracket_ (soundSourceStart src h) (soundSourceStop src h) 

withSoundSink :: SoundSink h -> (h -> IO a) -> IO a
withSoundSink sink = 
    bracket (soundSinkOpen sink) (soundSinkClose sink)

withSoundSinkRunning :: SoundSink h -> h -> IO a -> IO a
withSoundSinkRunning src h = bracket_ (soundSinkStart src h) (soundSinkStop src h) 

soundFmtMIME :: SoundFmt -> String
soundFmtMIME fmt = t ++ r ++ c
  where t = case sampleFmt fmt of
		SampleFmtLinear16BitSignedLE -> "audio/L16"
		SampleFmtMuLaw8Bit           -> "audio/basic"
        r = ";rate=" ++ show (sampleFreq fmt)
        c | numChannels fmt == 1 = ""
	  | otherwise = ";channels=" ++ show (numChannels fmt)

audioBytesPerSample :: SoundFmt -> Int
audioBytesPerSample fmt = 
	case sampleFmt fmt of
		SampleFmtLinear16BitSignedLE -> 2
		SampleFmtMuLaw8Bit           -> 1

-- assumes interleaved data
audioBytesPerFrame :: SoundFmt -> Int
audioBytesPerFrame fmt = numChannels fmt * audioBytesPerSample fmt

soundSourceBytesPerFrame :: SoundSource h -> Int
soundSourceBytesPerFrame = audioBytesPerFrame . soundSourceFmt

soundSinkBytesPerFrame :: SoundSink h -> Int
soundSinkBytesPerFrame = audioBytesPerFrame . soundSinkFmt

soundSourceReadBytes :: SoundSource h -> h -> Ptr () -> Int -> IO Int
soundSourceReadBytes src h buf n = 
	liftM (* c) $ soundSourceRead src h buf (n `div` c)
  where c = soundSourceBytesPerFrame src

soundSinkWriteBytes :: SoundSink h -> h -> Ptr () -> Int -> IO ()
soundSinkWriteBytes src h buf n = 
	soundSinkWrite src h buf (n `div` c)
  where c = soundSinkBytesPerFrame src

copySound :: SoundSource h1 
          -> SoundSink h2 
          -> Int -- ^ Buffer size (in bytes) to use
          -> IO ()
copySound source sink bufSize = 
    allocaBytes     bufSize $ \buf ->
    withSoundSource source  $ \from ->
    withSoundSink   sink    $ \to ->
       let loop = do n <- soundSourceReadBytes source from buf bufSize
                     when (n > 0) $ do soundSinkWriteBytes sink to buf n
                                       loop
        in loop

--
-- * Alsa stuff
--


debug = hPutStrLn stderr

alsaOpen :: String -- ^ device, e.g @"default"@
	-> SoundFmt -> PcmStream -> IO Pcm
alsaOpen dev fmt stream = 
    do debug "alsaOpen"
       h <- pcm_open dev stream 0
       let buffer_time = 500000 -- 0.5s
           period_time = 100000 -- 0.1s
       (buffer_time,buffer_size,period_time,period_size) <- 
           setHwParams h (sampleFmtToPcmFormat (sampleFmt fmt))
                         (numChannels fmt)
                         (sampleFreq fmt)
                         buffer_time
                         period_time
       setSwParams h buffer_size period_size
       pcm_prepare h
       debug $ "buffer_time = " ++ show buffer_time
       debug $ "buffer_size = " ++ show buffer_size
       debug $ "period_time = " ++ show period_time
       debug $ "period_size = " ++ show period_size
       return h

sampleFmtToPcmFormat :: SampleFmt -> PcmFormat
sampleFmtToPcmFormat SampleFmtLinear16BitSignedLE = PcmFormatS16Le
sampleFmtToPcmFormat SampleFmtMuLaw8Bit           = PcmFormatMuLaw

setHwParams :: Pcm 
            -> PcmFormat 
            -> Int -- ^ number of channels
            -> Int -- ^ sample frequency
            -> Int -- ^ buffer time
            -> Int -- ^ period time
            -> IO (Int,Int,Int,Int) 
               -- ^ (buffer_time,buffer_size,period_time,period_size)
setHwParams h format channels rate buffer_time period_time
  = withHwParams h $ \p ->
    do pcm_hw_params_set_access h p PcmAccessRwInterleaved
       pcm_hw_params_set_format h p format
       pcm_hw_params_set_channels h p channels
       pcm_hw_params_set_rate h p rate EQ
       (buffer_time,_) <- 
           pcm_hw_params_set_buffer_time_near h p buffer_time EQ
       buffer_size <- pcm_hw_params_get_buffer_size p
       (period_time,_) <- 
           pcm_hw_params_set_period_time_near h p period_time EQ
       (period_size,_) <- pcm_hw_params_get_period_size p
       return (buffer_time,buffer_size,period_time,period_size)

setSwParams :: Pcm 
            -> Int -- ^ buffer size 
            -> Int -- ^ period size
            -> IO ()
setSwParams h buffer_size period_size = withSwParams h $ \p -> 
    do let start_threshold = 
               (buffer_size `div` period_size) * period_size
       pcm_sw_params_set_start_threshold h p start_threshold
       pcm_sw_params_set_avail_min h p period_size
       pcm_sw_params_set_xfer_align h p 1

withHwParams :: Pcm -> (PcmHwParams -> IO a) -> IO a
withHwParams h f = 
    do p <- pcm_hw_params_malloc
       pcm_hw_params_any h p
       x <- f p
       pcm_hw_params h p
       pcm_hw_params_free p
       return x

withSwParams :: Pcm -> (PcmSwParams -> IO a) -> IO a
withSwParams h f = 
    do p <- pcm_sw_params_malloc
       pcm_sw_params_current h p
       x <- f p
       pcm_sw_params h p
       pcm_sw_params_free p
       return x

alsaClose :: Pcm -> IO ()
alsaClose pcm = 
    do debug "alsaClose"
       pcm_drain pcm
       pcm_close pcm

alsaStart :: Pcm -> IO ()
alsaStart pcm = 
    do debug "alsaStart"
       pcm_prepare pcm
       pcm_start pcm


-- FIXME: use pcm_drain for sinks?
alsaStop :: Pcm -> IO ()
alsaStop pcm = 
    do debug "alsaStop"
       pcm_drop pcm

alsaRead :: SoundFmt -> Pcm -> Ptr () -> Int -> IO Int
alsaRead fmt h buf n = 
     do --debug $ "Reading " ++ show n ++ " samples..."
        n' <- pcm_readi h buf n
        --debug $ "Got " ++ show n' ++ " samples."
	if n' < n 
          then do n'' <- alsaRead fmt h (buf `plusPtr` (n' * c)) (n - n')
	          return (n' + n'')
          else return n'
  where c = audioBytesPerFrame fmt

alsaWrite :: SoundFmt -> Pcm -> Ptr () -> Int -> IO ()
alsaWrite fmt h buf n = alsaWrite_ fmt h buf n >> return ()

alsaWrite_ :: SoundFmt -> Pcm -> Ptr () -> Int -> IO Int
alsaWrite_ fmt h buf n = 
     do --debug $ "Writing " ++ show n ++ " samples..."
        n' <- pcm_writei h buf n 
        --debug $ "Wrote " ++ show n' ++ " samples."
	if (n' /= n)
            then do n'' <- alsaWrite_ fmt h (buf `plusPtr` (n' * c)) (n - n')
                    return (n' + n'')
            else return n'
  where c = audioBytesPerFrame fmt



alsaSoundSource :: String -> SoundFmt -> SoundSource Pcm
alsaSoundSource dev fmt = 
    (nullSoundSource fmt) {
                           soundSourceOpen  = alsaOpen dev fmt PcmStreamCapture,
                           soundSourceClose = alsaClose,
                           soundSourceStart = alsaStart,
                           soundSourceStop  = alsaStop,
                           soundSourceRead  = alsaRead fmt
                          }

alsaSoundSink :: String -> SoundFmt -> SoundSink Pcm
alsaSoundSink dev fmt = 
    (nullSoundSink fmt) {
                         soundSinkOpen  = alsaOpen dev fmt PcmStreamPlayback,
                         soundSinkClose = alsaClose,
                         soundSinkStart = alsaStart,
                         soundSinkStop  = alsaStop,
                         soundSinkWrite = alsaWrite fmt
                        }

--
-- * File stuff
--

fileRead :: SoundFmt -> Handle -> Ptr () -> Int -> IO Int
fileRead fmt h buf n = liftM (`div` c) $ hGetBuf h buf (n * c)
  where c = audioBytesPerSample fmt

fileWrite :: SoundFmt -> Handle -> Ptr () -> Int -> IO ()
fileWrite fmt h buf n = hPutBuf h buf (n * c)
  where c = audioBytesPerSample fmt

fileSoundSource :: FilePath -> SoundFmt -> SoundSource Handle
fileSoundSource file fmt = 
    (nullSoundSource fmt) {
                           soundSourceOpen  = openBinaryFile file ReadMode,
                           soundSourceClose = hClose,
                           soundSourceRead  = fileRead fmt
                          }

fileSoundSink :: FilePath -> SoundFmt -> SoundSink Handle
fileSoundSink file fmt = 
    (nullSoundSink fmt) {
                         soundSinkOpen  = openBinaryFile file WriteMode,
                         soundSinkClose = hClose,
                         soundSinkWrite = fileWrite fmt
                        }

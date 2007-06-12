module Sound.Alsa where

import Sound.Alsa.Core

import Control.Monad
import Foreign.Ptr (Ptr)
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
                 soundSourceFmt :: SoundFmt,
                 soundSourceOpen :: IO handle,
                 soundSourceRead :: handle -> Ptr () -> Int -> IO Int,
                 soundSourceClose :: handle -> IO ()
                }

data SoundSink handle = 
    SoundSink {
                 soundSinkFmt   :: SoundFmt,
                 soundSinkOpen  :: IO handle,
                 soundSinkWrite :: handle -> Ptr () -> Int -> IO (),
                 soundSinkClose :: handle -> IO ()
                }

--
--
--

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

--
-- * Alsa stuff
--

alsaOpen :: String -- ^ device, e.g @"default"@
	-> SoundFmt -> PcmStream -> IO Pcm
alsaOpen dev fmt stream = 
    do h <- pcm_open dev stream 0
       alsaSetHwParams h $ \h p ->
           do pcm_hw_params_set_access h p PcmAccessRwInterleaved
              pcm_hw_params_set_format h p (sampleFmtToPcmFormat (sampleFmt fmt))
              pcm_hw_params_set_rate h p (sampleFreq fmt) 0
              pcm_hw_params_set_channels h p (numChannels fmt)
       alsaSetSwParams h $ \h p ->
           do -- pcm_sw_params_set_xfer_align h p 1
	      return ()
       pcm_prepare h
       return h

sampleFmtToPcmFormat :: SampleFmt -> PcmFormat
sampleFmtToPcmFormat SampleFmtLinear16BitSignedLE = PcmFormatS16Le
sampleFmtToPcmFormat SampleFmtMuLaw8Bit           = PcmFormatMuLaw

alsaSetHwParams :: Pcm -> (Pcm -> PcmHwParams -> IO a) -> IO a
alsaSetHwParams h f = 
    do p <- pcm_hw_params_malloc
       pcm_hw_params_any h p
       x <- f h p
       pcm_hw_params h p
       pcm_hw_params_free p
       return x

alsaSetSwParams :: Pcm -> (Pcm -> PcmSwParams -> IO a) -> IO a
alsaSetSwParams h f = 
    do p <- pcm_sw_params_malloc
       pcm_sw_params_current h p
       x <- f h p
       pcm_sw_params h p
       pcm_sw_params_free p
       return x

alsaRead :: SoundFmt -> Pcm -> Ptr () -> Int -> IO Int
alsaRead _ h buf n = pcm_readi h buf n

alsaWrite :: SoundFmt -> Pcm -> Ptr () -> Int -> IO ()
alsaWrite _ h buf n = pcm_writei h buf n >> return ()
-- FIXME: check return count from pcm_writei?

alsaClose :: Pcm -> IO ()
alsaClose pcm = 
	do pcm_drain pcm
	   pcm_close pcm

alsaSoundSource :: String -> SoundFmt -> SoundSource Pcm
alsaSoundSource dev fmt = SoundSource {
 		               soundSourceFmt   = fmt,
                               soundSourceOpen  = alsaOpen dev fmt PcmStreamCapture,
                               soundSourceRead  = alsaRead fmt,
                               soundSourceClose = alsaClose
                              }

alsaSoundSink :: String -> SoundFmt -> SoundSink Pcm
alsaSoundSink dev fmt = SoundSink {
 		               soundSinkFmt   = fmt,
                               soundSinkOpen  = alsaOpen dev fmt PcmStreamPlayback,
                               soundSinkWrite = alsaWrite fmt,
                               soundSinkClose = alsaClose
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

-- FIXME: bytes vs sample count
fileSoundSource :: FilePath -> SoundFmt -> SoundSource Handle
fileSoundSource file fmt = 
    SoundSource {
	         soundSourceFmt   = fmt,
                 soundSourceOpen  = openBinaryFile file ReadMode,
                 soundSourceRead  = fileRead fmt,
                 soundSourceClose = hClose
                }

fileSoundSink :: FilePath -> SoundFmt -> SoundSink Handle
fileSoundSink file fmt = 
    SoundSink {
	         soundSinkFmt   = fmt,
                 soundSinkOpen  = openBinaryFile file WriteMode,
                 soundSinkWrite = fileWrite fmt,
                 soundSinkClose = hClose
                }

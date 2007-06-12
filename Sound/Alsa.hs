module Sound.Alsa where

import Sound.Alsa.Core

import Foreign.Ptr (Ptr)
import System.IO

-- 
-- * Generic sound API
--

data SampleFmt = SampleFmtLinear16BitSignedLE
               | SampleFmtMuLaw8Bit
  deriving (Show)

audioBytesPerSample :: SoundFmt -> Int
audioBytesPerSample fmt = 
	case sampleFmt fmt of
		SampleFmtLinear16BitSignedLE -> 2
		SampleFmtMuLaw8Bit           -> 1


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

--
-- * Alsa stuff
--

alsaOpen :: SoundFmt -> IO Pcm
alsaOpen fmt = 
    do h <- pcm_open "default" PcmStreamCapture 0
       alsaSetHwParams h $ \h p ->
           do pcm_hw_params_set_access h p PcmAccessRwInterleaved
              pcm_hw_params_set_format h p (sampleFmtToPcmFormat (sampleFmt fmt))
              pcm_hw_params_set_rate h p (sampleFreq fmt) 0
              pcm_hw_params_set_channels h p (numChannels fmt)
       pcm_prepare h
       return h

sampleFmtToPcmFormat :: SampleFmt -> PcmFormat
sampleFmtToPcmFormat SampleFmtLinear16BitSignedLE = PcmFormatS16Le
sampleFmtToPcmFormat SampleFmtMuLaw8Bit           = PcmFormatMuLaw

alsaSetHwParams :: Pcm -> (Pcm -> PcmHwParams -> IO ()) -> IO ()
alsaSetHwParams h f = 
    do p <- pcm_hw_params_malloc
       pcm_hw_params_any h p
       f h p
       pcm_hw_params h p
       pcm_hw_params_free p

alsaRead :: SoundFmt -> Pcm -> Ptr () -> Int -> IO Int
alsaRead _ h buf n = pcm_readi h buf n

alsaClose :: Pcm -> IO ()
alsaClose = pcm_close

alsaSoundSource :: SoundFmt -> SoundSource Pcm
alsaSoundSource fmt = SoundSource {
 		               soundSourceFmt   = fmt,
                               soundSourceOpen  = alsaOpen fmt,
                               soundSourceRead  = alsaRead fmt,
                               soundSourceClose = alsaClose
                              }

--
-- * File stuff
--

fileRead :: SoundFmt -> Handle -> Ptr () -> Int -> IO Int
fileRead fmt h buf n = fmap (`div` c) $ hGetBuf h buf (n * c)
  where c = audioBytesPerSample fmt

-- FIXME: bytes vs sample count
fileSoundSource :: SoundFmt -> FilePath -> SoundSource Handle
fileSoundSource fmt file = 
    SoundSource {
	         soundSourceFmt  = fmt,
                 soundSourceOpen = openBinaryFile file ReadMode,
                 soundSourceRead = fileRead fmt,
                 soundSourceClose = hClose
                }

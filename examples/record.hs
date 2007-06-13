import Sound.Alsa

import System.Environment
import System.Exit
import System.IO

bufSize :: Int
bufSize = 8192

soundFormat :: SoundFmt
soundFormat = SoundFmt {
                        sampleFmt   = SampleFmtLinear16BitSignedLE,
                        sampleFreq  = 8000,
                        numChannels = 1
                       }

main :: IO ()
main = do args <- getArgs
          case args of 
            [file] -> record file
            _      -> do hPutStrLn stderr "Usage: record <file.pcm>"
                         exitFailure

record :: FilePath -> IO ()
record file =
    do let source = alsaSoundSource "plughw:0,0" soundFormat
           sink   = fileSoundSink file soundFormat
       copySound source sink bufSize

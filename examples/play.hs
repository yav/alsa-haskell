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
            [file] -> play file
            _      -> do hPutStrLn stderr "Usage: play <file.pcm>"
                         exitFailure

play :: FilePath -> IO ()
play file =
    do let source = fileSoundSource file soundFormat
           sink   = alsaSoundSink "plughw:0,0" soundFormat
       copySound source sink bufSize

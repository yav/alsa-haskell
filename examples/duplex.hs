import Sound.Alsa

import Control.Concurrent
import System.Environment
import System.Exit
import System.IO

bufSize :: Int
bufSize = 4096

soundFormat :: SoundFmt
soundFormat = SoundFmt {
                        sampleFmt   = SampleFmtLinear16BitSignedLE,
                        sampleFreq  = 8000,
                        numChannels = 1
                       }

main :: IO ()
main = do args <- getArgs
          case args of 
            [infile,outfile] -> duplex infile outfile
            _                -> 
                do hPutStrLn stderr "Usage: duplex <input.pcm> <output.pcm>"
                   exitFailure

duplex :: FilePath -> FilePath -> IO ()
duplex infile outfile = 
    do forkOS (play infile)
       forkOS (record outfile)
       threadDelay 5000000
       return ()


play :: FilePath -> IO ()
play file =
    do let source = fileSoundSource file soundFormat
           sink   = alsaSoundSink "plughw:0,0" soundFormat
       copySound source sink bufSize

record :: FilePath -> IO ()
record file =
    do let source = alsaSoundSource "plughw:0,0" soundFormat
           sink   = fileSoundSink file soundFormat
       copySound source sink bufSize

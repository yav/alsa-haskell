import Sound.Alsa

import Foreign
import Data.Word
import System.Environment
import System.Exit
import System.IO

bufSize :: Int
bufSize = 910

inputFormat :: SoundFmt
inputFormat = SoundFmt {
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
    do let src = alsaSoundSource "plughw:0,0" inputFormat
       h <- soundSourceOpen src
       fh <- openBinaryFile file WriteMode
       allocaBytes bufSize $ loop src h fh bufSize
       hClose fh
       soundSourceClose src h

loop :: SoundSource h -> h -> Handle -> Int -> Ptr () -> IO ()
loop src h fh n buf =
    do n' <- soundSourceRead src h buf n
       hPutBuf fh buf n'
       loop src h fh n buf

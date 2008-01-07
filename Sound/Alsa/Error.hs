{-#  LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving,
              DeriveDataTypeable
  #-}
module Sound.Alsa.Error where

import Control.Exception (catchDyn,throwDyn)
import Data.Typeable
import Foreign.C.Error
import Foreign.C.String

data AlsaException =
  AlsaException { exception_location    :: String
                , exception_description :: String
                , exception_code        :: Errno
                } deriving (Typeable)

checkResult :: Integral a => String -> a -> IO a
checkResult f r
  | r < 0 = throwAlsa f (Errno (negate (fromIntegral r)))
  | otherwise = return r

checkResult_ :: Integral a => String -> a -> IO ()
checkResult_ f r = checkResult f r >> return ()

throwAlsa :: String -> Errno -> IO a
throwAlsa fun err = do d <- strerror err
                       throwDyn AlsaException
                         { exception_location = fun
                         , exception_description = d
                         , exception_code = err
                         }

catchAlsa :: IO a -> (AlsaException -> IO a) -> IO a
catchAlsa = catchDyn

catchAlsaErrno :: Errno
               -> IO a -- ^ Action
               -> IO a -- ^ Handler
               -> IO a
catchAlsaErrno e x h =
    catchAlsa x (\ex -> if exception_code ex == e then h else throwDyn ex)

catchXRun :: IO a -- ^ Action
          -> IO a -- ^ Handler
          -> IO a
catchXRun = catchAlsaErrno ePIPE

showAlsaException :: AlsaException -> String
showAlsaException e = exception_location e ++ ": " ++ exception_description e

-- | Converts any 'AlsaException' into an 'IOError'.
-- This produces better a error message than letting an uncaught
-- 'AlsaException' propagate to the top.
rethrowAlsaExceptions :: IO a -> IO a
rethrowAlsaExceptions x =
    catchAlsa x $ \e ->
       ioError (errnoToIOError (exception_location e)
                               (exception_code e) Nothing Nothing)

-- | Returns the message for an error code.
strerror :: Errno -> IO String
strerror x = peekCString =<< snd_strerror x

foreign import ccall "alsa/asoundlib.h snd_strerror"
  snd_strerror :: Errno -> IO CString


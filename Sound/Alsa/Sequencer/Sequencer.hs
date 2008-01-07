{-| PRIVATE MODULE
Reference:
<http://www.alsa-project.org/alsa-doc/alsa-lib/group___sequencer.html>
-}

module Sound.Alsa.Sequencer.Sequencer where


import Foreign.C.Types(CInt,CSize)
import Foreign.C.String(CString,withCAString,peekCString)
import Foreign.Ptr(Ptr)
import Foreign.Marshal.Alloc(alloca)
import Foreign.Storable

import Data.Word

import Sound.Alsa.Sequencer.Marshal
import Sound.Alsa.Sequencer.Errors


-- | Creates a new handle and opens a connection to the kernel sequencer
-- interface. After a client is created successfully,
-- a 'ClientStart' event is broadcast to the announce port.
-- May throw an exception.
-- See also: 'open_lconf', 'close', 'get_seq_type',
--   'get_seq_name', 'set_blocking', 'get_client_id'.

open
  :: String     -- ^ The sequencer's \"name\". This is not a name that you
                -- make up for your own purposes; it has special significance
                -- to the ALSA library. Usually you need to pass 'default_name'
                -- here.
  -> OpenMode   -- Read\/Write permissions
  -> BlockMode  -- Blocking behavior
  -> IO SndSeq  -- Handle to the sequencer.

open t om bm = alloca $ \p -> withCAString t $ \s ->
  do check_error_ =<< snd_seq_open p s (exp_OpenMode om) (exp_BlockMode bm)
     SndSeq `fmap` peek p

foreign import ccall "alsa/asoundlib.h snd_seq_open"
  snd_seq_open :: Ptr (Ptr SndSeq_) -> CString -> CInt -> CInt -> IO CInt


-- | Close the sequencer. Closes the sequencer client and releases its
-- resources. After a client is closed, an event with 'ClientExit' is
-- broadcast to announce port. The connection between other clients are
-- disconnected. Call this just before exiting your program.
-- NOTE: we could put this in a finalizer for the handle?

close
  :: SndSeq   -- ^ handle to the sequencer
  -> IO ()
close (SndSeq s) = check_error_ =<< snd_seq_close s

foreign import ccall "alsa/asoundlib.h snd_seq_close"
  snd_seq_close :: Ptr SndSeq_ -> IO CInt


-- | Get identifier of a sequencer handle.
-- It is the same identifier specified in the call to 'open'.
get_seq_name
  :: SndSeq     -- ^ sequencer handle
  -> IO String  -- ^ ALSA identifier for the handel
get_seq_name (SndSeq h) = peekCString =<< snd_seq_name h

foreign import ccall "alsa/asoundlib.h snd_seq_name"
  snd_seq_name :: Ptr SndSeq_ -> IO CString


-- | Change the blocking mode of the given client.
-- In block mode, the client falls into sleep when it fills the output
-- pool with events, or when it demands events from an empty input pool.
-- memory pool with full events. Clients that are sleeping due to
-- loack of space in the output pool are woken when a certain
-- amount of free space becomes available (see 'set_output_room').
set_blocking
  :: SndSeq     -- ^ sequencer handle
  -> BlockMode  -- ^ blocking mode
  -> IO ()
set_blocking (SndSeq h) m = check_error_ =<< snd_seq_nonblock h(exp_BlockMode m)

foreign import ccall "alsa/asoundlib.h snd_seq_nonblock"
  snd_seq_nonblock :: Ptr SndSeq_ -> CInt -> IO CInt



-- Buffers ---------------------------------------------------------------------

-- | Return the byte size of the output buffer.
get_output_buffer_size
  :: SndSeq   -- ^ Sequencer handle.
  -> IO Word  -- ^ Size of output buffer in bytes.

get_output_buffer_size (SndSeq h) =
  fromIntegral `fmap` snd_seq_get_output_buffer_size h

foreign import ccall "alsa/asoundlib.h snd_seq_get_output_buffer_size"
  snd_seq_get_output_buffer_size :: Ptr SndSeq_ -> IO CSize


-- | Resize of the output buffer.
-- This function clears all output events (see 'drop_output').
set_output_buffer_size
  :: SndSeq   -- ^ Sequencer handle.
  -> Word     -- ^ New buffer size in bytes.
  -> IO ()

set_output_buffer_size (SndSeq h) x =
  check_error_ =<< snd_seq_set_output_buffer_size h (fromIntegral x)

foreign import ccall "alsa/asoundlib.h snd_seq_set_output_buffer_size"
  snd_seq_set_output_buffer_size :: Ptr SndSeq_ -> CSize -> IO CInt


-- | Return the byte size of input buffer.
get_input_buffer_size
  :: SndSeq   -- ^ Sequencer handle.
  -> IO Word  -- ^ Size of input buffer in bytes.

get_input_buffer_size (SndSeq h) =
  fromIntegral `fmap` snd_seq_get_input_buffer_size h

foreign import ccall "alsa/asoundlib.h snd_seq_get_input_buffer_size"
   snd_seq_get_input_buffer_size :: Ptr SndSeq_ -> IO CSize


-- | Resize the input buffer.
-- This function clears all input events (see 'drop_input').
set_input_buffer_size
  :: SndSeq   -- ^ Sequencer handle.
  -> Word     -- ^ New byffer size in bytes.
  -> IO ()

set_input_buffer_size (SndSeq h) x =
  check_error_ =<< snd_seq_set_input_buffer_size h (fromIntegral x)

foreign import ccall "alsa/asoundlib.h snd_seq_set_input_buffer_size"
  snd_seq_set_input_buffer_size :: Ptr SndSeq_ -> CSize -> IO CInt



-- Pool management -------------------------------------------------------------

-- | Resize the output memory pool.
set_pool_output
  :: SndSeq   -- ^ Sequencer handle.
  -> Word     -- ^ New size in bytes.
  -> IO ()

set_pool_output (SndSeq h) x =
  check_error_ =<< snd_seq_set_client_pool_output h (fromIntegral x)

foreign import ccall "alsa/asoundlib.h snd_seq_set_client_pool_output"
  snd_seq_set_client_pool_output :: Ptr SndSeq_ -> CSize -> IO CInt


-- | Specify how much space should become free before waking clients
-- that are blocked due to a lack of space in the output pool.
set_pool_output_room
  :: SndSeq   -- ^ Sequencer handle.
  -> Word     -- ^ Number of bytes need to wake up.
  -> IO ()

set_pool_output_room (SndSeq h) x =
  check_error_ =<< snd_seq_set_client_pool_output_room h (fromIntegral x)

foreign import ccall "alsa/asoundlib.h snd_seq_set_client_pool_output_room"
  snd_seq_set_client_pool_output_room :: Ptr SndSeq_ -> CSize -> IO CInt


-- | Reset the output pool.
reset_pool_output
  :: SndSeq   -- ^ Sequencer handle.
  -> IO ()

reset_pool_output (SndSeq h) =
  check_error_ =<< snd_seq_reset_pool_output h

foreign import ccall "alsa/asoundlib.h snd_seq_reset_pool_output"
  snd_seq_reset_pool_output :: Ptr SndSeq_ -> IO CInt



-- | Resize the input memory pool.
set_pool_input
  :: SndSeq   -- ^ Sequencer handle.
  -> Word     -- ^ New size in bytes.
  -> IO ()

set_pool_input (SndSeq h) x =
  check_error_ =<< snd_seq_set_client_pool_input h (fromIntegral x)

foreign import ccall "alsa/asoundlib.h snd_seq_set_client_pool_input"
  snd_seq_set_client_pool_input :: Ptr SndSeq_ -> CSize -> IO CInt


-- | Reset the input pool.
reset_pool_input
  :: SndSeq   -- ^ Sequencer handle.
  -> IO ()

reset_pool_input (SndSeq h) =
  check_error_ =<< snd_seq_reset_pool_input h

foreign import ccall "alsa/asoundlib.h snd_seq_reset_pool_input"
  snd_seq_reset_pool_input :: Ptr SndSeq_ -> IO CInt









--Middle Level Interface -------------------------------------------------------


-- | Simple subscription (w\/o exclusive & time conversion).
connect_from :: SndSeq -> Port -> Addr -> IO ()
connect_from (SndSeq h) me a =
  check_error_ =<< snd_seq_connect_from h (exp_Port me) c p
  where (c,p) = exp_Addr a

foreign import ccall "alsa/asoundlib.h snd_seq_connect_from"
  snd_seq_connect_from :: Ptr SndSeq_ -> CInt -> CInt -> CInt -> IO CInt


-- | Simple subscription (w\/o exclusive & time conversion).
connect_to :: SndSeq -> Port -> Addr -> IO ()
connect_to (SndSeq h) me a =
  check_error_ =<< snd_seq_connect_to h (exp_Port me) c p
  where (c,p) = exp_Addr a

foreign import ccall "alsa/asoundlib.h snd_seq_connect_to"
  snd_seq_connect_to :: Ptr SndSeq_ -> CInt -> CInt -> CInt -> IO CInt


-- | Simple disconnection.
disconnect_from :: SndSeq -> Port -> Addr -> IO ()
disconnect_from (SndSeq h) me a =
  check_error_ =<< snd_seq_disconnect_from h (exp_Port me) c p
  where (c,p) = exp_Addr a

foreign import ccall "alsa/asoundlib.h snd_seq_disconnect_from"
  snd_seq_disconnect_from :: Ptr SndSeq_ -> CInt -> CInt -> CInt -> IO CInt

-- | Simple disconnection.
disconnect_to :: SndSeq -> Port -> Addr -> IO ()
disconnect_to (SndSeq h) me a =
  check_error_ =<< snd_seq_disconnect_to h (exp_Port me) c p
  where (c,p) = exp_Addr a

foreign import ccall "alsa/asoundlib.h snd_seq_disconnect_to"
  snd_seq_disconnect_to :: Ptr SndSeq_ -> CInt -> CInt -> CInt -> IO CInt


-- | Parse the given string into sequencer address.
-- The client and port are separated by either colon or period, e.g. 128:1.
-- The function also accepts client names.
parse_address
  :: SndSeq   -- ^ Sequencer handle.
  -> String   -- ^ String to be parsed.
  -> IO Addr  -- ^ The parsed address.

parse_address (SndSeq h) s =
  alloca $ \pa ->
  withCAString s $ \ps ->
    do check_error =<< snd_seq_parse_address h pa ps
       peek pa

foreign import ccall "alsa/asoundlib.h snd_seq_parse_address"
  snd_seq_parse_address :: Ptr SndSeq_ -> Ptr Addr -> CString -> IO CInt



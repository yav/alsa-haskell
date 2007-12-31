{-| PRIVATE MODULE
Reference:
<http://www.alsa-project.org/alsa-doc/alsa-lib/group___sequencer.html>
-}

{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.Alsa.Sequencer.Sequencer where


import Foreign.C.Types(CInt,CSize)
import Foreign.C.String(CString,withCAString,peekCString)
import Foreign.Ptr(Ptr,nullPtr)
import Foreign.Marshal.Alloc(alloca)
import Foreign.Storable

import Data.Word
import Data.Int

import Sound.Alsa.Sequencer.Types
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


-- Queues ----------------------------------------------------------------------


-- | Allocate a queue.
alloc_queue
  :: SndSeq   -- ^ Sequencer handle.
  -> IO Queue -- ^ Queue identifier.
alloc_queue (SndSeq h) =
  imp_Queue `fmap` (check_error =<< snd_seq_alloc_queue h)

foreign import ccall "alsa/asoundlib.h snd_seq_alloc_queue"
  snd_seq_alloc_queue :: Ptr SndSeq_ -> IO CInt


-- | Allocate a queue with the specified name,
alloc_named_queue
  :: SndSeq   -- ^ Sequencer handle.
  -> String   -- ^ Name for the queue.
  -> IO Queue -- ^ Queue identifier.
alloc_named_queue (SndSeq h) x = withCAString x $ \s ->
  imp_Queue `fmap` (check_error =<< snd_seq_alloc_named_queue h s)

foreign import ccall "alsa/asoundlib.h snd_seq_alloc_named_queue"
  snd_seq_alloc_named_queue :: Ptr SndSeq_ -> CString -> IO CInt


-- | Delete the specified queue.
free_queue
  :: SndSeq   -- ^ Sequencer handle.
  -> Queue    -- ^ Queue identifier.
  -> IO ()
free_queue (SndSeq h) q =
  check_error_ =<< snd_seq_free_queue h (exp_Queue q)

foreign import ccall "alsa/asoundlib.h snd_seq_free_queue"
  snd_seq_free_queue :: Ptr SndSeq_ -> CInt -> IO CInt


-- | Wait until all events of the client are processed.
sync_output_queue
  :: SndSeq   -- ^ Sequencer handle.
  -> IO ()
sync_output_queue (SndSeq h) =
  check_error_ =<< snd_seq_sync_output_queue h

foreign import ccall "alsa/asoundlib.h snd_seq_sync_output_queue"
  snd_seq_sync_output_queue :: Ptr SndSeq_ -> IO CInt



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



-- Event functions -------------------------------------------------------------

-- | Get an event from the input buffer.
-- If the input buffer is empty, then it is filled with data from the
-- sequencer queue.  If there is no data in the sequencer queue,
-- then the process is either put to sleep (if the sequencer is operating
-- in blocking mode), or we throw @EAGAIN@ (if the sequence is operating
-- in non-blocking mode).
--
-- We may also throw @ENOSPC@, which means that the sequencer queue
-- over-run and some events were lost (this clears the input buffer).
--
event_input
  :: SndSeq   -- ^ handle to the sequencer
  -> IO Event -- ^ the event that was retrieved

event_input (SndSeq h) = alloca $ \p ->
  do check_error =<< snd_seq_event_input h p
     peek =<< peek p

foreign import ccall "alsa/asoundlib.h snd_seq_event_input"
  snd_seq_event_input :: Ptr SndSeq_ -> Ptr (Ptr Event) -> IO CInt


-- | Returns the number of events in the input buffer.
-- If the input buffer is empty and the boolean argumen is true,
-- then try to fill the input buffer with data from the sequencer queue.
-- See also: 'snd_seq_event_input'.

event_input_pending
  :: SndSeq   -- ^ sequencer handle
  -> Bool     -- ^ refill if empty?
  -> IO Word  -- ^ number of events in buffer
event_input_pending (SndSeq h) fill =
  check_error =<< snd_seq_event_input_pending h (if fill then 1 else 0)

foreign import ccall "alsa/asoundlib.h snd_seq_event_input_pending"
  snd_seq_event_input_pending :: Ptr SndSeq_ -> Int -> IO CInt



-- | Output an event and dain the buffer, if it bacame full.
-- Thorws exceptions.
-- See also: 'event_output_direct', 'event_output_buffer',
--           'event_output_pending', 'drain_output', 'drop_output',
--           'extract_output', 'remove_events'

event_output :: SndSeq    -- ^ sequencer handle
             -> Event     -- ^ event to be output
             -> IO Word   -- ^ the number of remaining events (or bytes?)
event_output (SndSeq h) e =
  alloca_ev e $ \p -> check_error =<< snd_seq_event_output h p

foreign import ccall "alsa/asoundlib.h snd_seq_event_output "
  snd_seq_event_output :: Ptr SndSeq_ -> Ptr Event -> IO CInt



-- | Output an event without draining the buffer.
-- Throws @-EAGAIN@ if the buffer becomes full.
-- See also 'event_output'.

event_output_buffer :: SndSeq   -- ^ sequencer handle
                    -> Event    -- ^ event to be output
                    -> IO Word  -- ^ the byte size of remaining events

event_output_buffer (SndSeq h) e =
  alloca_ev e $ \p -> check_error =<< snd_seq_event_output_buffer h p

foreign import ccall "alsa/asoundlib.h snd_seq_event_output_buffer"
  snd_seq_event_output_buffer :: Ptr SndSeq_ -> Ptr Event -> IO CInt


-- | Output an event directly to the sequencer, NOT through the output buffer.
-- If an error occurs, then we throw an exception.
-- See also 'event_output'.

event_output_direct
  :: SndSeq   -- ^ sequencer handle
  -> Event    -- ^ event to be output
  -> IO Word  -- ^ number of bytes sent to the sequencer

event_output_direct (SndSeq h) e =
  alloca_ev e $ \p -> check_error =<< snd_seq_event_output_direct h p

foreign import ccall "alsa/asoundlib.h snd_seq_event_output_direct"
  snd_seq_event_output_direct :: Ptr SndSeq_ -> Ptr Event -> IO CInt


-- | Return the size (in bytes) of pending events on output buffer.
-- See also 'snd_seq_event_output'.
event_output_pending
  :: SndSeq   -- ^ sequencer handle
  -> IO Word  -- ^ size of pending events (in bytes)
event_output_pending (SndSeq h) =
  fromIntegral `fmap` snd_seq_event_output_pending h

foreign import ccall "alsa/asoundlib.h snd_seq_event_output_pending"
  snd_seq_event_output_pending :: Ptr SndSeq_ -> IO CInt


-- | Extract the first event in output buffer.
-- Throws an exception on error.
-- See also 'snd_seq_event_output'.
extract_output
  :: SndSeq     -- ^ sequencer handle
  -> IO Event   -- ^ the first event in the buffer (if one was present)
extract_output (SndSeq h) =
  alloca $ \p -> do check_error =<< snd_seq_extract_output h p
                    peek =<< peek p

-- | Remove the first event in output buffer.
-- Throws an exception on error.
-- See also 'snd_seq_event_output'.
remove_output
  :: SndSeq     -- ^ sequencer handle
  -> IO ()
remove_output (SndSeq h) = check_error_ =<< snd_seq_extract_output h nullPtr

foreign import ccall "alsa/asoundlib.h snd_seq_extract_output"
  snd_seq_extract_output :: Ptr SndSeq_ -> Ptr (Ptr Event) -> IO CInt


-- | Drain output buffer to sequencer.
-- This function drains all pending events on the output buffer.
-- The function returns immediately after the events are sent to the queues
-- regardless whether the events are processed or not.
-- To get synchronization with the all event processes,
-- use 'sync_output_queue' after calling this function.
-- Throws an exception on error.
-- See also: 'event_output', 'sync_output_queue'.

drain_output
  :: SndSeq  -- ^ sequencer handle
  -> IO Word -- ^ byte size of events remaining in the buffer.

drain_output (SndSeq h) = check_error =<< snd_seq_drain_output h

foreign import ccall "alsa/asoundlib.h snd_seq_drain_output"
  snd_seq_drain_output :: Ptr SndSeq_ -> IO CInt


-- | Remove events from both the user-space output buffer,
-- and the kernel-space sequencer queue.
-- See also: 'drain_output', 'drop_output_buffer', 'remove_events'.
drop_output
  :: SndSeq -- ^ sequencer handle
  -> IO ()
drop_output (SndSeq h) = check_error_ =<< snd_seq_drop_output h

foreign import ccall "alsa/asoundlib.h snd_seq_drop_output"
  snd_seq_drop_output :: Ptr SndSeq_ -> IO CInt


-- | Remove events from the user-space output buffer.
-- See also: 'drop_output'.
drop_output_buffer
  :: SndSeq -- ^ sequencer handle
  -> IO ()
drop_output_buffer (SndSeq h) = check_error_ =<< snd_seq_drop_output_buffer h

foreign import ccall "alsa/asoundlib.h snd_seq_drop_output_buffer"
  snd_seq_drop_output_buffer :: Ptr SndSeq_ -> IO CInt


-- | Remove events from both the user-space input buffer,
-- and the kernel-space sequencer queue.
-- See also: 'drop_input_buffer', 'remove_events'.
drop_input
  :: SndSeq -- ^ sequencer handle
  -> IO ()
drop_input (SndSeq h) = check_error_ =<< snd_seq_drop_input h

foreign import ccall "alsa/asoundlib.h snd_seq_drop_input"
  snd_seq_drop_input :: Ptr SndSeq_ -> IO CInt


-- | Remove events from the user-space input buffer.
-- See also: 'drop_input'.
drop_input_buffer
  :: SndSeq -- ^ sequencer handle
  -> IO ()
drop_input_buffer (SndSeq h) = check_error_ =<< snd_seq_drop_input_buffer h

foreign import ccall "alsa/asoundlib.h snd_seq_drop_input_buffer"
  snd_seq_drop_input_buffer :: Ptr SndSeq_ -> IO CInt



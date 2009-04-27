--------------------------------------------------------------------------------
-- |
-- Module    : Sound.Alsa.Sequencer.Event
-- Copyright : (c) Iavor S. Diatchki, 2007
-- License   : BSD3
--
-- Maintainer: Iavor S. Diatchki
-- Stability : provisional
--
-- This module contains functions for working with events.
-- Reference:
-- <http://www.alsa-project.org/alsa-doc/alsa-lib/group___seq_event.html>
--------------------------------------------------------------------------------

module Sound.Alsa.Sequencer.Event
  ( sync_output_queue     -- :: SndSeq -> IO ()
  , event_input           -- :: SndSeq -> IO Event
  , event_input_pending   -- :: SndSeq -> Bool -> IO Word
  , event_output          -- :: SndSeq -> Event -> IO Word
  , event_output_buffer   -- :: SndSeq -> Event -> IO Word
  , event_output_direct   -- :: SndSeq -> Event -> IO Word
  , event_output_pending  -- :: SndSeq -> IO Word
  , extract_output        -- :: SndSeq -> IO Event
  , remove_output         -- :: SndSeq -> IO ()
  , drain_output          -- :: SndSeq -> IO Word
  , drop_output           -- :: SndSeq -> IO ()
  , drop_output_buffer    -- :: SndSeq -> IO ()
  , drop_input            -- :: SndSeq -> IO ()
  , drop_input_buffer     -- :: SndSeq -> IO ()
  ) where


import Foreign.C.Types(CInt)
import Foreign.Ptr(Ptr,nullPtr)
import Foreign.Marshal.Alloc(alloca)
import Foreign.Storable

import Data.Word
import Data.Int

import Sound.Alsa.Sequencer.Marshal
import Sound.Alsa.Sequencer.Errors


-- | Wait until all events of the client are processed.
sync_output_queue :: SndSeq -> IO ()
sync_output_queue (SndSeq h) =
  check_error_ =<< snd_seq_sync_output_queue h

foreign import ccall "alsa/asoundlib.h snd_seq_sync_output_queue"
  snd_seq_sync_output_queue :: Ptr SndSeq_ -> IO CInt


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
event_input :: SndSeq -> IO Event
event_input (SndSeq h) = alloca $ \p ->
  do check_error =<< snd_seq_event_input h p
     peek =<< peek p

foreign import ccall "alsa/asoundlib.h snd_seq_event_input"
  snd_seq_event_input :: Ptr SndSeq_ -> Ptr (Ptr Event) -> IO CInt


-- | Returns the number of events in the input buffer.
-- If the input buffer is empty and the boolean argument is true,
-- then try to fill the input buffer with data from the sequencer queue.
-- See also: 'snd_seq_event_input'.

event_input_pending
  :: SndSeq
  -> Bool     -- ^ refill if empty?
  -> IO Word  -- ^ number of events in buffer
event_input_pending (SndSeq h) fill =
  check_error =<< snd_seq_event_input_pending h (if fill then 1 else 0)

foreign import ccall "alsa/asoundlib.h snd_seq_event_input_pending"
  snd_seq_event_input_pending :: Ptr SndSeq_ -> Int -> IO CInt



-- | Output an event and drain the buffer, if it became full.
-- Throws exceptions.
-- See also: 'event_output_direct', 'event_output_buffer',
--           'event_output_pending', 'drain_output', 'drop_output',
--           'extract_output', 'remove_events'

event_output :: SndSeq
             -> Event
             -> IO Word   -- ^ the number of remaining events (or bytes?)
event_output (SndSeq h) e =
  alloca_ev e $ \p -> check_error =<< snd_seq_event_output h p

foreign import ccall "alsa/asoundlib.h snd_seq_event_output "
  snd_seq_event_output :: Ptr SndSeq_ -> Ptr Event -> IO CInt



-- | Output an event without draining the buffer.
-- Throws @-EAGAIN@ if the buffer becomes full.
-- See also 'event_output'.

event_output_buffer :: SndSeq
                    -> Event
                    -> IO Word  -- ^ the byte size of remaining events

event_output_buffer (SndSeq h) e =
  alloca_ev e $ \p -> check_error =<< snd_seq_event_output_buffer h p

foreign import ccall "alsa/asoundlib.h snd_seq_event_output_buffer"
  snd_seq_event_output_buffer :: Ptr SndSeq_ -> Ptr Event -> IO CInt


-- | Output an event directly to the sequencer, NOT through the output buffer.
-- If an error occurs, then we throw an exception.
-- See also 'event_output'.

event_output_direct
  :: SndSeq
  -> Event
  -> IO Word  -- ^ number of bytes sent to the sequencer

event_output_direct (SndSeq h) e =
  alloca_ev e $ \p -> check_error =<< snd_seq_event_output_direct h p

foreign import ccall "alsa/asoundlib.h snd_seq_event_output_direct"
  snd_seq_event_output_direct :: Ptr SndSeq_ -> Ptr Event -> IO CInt


-- | Return the size (in bytes) of pending events on output buffer.
-- See also 'snd_seq_event_output'.
event_output_pending
  :: SndSeq
  -> IO Word  -- ^ size of pending events (in bytes)
event_output_pending (SndSeq h) =
  fromIntegral `fmap` snd_seq_event_output_pending h

foreign import ccall "alsa/asoundlib.h snd_seq_event_output_pending"
  snd_seq_event_output_pending :: Ptr SndSeq_ -> IO CInt


-- | Extract the first event in output buffer.
-- Throws an exception on error.
-- See also 'snd_seq_event_output'.
extract_output
  :: SndSeq
  -> IO Event   -- ^ the first event in the buffer (if one was present)
extract_output (SndSeq h) =
  alloca $ \p -> do check_error =<< snd_seq_extract_output h p
                    peek =<< peek p

-- | Remove the first event in output buffer.
-- Throws an exception on error.
-- See also 'snd_seq_event_output'.
remove_output :: SndSeq -> IO ()
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
  :: SndSeq
  -> IO Word -- ^ byte size of events remaining in the buffer.

drain_output (SndSeq h) = check_error =<< snd_seq_drain_output h

foreign import ccall "alsa/asoundlib.h snd_seq_drain_output"
  snd_seq_drain_output :: Ptr SndSeq_ -> IO CInt


-- | Remove events from both the user-space output buffer,
-- and the kernel-space sequencer queue.
-- See also: 'drain_output', 'drop_output_buffer', 'remove_events'.
drop_output :: SndSeq -> IO ()
drop_output (SndSeq h) = check_error_ =<< snd_seq_drop_output h

foreign import ccall "alsa/asoundlib.h snd_seq_drop_output"
  snd_seq_drop_output :: Ptr SndSeq_ -> IO CInt


-- | Remove events from the user-space output buffer.
-- See also: 'drop_output'.
drop_output_buffer :: SndSeq -> IO ()
drop_output_buffer (SndSeq h) = check_error_ =<< snd_seq_drop_output_buffer h

foreign import ccall "alsa/asoundlib.h snd_seq_drop_output_buffer"
  snd_seq_drop_output_buffer :: Ptr SndSeq_ -> IO CInt


-- | Remove events from both the user-space input buffer,
-- and the kernel-space sequencer queue.
-- See also: 'drop_input_buffer', 'remove_events'.
drop_input :: SndSeq -> IO ()
drop_input (SndSeq h) = check_error_ =<< snd_seq_drop_input h

foreign import ccall "alsa/asoundlib.h snd_seq_drop_input"
  snd_seq_drop_input :: Ptr SndSeq_ -> IO CInt


-- | Remove events from the user-space input buffer.
-- See also: 'drop_input'.
drop_input_buffer :: SndSeq -> IO ()
drop_input_buffer (SndSeq h) = check_error_ =<< snd_seq_drop_input_buffer h

foreign import ccall "alsa/asoundlib.h snd_seq_drop_input_buffer"
  snd_seq_drop_input_buffer :: Ptr SndSeq_ -> IO CInt



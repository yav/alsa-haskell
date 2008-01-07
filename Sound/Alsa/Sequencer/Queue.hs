--------------------------------------------------------------------------------
-- |
-- Module    : Sound.Alsa.Sequencer.Queue
-- Copyright : (c) Iavor S. Diatchki, 2007
-- License   : BSD3
--
-- Maintainer: Iavor S. Diatchki
-- Stability : provisional
--
-- This module contains functions for working with sequencer queue.
-- Reference:
-- <http://www.alsa-project.org/alsa-doc/alsa-lib/group___seq_queue.html>
--------------------------------------------------------------------------------

module Sound.Alsa.Sequencer.Queue
  ( -- * General Queue Functions
    Queue
  , queue_direct
  , alloc_queue
  , alloc_named_queue
  , free_queue

  -- * Queue Information
  , QueueInfo
  , queue_info_copy
  , queue_info_clone

  , queue_info_get_name
  , queue_info_get_locked
  , queue_info_get_owner
  , queue_info_get_flags

  , queue_info_set_name
  , queue_info_set_locked
  , queue_info_set_owner
  , queue_info_set_flags

  -- * Queue Status
  , QueueStatus
  , queue_status_copy
  , queue_status_clone


  -- * Queue Tempo
  , QueueTempo
  , queue_tempo_copy
  , queue_tempo_clone


  -- * Queue Timer
  , QueueTimer
  , queue_timer_copy
  , queue_timer_clone
  , QueueTimerType(..)
  ) where

import Sound.Alsa.Sequencer.Area
import Sound.Alsa.Sequencer.Marshal
import Sound.Alsa.Sequencer.Errors

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

data QueueTimerType = TimerAlsa
                    | TimerMidiClock
                    | TimerMidiTick

alloc_queue :: SndSeq -> IO Queue -- ^ Queue identifier.
alloc_queue (SndSeq h) =
  imp_Queue `fmap` (check_error =<< snd_seq_alloc_queue h)

foreign import ccall "alsa/asoundlib.h snd_seq_alloc_queue"
  snd_seq_alloc_queue :: Ptr SndSeq_ -> IO CInt


alloc_named_queue :: SndSeq -> String -> IO Queue
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



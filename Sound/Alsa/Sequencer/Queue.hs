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
  , get_queue_info
  , set_queue_info
  , queue_info_copy
  , queue_info_clone

  , queue_info_get_queue
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
  , get_queue_status
  , queue_status_copy
  , queue_status_clone


  -- * Queue Tempo
  , QueueTempo
  , get_queue_tempo
  , set_queue_tempo
  , queue_tempo_copy
  , queue_tempo_clone

  , queue_tempo_get_queue
  , queue_tempo_get_tempo
  , queue_tempo_get_ppq
  , queue_tempo_get_skew
  , queue_tempo_get_skew_base

  , queue_tempo_set_tempo
  , queue_tempo_set_ppq
  , queue_tempo_set_skew
  , queue_tempo_set_skew_base

  -- * Queue Timer
  , QueueTimer
  , get_queue_timer
  , set_queue_timer
  , queue_timer_copy
  , queue_timer_clone

  , queue_timer_get_queue
  , queue_timer_get_type
  , queue_timer_get_resolution

  , queue_timer_set_type
  , queue_timer_set_resolution

  , QueueTimerType(..)
  ) where

import Sound.Alsa.Sequencer.Area
import Sound.Alsa.Sequencer.Marshal
import Sound.Alsa.Sequencer.Errors

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

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


-- XXX: Lots of duplication.

get_queue_info :: SndSeq -> Queue -> IO QueueInfo
get_queue_info (SndSeq h) q =
  do info <- queue_info_malloc
     check_error =<< with_queue_info info
                                     (snd_seq_get_queue_info h (exp_Queue q))
     return info

foreign import ccall "alsa/asoundlib.h snd_seq_get_queue_info"
  snd_seq_get_queue_info :: Ptr SndSeq_ -> CInt -> Ptr QueueInfo_ -> IO CInt


set_queue_info :: SndSeq -> Queue -> QueueInfo -> IO ()
set_queue_info (SndSeq h) p info =
  check_error_ =<< with_queue_info info (snd_seq_set_queue_info h (exp_Queue p))

foreign import ccall "alsa/asoundlib.h snd_seq_set_queue_info"
  snd_seq_set_queue_info :: Ptr SndSeq_ -> CInt -> Ptr QueueInfo_ -> IO CInt



get_queue_tempo :: SndSeq -> Queue -> IO QueueTempo
get_queue_tempo (SndSeq h) q =
  do tempo <- queue_tempo_malloc
     check_error =<< with_queue_tempo tempo
                                     (snd_seq_get_queue_tempo h (exp_Queue q))
     return tempo

foreign import ccall "alsa/asoundlib.h snd_seq_get_queue_tempo"
  snd_seq_get_queue_tempo :: Ptr SndSeq_ -> CInt -> Ptr QueueTempo_ -> IO CInt


set_queue_tempo :: SndSeq -> Queue -> QueueTempo -> IO ()
set_queue_tempo (SndSeq h) p tempo =
  check_error_ =<< with_queue_tempo tempo (snd_seq_set_queue_tempo h (exp_Queue p))

foreign import ccall "alsa/asoundlib.h snd_seq_set_queue_tempo"
  snd_seq_set_queue_tempo :: Ptr SndSeq_ -> CInt -> Ptr QueueTempo_ -> IO CInt





get_queue_status :: SndSeq -> Queue -> IO QueueStatus
get_queue_status (SndSeq h) q =
  do status <- queue_status_malloc
     check_error =<< with_queue_status status
                                     (snd_seq_get_queue_status h (exp_Queue q))
     return status

foreign import ccall "alsa/asoundlib.h snd_seq_get_queue_status"
  snd_seq_get_queue_status :: Ptr SndSeq_ -> CInt -> Ptr QueueStatus_ -> IO CInt


get_queue_timer :: SndSeq -> Queue -> IO QueueTimer
get_queue_timer (SndSeq h) q =
  do timer <- queue_timer_malloc
     check_error =<< with_queue_timer timer
                                     (snd_seq_get_queue_timer h (exp_Queue q))
     return timer

foreign import ccall "alsa/asoundlib.h snd_seq_get_queue_timer"
  snd_seq_get_queue_timer :: Ptr SndSeq_ -> CInt -> Ptr QueueTimer_ -> IO CInt


set_queue_timer :: SndSeq -> Queue -> QueueTimer -> IO ()
set_queue_timer (SndSeq h) p timer =
  check_error_ =<< with_queue_timer timer (snd_seq_set_queue_timer h (exp_Queue p))

foreign import ccall "alsa/asoundlib.h snd_seq_set_queue_timer"
  snd_seq_set_queue_timer :: Ptr SndSeq_ -> CInt -> Ptr QueueTimer_ -> IO CInt



------------------------------------------------------------------
-- |
-- Module    : Sound.Alsa.Sequencer
-- Copyright : (c) Iavor S. Diatchki, 2007
-- License   : BSD3
--
-- Maintainer: Iavor S. Diatchki
-- Stability : provisional
--
-- Overview: <http://www.alsa-project.org/alsa-doc/alsa-lib/seq.html>
--
-- WARNING: This whole library does not seem to be particlarly thread aware.
-- Perhaps place the sequencer handle in an MVar?

module Sound.Alsa.Sequencer
  ( -- * Sequencer
    SndSeq
  , OpenMode(..)
  , BlockMode(..)

  , open
  , close
  , default_seq_name
  , get_seq_name
  , set_blocking

   -- ** Manage user-space buffers
  , get_output_buffer_size
  , set_output_buffer_size
  , get_input_buffer_size
  , set_input_buffer_size

  -- ** Manage kernel-space memory pools
  , set_pool_output
  , set_pool_output_room
  , reset_pool_output
  , set_pool_input
  , reset_pool_input


  -- * Queue Interface
  , Queue
  , queue_direct
  , alloc_queue
  , alloc_named_queue
  , free_queue

  -- * Client Interface
  , module Sound.Alsa.Sequencer.Client

  -- * Port Interface
  , module Sound.Alsa.Sequencer.Port


  -- ** Connections
  , connect_from
  , connect_to
  , disconnect_from
  , disconnect_to

  -- * Events
  , volume_same
  , module Sound.Alsa.Sequencer.Event

    -- ** Types
  , RealTime(..)
  , TimeStamp(..)
  , InstrCluster
  , Instr(..)

  , Event(..)
  , EventData(..)
  , NoteEv(..), Note(..), simple_note
  , CtrlEv(..), Ctrl(..)
  , AddrEv(..), Addr(..), parse_address, addr_subscribers
  , ConnEv(..), Connect
  , EmptyEv(..)

  , Sample(..)
  , Cluster(..)
  , Volume(..)


  -- * Error handling
  , AlsaException
  , exception_code, exception_description
  , alsa_catch
  ) where

import Data.Word
import Data.Int

import Sound.Alsa.Sequencer.Types
import Sound.Alsa.Sequencer.Errors
import Sound.Alsa.Sequencer.Sequencer
import Sound.Alsa.Sequencer.Client
import Sound.Alsa.Sequencer.Port
import Sound.Alsa.Sequencer.Event



-- | The address of all subscribed ports.
addr_subscribers :: Addr
addr_subscribers = Addr { addr_client = client_subscribers
                        , addr_port   = port_unknown
                        }


-- | This is the name that should be passed to 'open' in most cases.
default_seq_name :: String
default_seq_name = "default"



-- | Make a note whose unspecified fields contain 0.
simple_note
  :: Word8  -- ^ Channel.
  -> Word8  -- ^ Note.
  -> Word8  -- ^ Velocity.
  -> Note
simple_note c n v = Note { note_channel = c, note_note = n, note_velocity = v
                         , note_off_velocity = 0, note_duration = 0
                         }


-- | Used for volume control: means do not change the valume.
volume_same :: Int16
volume_same = -1




--------------------------------------------------------------------------------
-- |
-- Module    : Sound.Alsa.Sequencer.Client
-- Copyright : (c) Iavor S. Diatchki, 2007
-- License   : BSD3
--
-- Maintainer: Iavor S. Diatchki
-- Stability : provisional
--
-- This module contains functions for working with sequencer clients.
-- Reference:
-- <http://www.alsa-project.org/alsa-doc/alsa-lib/group___seq_client.html>
--------------------------------------------------------------------------------

module Sound.Alsa.Sequencer.Client
  ( Client                            -- :: *
  , client_unknown                    -- :: Client
  , client_system                     -- :: Client
  , client_subscribers                -- :: Client
  , client_broadcast                  -- :: Client

  , get_client_id                     -- :: SndSeq -> IO Client
  , set_client_name                   -- :: SndSeq -> String -> IO ()

  , ClientInfo                        -- :: *
  , ClientType(..)                    -- :: *

  , get_client_info                   -- :: SndSeq -> IO ClientInfo
  , get_any_client_info               -- :: SndSeq -> Client -> IO ClientInfo
  , query_first_client                -- :: SndSeq -> IO ClientInfo
  , query_next_client                 -- :: SndSeq -> ClientInfo -> IO Bool
  , set_client_info                   -- :: SndSeq -> ClientInfo -> IO ()

  , client_info_copy                  -- :: ClientInfo -> ClientInfo -> IO ()
  , client_info_clone                 -- :: ClientInfo -> IO ClientInfo

  , client_info_get_client            -- :: ClientInfo -> IO Client
  , client_info_get_type              -- :: ClientInfo -> IO ClientType
  , client_info_get_name              -- :: ClientInfo -> IO String
  , client_info_get_broadcast_filter  -- :: ClientInfo -> IO Bool
  , client_info_get_error_bounce      -- :: ClientInfo -> IO Bool
  , client_info_get_num_ports         -- :: ClientInfo -> IO Word
  , client_info_get_event_lost        -- :: ClientInfo -> IO Word

  , client_info_set_client            -- :: ClientInfo -> Client -> IO ()
  , client_info_set_name              -- :: ClientInfo -> String -> IO ()
  , client_info_set_broadcast_filter  -- :: ClientInfo -> Bool -> IO ()
  , client_info_set_error_bounce      -- :: ClientInfo -> Bool -> IO ()
  ) where

import Foreign.C.Types(CInt)
import Foreign.Ptr(Ptr)

import Control.Monad(guard)

import Sound.Alsa.Sequencer.Marshal
import Sound.Alsa.Sequencer.Area
import Sound.Alsa.Sequencer.Errors

-- XXX: Still missing the pool interface.

-- Convinience functions -------------------------------------------------------
-- These are part of the "middle" interface, but it seems simple to
-- define them directly in Haskell.

-- | Get the client identifier for the sequencer.
-- A convinience function.
get_client_id :: SndSeq -> IO Client
get_client_id h = client_info_get_client =<< get_client_info h

-- | Set the name for the sequencer client.
-- A convinience function.
set_client_name :: SndSeq -> String -> IO ()
set_client_name h s =
  do i <- get_client_info h
     client_info_set_name i s
     set_client_info h i


--------------------------------------------------------------------------------

-- | Create a new information area filled with data about the sequencer client.
get_client_info :: SndSeq -> IO ClientInfo
get_client_info (SndSeq h) =
  do info <- client_info_malloc
     check_error =<< with_client_info info (snd_seq_get_client_info h)
     return info

foreign import ccall "alsa/asoundlib.h snd_seq_get_client_info"
  snd_seq_get_client_info :: Ptr SndSeq_ -> Ptr ClientInfo_ -> IO CInt


-- | Create a new information area filled with data about an arbitrary client.
get_any_client_info :: SndSeq -> Client -> IO ClientInfo
get_any_client_info (SndSeq h) c =
  do info <- client_info_malloc
     check_error =<< with_client_info info
                        (snd_seq_get_any_client_info h (exp_Client c))
     return info

foreign import ccall "alsa/asoundlib.h snd_seq_get_any_client_info"
  snd_seq_get_any_client_info
    :: Ptr SndSeq_ -> CInt -> Ptr ClientInfo_ -> IO CInt



query_first_client  :: SndSeq -> IO ClientInfo
query_first_client h =
  do x <- client_info_malloc
     with_client_info x (`snd_seq_client_info_set_client` (-1))
     b <- query_next_client h x
     -- XXX: check that we did get the first client (should be System)
     return x


-- | Get information about the client with the next biggest identifier.
query_next_client  :: SndSeq -> ClientInfo
                    -> IO Bool  -- ^ Was there a next client?
query_next_client (SndSeq h) info =
  check_error' (const True)
               (\x -> guard (x == -2) >> return False)
      =<< with_client_info info (snd_seq_query_next_client h)

foreign import ccall "alsa/asoundlib.h snd_seq_query_next_client"
  snd_seq_query_next_client :: Ptr SndSeq_ -> Ptr ClientInfo_ -> IO CInt


-- | Set the information for the sequencer client based on the data
-- in the given information area.
set_client_info :: SndSeq -> ClientInfo -> IO ()
set_client_info (SndSeq h) info =
  check_error_ =<< with_client_info info (snd_seq_set_client_info h)

foreign import ccall "alsa/asoundlib.h snd_seq_set_client_info"
  snd_seq_set_client_info :: Ptr SndSeq_ -> Ptr ClientInfo_ -> IO CInt



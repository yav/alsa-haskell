{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
------------------------------------------------------------------
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
  , query_next_client                 -- :: SndSeq -> ClientInfo -> IO ()
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

import Foreign
import Foreign.C.Types(CInt)
import Foreign.C.String(CString,withCAString,peekCString)
import Foreign.Ptr(Ptr)
import Foreign.Marshal.Alloc(alloca)
import Foreign.Storable
import Data.Word

import Sound.Alsa.Sequencer.Types
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

-- | An object used for getting\/setting client information.
newtype ClientInfo = ClientInfo (ForeignPtr ClientInfo_) deriving Eq
data ClientInfo_

with_info_ptr :: ClientInfo -> (Ptr ClientInfo_ -> IO a) -> IO a
with_info_ptr (ClientInfo p) f = withForeignPtr p f




-- | Allocate an uninitiazlied client area. (Not exported)
client_info_malloc :: IO ClientInfo
client_info_malloc = alloca $ \p ->
  do check_error =<< snd_seq_client_info_malloc p
     ClientInfo `fmap` (newForeignPtr snd_seq_client_info_free =<< peek p)

foreign import ccall "alsa/asoundlib.h snd_seq_client_info_malloc"
  snd_seq_client_info_malloc :: Ptr (Ptr ClientInfo_) -> IO CInt

foreign import ccall "alsa/asoundlib.h &snd_seq_client_info_free"
   snd_seq_client_info_free :: FunPtr (Ptr ClientInfo_ -> IO ())


-- | Copy the content of one information area into another.
client_info_copy
  :: ClientInfo   -- ^ Destination
  -> ClientInfo   -- ^ Source
  -> IO ()

client_info_copy to from =
  with_info_ptr to $ \p1 ->
  with_info_ptr from $ \p2 ->
    snd_seq_client_info_copy p1 p2

foreign import ccall "alsa/asoundlib.h snd_seq_client_info_copy"
  snd_seq_client_info_copy:: Ptr ClientInfo_ -> Ptr ClientInfo_ -> IO ()


-- | Copy the content of an information area into a new area.
client_info_clone :: ClientInfo -> IO ClientInfo
client_info_clone from =
  do to <- client_info_malloc
     client_info_copy to from
     return to

-- | Create a new information area filled with data about the sequencer client.
get_client_info :: SndSeq -> IO ClientInfo
get_client_info (SndSeq h) =
  do info <- client_info_malloc
     check_error =<< with_info_ptr info (snd_seq_get_client_info h)
     return info

foreign import ccall "alsa/asoundlib.h snd_seq_get_client_info"
  snd_seq_get_client_info :: Ptr SndSeq_ -> Ptr ClientInfo_ -> IO CInt


-- | Create a new information area filled with data about an arbitrary client.
get_any_client_info :: SndSeq -> Client -> IO ClientInfo
get_any_client_info (SndSeq h) c =
  do info <- client_info_malloc
     check_error =<< with_info_ptr info
                        (snd_seq_get_any_client_info h (exp_Client c))
     return info

foreign import ccall "alsa/asoundlib.h snd_seq_get_any_client_info"
  snd_seq_get_any_client_info
    :: Ptr SndSeq_ -> CInt -> Ptr ClientInfo_ -> IO CInt


-- | Get information about the client with the next biggest identifier.
-- If such a client exists, that its information is stored in the
-- given area, otherwise we throw an error.
query_next_client  :: SndSeq -> ClientInfo -> IO ()
query_next_client (SndSeq h) info =
  check_error_ =<< with_info_ptr info (snd_seq_query_next_client h)

foreign import ccall "alsa/asoundlib.h snd_seq_query_next_client"
  snd_seq_query_next_client :: Ptr SndSeq_ -> Ptr ClientInfo_ -> IO CInt


-- | Set the information for the sequencer client based on the data
-- in the given information area.
set_client_info :: SndSeq -> ClientInfo -> IO ()
set_client_info (SndSeq h) info =
  check_error_ =<< with_info_ptr info (snd_seq_set_client_info h)

foreign import ccall "alsa/asoundlib.h snd_seq_set_client_info"
  snd_seq_set_client_info :: Ptr SndSeq_ -> Ptr ClientInfo_ -> IO CInt


-- | Get the client identifier of the client.
client_info_get_client :: ClientInfo -> IO Client
client_info_get_client i =
  (imp_Client . fromIntegral)
      `fmap` with_info_ptr i snd_seq_client_info_get_client

foreign import ccall "alsa/asoundlib.h snd_seq_client_info_get_client"
  snd_seq_client_info_get_client :: Ptr ClientInfo_ -> IO CInt

-- | Get the type of the client.
client_info_get_type :: ClientInfo -> IO ClientType
client_info_get_type i =
  imp_ClientType `fmap` with_info_ptr i snd_seq_client_info_get_type

foreign import ccall "alsa/asoundlib.h snd_seq_client_info_get_type"
  snd_seq_client_info_get_type :: Ptr ClientInfo_ -> IO CInt

-- | Get the name of the client.
client_info_get_name :: ClientInfo -> IO String
client_info_get_name i =
  peekCString =<< with_info_ptr i snd_seq_client_info_get_name

foreign import ccall "alsa/asoundlib.h snd_seq_client_info_get_name"
  snd_seq_client_info_get_name :: Ptr ClientInfo_ -> IO CString

-- | Does client accept broadcast messages?
client_info_get_broadcast_filter :: ClientInfo -> IO Bool
client_info_get_broadcast_filter i =
  (1 ==) `fmap` with_info_ptr i snd_seq_client_info_get_broadcast_filter

foreign import ccall "alsa/asoundlib.h snd_seq_client_info_get_broadcast_filter"
  snd_seq_client_info_get_broadcast_filter :: Ptr ClientInfo_ -> IO CInt

-- | Is error-bouncing enabled?
client_info_get_error_bounce :: ClientInfo -> IO Bool
client_info_get_error_bounce i =
  (1 ==) `fmap` with_info_ptr i snd_seq_client_info_get_error_bounce

foreign import ccall "alsa/asoundlib.h snd_seq_client_info_get_error_bounce"
  snd_seq_client_info_get_error_bounce :: Ptr ClientInfo_ -> IO CInt

-- | The number of open ports on the client.
client_info_get_num_ports :: ClientInfo -> IO Word
client_info_get_num_ports i =
  fromIntegral `fmap` with_info_ptr i snd_seq_client_info_get_num_ports

foreign import ccall "alsa/asoundlib.h snd_seq_client_info_get_num_ports"
  snd_seq_client_info_get_num_ports :: Ptr ClientInfo_ -> IO CInt

-- | The number of lost events for the client.
client_info_get_event_lost :: ClientInfo -> IO Word
client_info_get_event_lost i =
  fromIntegral `fmap` with_info_ptr i snd_seq_client_info_get_event_lost

foreign import ccall "alsa/asoundlib.h snd_seq_client_info_get_event_lost"
  snd_seq_client_info_get_event_lost :: Ptr ClientInfo_ -> IO CInt


-- | Set the client indetifier of an info structure.
client_info_set_client :: ClientInfo -> Client -> IO ()
client_info_set_client i c =
  with_info_ptr i (`snd_seq_client_info_set_client` exp_Client c)

foreign import ccall "alsa/asoundlib.h snd_seq_client_info_set_client"
  snd_seq_client_info_set_client  :: Ptr ClientInfo_ -> CInt -> IO ()

-- | Set the name of an info structure.
client_info_set_name :: ClientInfo -> String -> IO ()
client_info_set_name i c =
  withCAString c $ \s -> with_info_ptr i (`snd_seq_client_info_set_name` s)

foreign import ccall "alsa/asoundlib.h snd_seq_client_info_set_name"
  snd_seq_client_info_set_name :: Ptr ClientInfo_ -> CString -> IO ()

-- | Specify if broadcast messages should be accepted.
client_info_set_broadcast_filter :: ClientInfo -> Bool -> IO ()
client_info_set_broadcast_filter i c =
  let x = if c then 1 else 0
  in with_info_ptr i (`snd_seq_client_info_set_broadcast_filter` x)

foreign import ccall "alsa/asoundlib.h snd_seq_client_info_set_broadcast_filter"
  snd_seq_client_info_set_broadcast_filter :: Ptr ClientInfo_ -> CInt -> IO ()

-- | Set the error bounce usage of an info structure.
client_info_set_error_bounce :: ClientInfo -> Bool -> IO ()
client_info_set_error_bounce i c =
  let x = if c then 1 else 0
  in with_info_ptr i (`snd_seq_client_info_set_error_bounce` x)

foreign import ccall "alsa/asoundlib.h snd_seq_client_info_set_error_bounce"
  snd_seq_client_info_set_error_bounce :: Ptr ClientInfo_ -> CInt -> IO ()



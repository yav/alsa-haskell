--------------------------------------------------------------------------------
-- |
-- Module    : Sound.Alsa.Sequencer.Port
-- Copyright : (c) Iavor S. Diatchki, 2007
-- License   : BSD3
--
-- Maintainer: Iavor S. Diatchki
-- Stability : provisional
--
-- This module contains functions for working with ports.
-- Reference:
-- <http://www.alsa-project.org/alsa-doc/alsa-lib/group___seq_port.html>
--------------------------------------------------------------------------------

module Sound.Alsa.Sequencer.Port
  ( Port                  -- :: *
  , port_system_timer     -- :: Port
  , port_system_announce  -- :: Port
  , port_unknown          -- :: Port

  , PortCap             -- :: *
  , cap_read            -- :: PortCap
  , cap_write           -- :: PortCap
  , cap_sync_read       -- :: PortCap
  , cap_sync_write      -- :: PortCap
  , cap_duplex          -- :: PortCap
  , cap_subs_read       -- :: PortCap
  , cap_subs_write      -- :: PortCap
  , cap_no_export       -- :: PortCap
  , caps                -- :: [PortCap] -> PortCap

  , PortType            -- :: *
  , type_specific       -- :: PortType
  , type_midi_generic   -- :: PortType
  , type_midi_gm        -- :: PortType
  , type_midi_gs        -- :: PortType
  , type_midi_xg        -- :: PortType
  , type_midi_mt32      -- :: PortType
  , type_midi_gm2       -- :: PortType

  , type_synth         -- :: PortType
  , type_direct_sample -- :: PortType
  , type_sample        -- :: PortType

  , type_hardware      -- :: PortType
  , type_software      -- :: PortType
  , type_synthesizer   -- :: PortType
  , type_port          -- :: PortType
  , type_application   -- :: PortType
  , types              -- :: [PortType] -> PortType

  , PortInfo            -- :: *

  , create_port         -- :: SndSeq -> PortInfo -> IO ()
  , create_simple_port  -- :: SndSeq -> String -> PortCap -> PortType -> IO Port
  , delete_port         -- :: SndSeq -> Port -> IO ()

  , get_port_info       -- :: SndSeq -> IO PortInfo
  , get_any_port_info   -- :: SndSeq -> Client -> Port -> IO PortInfo
  , query_first_port    -- :: SndSeq -> IO PortInfo
  , query_next_port     -- :: SndSeq -> PortInfo -> IO ()
  , set_port_info       -- :: SndSeq -> Port -> PortInfo -> IO ()

  , port_info_copy      -- :: PortInfo -> PortInfo -> IO ()
  , port_info_clone     -- :: PortInfo -> IO PortInfo

  , port_info_get_port                  -- :: PortInfo -> IO Port
  , port_info_get_client                -- :: PortInfo -> IO Client
  , port_info_get_addr                  -- :: PortInfo -> IO Addr
  , port_info_get_name                  -- :: PortInfo -> IO String
  , port_info_get_capability            -- :: PortInfo -> IO PortCap
  , port_info_get_midi_channels         -- :: PortInfo -> IO Word
  , port_info_get_midi_voices           -- :: PortInfo -> IO Word
  , port_info_get_synth_voices          -- :: PortInfo -> IO Word
  , port_info_get_port_specified        -- :: PortInfo -> IO Bool
  , port_info_get_timestamping          -- :: PortInfo -> IO Bool
  , port_info_get_timestamp_real        -- :: PortInfo -> IO Bool
  , port_info_get_timestamp_queue       -- :: PortInfo -> IO Queue

  , port_info_get_read_use              -- :: PortInfo -> IO Word
  , port_info_get_write_use             -- :: PortInfo -> IO Word

  , port_info_set_port              -- :: PortInfo -> Port -> IO ()
  , port_info_set_client            -- :: PortInfo -> Client -> IO ()
  , port_info_set_addr              -- :: PortInfo -> Addr -> IO ()
  , port_info_set_name              -- :: PortInfo -> String -> IO ()
  , port_info_set_capability        -- :: PortInfo -> PortCap -> IO ()
  , port_info_set_midi_channels     -- :: PortInfo -> Word -> IO ()
  , port_info_set_synth_voices      -- :: PortInfo -> Word -> IO ()
  , port_info_set_midi_voices       -- :: PortInfo -> Word -> IO ()
  , port_info_set_port_specified    -- :: PortInfo -> Bool -> IO ()
  , port_info_set_timestamping      -- :: PortInfo -> Bool -> IO ()
  , port_info_set_timestamp_real    -- :: PortInfo -> Bool -> IO ()
  , port_info_set_timestamp_queue   -- :: PortInfo -> Queue -> IO ()
  ) where

import Foreign.C.Types(CInt,CUInt)
import Foreign.C.String(CString,withCAString)
import Foreign.Ptr(Ptr)
import Foreign.Marshal.Alloc(alloca)
import Foreign.Storable

import Sound.Alsa.Sequencer.Marshal
import Sound.Alsa.Sequencer.Area
import Sound.Alsa.Sequencer.Errors




--------------------------------------------------------------------------------

-- | Create a port - simple version.
create_simple_port :: SndSeq -> String -> PortCap -> PortType -> IO Port
create_simple_port (SndSeq h) s (PortCap c) (PortType t) =
  withCAString s $ \a ->
    imp_Port `fmap` (check_error =<< snd_seq_create_simple_port h a c t)

foreign import ccall "alsa/asoundlib.h snd_seq_create_simple_port"
  snd_seq_create_simple_port :: Ptr SndSeq_ -> CString -> CUInt -> CUInt
                                -> IO CInt




--------------------------------------------------------------------------------

-- | Create a new port, as described by the info structure.
create_port :: SndSeq -> PortInfo -> IO ()
create_port (SndSeq s) p =
  check_error_ =<< with_port_info p (snd_seq_create_port s)

foreign import ccall "alsa/asoundlib.h snd_seq_create_port"
  snd_seq_create_port :: Ptr SndSeq_ -> Ptr PortInfo_ -> IO CInt

-- | Delete the port.
delete_port :: SndSeq -> Port -> IO ()
delete_port (SndSeq h) (Port p) =
  check_error_ =<< snd_seq_delete_port h (fromIntegral p)

foreign import ccall "alsa/asoundlib.h snd_seq_delete_port"
  snd_seq_delete_port :: Ptr SndSeq_ -> CInt -> IO CInt

-- | Create a new information area filled with data about a specific
-- port on our client.
get_port_info :: SndSeq -> Port -> IO PortInfo
get_port_info (SndSeq h) p =
  do info <- port_info_malloc
     check_error =<< with_port_info info (snd_seq_get_port_info h (exp_Port p))
     return info

foreign import ccall "alsa/asoundlib.h snd_seq_get_port_info"
  snd_seq_get_port_info :: Ptr SndSeq_ -> CInt -> Ptr PortInfo_ -> IO CInt


-- | Create a new information area filled with data about an given
-- port on a given client.
get_any_port_info :: SndSeq -> Client -> Port -> IO PortInfo
get_any_port_info (SndSeq h) c p =
  do info <- port_info_malloc
     check_error =<< with_port_info info
                       (snd_seq_get_any_port_info h (exp_Client c) (exp_Port p))
     return info

foreign import ccall "alsa/asoundlib.h snd_seq_get_any_port_info"
  snd_seq_get_any_port_info
    :: Ptr SndSeq_ -> CInt -> CInt -> Ptr PortInfo_ -> IO CInt

-- | Get information about the first port on our client.
query_first_port  :: SndSeq -> IO PortInfo
query_first_port s =
  do x <- port_info_malloc
     with_port_info x (`snd_seq_port_info_set_port` (-1))
     query_next_port s x
     return x


-- | Get information about the port with the next biggest identifier.
-- If a matching port is found, then its information is stored in the
-- given area, otherwise we throw an error.
query_next_port  :: SndSeq -> PortInfo -> IO ()
query_next_port (SndSeq h) info =
   check_error_ =<< with_port_info info (snd_seq_query_next_port h)

foreign import ccall "alsa/asoundlib.h snd_seq_query_next_port"
  snd_seq_query_next_port :: Ptr SndSeq_ -> Ptr PortInfo_ -> IO CInt

-- | Set the information for the sequencer port based on the data
-- in the given information area.
set_port_info :: SndSeq -> Port -> PortInfo -> IO ()
set_port_info (SndSeq h) p info =
  check_error_ =<< with_port_info info (snd_seq_set_port_info h (exp_Port p))

foreign import ccall "alsa/asoundlib.h snd_seq_set_port_info"
  snd_seq_set_port_info :: Ptr SndSeq_ -> CInt -> Ptr PortInfo_ -> IO CInt



-- | Get the address of the information area.
port_info_get_addr :: PortInfo -> IO Addr
port_info_get_addr i =
  peek =<< with_port_info i snd_seq_port_info_get_addr

foreign import ccall "alsa/asoundlib.h snd_seq_port_info_get_addr"
  snd_seq_port_info_get_addr :: Ptr PortInfo_ -> IO (Ptr Addr)


-- | Set the port address.
port_info_set_addr :: PortInfo -> Addr -> IO ()
port_info_set_addr i c =
  alloca $ \p -> poke p c >> with_port_info i (`snd_seq_port_info_set_addr` p)

foreign import ccall "alsa/asoundlib.h snd_seq_port_info_set_addr"
  snd_seq_port_info_set_addr :: Ptr PortInfo_ -> Ptr Addr -> IO ()




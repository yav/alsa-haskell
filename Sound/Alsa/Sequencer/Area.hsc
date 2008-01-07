--------------------------------------------------------------------------------
-- |
-- Module    : Sound.Alsa.Sequencer.Area
-- Copyright : (c) Iavor S. Diatchki, 2007
-- License   : BSD3
--
-- Maintainer: Iavor S. Diatchki
-- Stability : provisional
--
-- PRIVATE MODULE.
--
-- Here we have macros to deal with the various inforamtion
-- areas present in the library.
--------------------------------------------------------------------------------

module Sound.Alsa.Sequencer.Area where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Sound.Alsa.Sequencer.Errors
import Sound.Alsa.Sequencer.Marshal

#{let area x,y =
"data "x"_\n"
"newtype "x" = "x" (ForeignPtr "x"_)\n"
"\n"
"with_"y" :: "x" -> (Ptr "x"_ -> IO a) -> IO a\n"
"with_"y" ("x" p) f = withForeignPtr p f\n"
"\n"
"-- | Allocate an uninitiazlied object. (Not exported)\n"
y"_malloc :: IO "x"\n"
y"_malloc = alloca $ \\p ->\n"
"  do check_error =<< snd_seq_"y"_malloc p\n"
"     "x" `fmap` (newForeignPtr snd_seq_"y"_free =<< peek p)\n"
"\n"
"foreign import ccall \"alsa/asoundlib.h snd_seq_"y"_malloc\"\n"
"  snd_seq_"y"_malloc :: Ptr (Ptr "x"_) -> IO CInt\n"
"\n"
"foreign import ccall \"alsa/asoundlib.h &snd_seq_"y"_free\"\n"
"   snd_seq_"y"_free :: FunPtr (Ptr "x"_ -> IO ())\n"
"\n"
"-- | Copy the content of one object into another.\n"
y"_copy\n"
"  :: "x"     -- ^ Destination\n"
"  -> "x"     -- ^ Source\n"
"  -> IO ()\n"
"\n"
y"_copy to from =\n"
"  with_"y" to $ \\p1 ->\n"
"  with_"y" from $ \\p2 ->\n"
"    snd_seq_"y"_copy p1 p2\n"
"\n"
"foreign import ccall \"alsa/asoundlib.h snd_seq_"y"_copy\"\n"
"  snd_seq_"y"_copy :: Ptr "x"_ -> Ptr "x"_ -> IO ()\n"
"\n"
"-- | Copy the content of an object to a newly created object.\n"
y"_clone :: "x" -> IO "x"\n"
y"_clone from =\n"
"  do to <- "y"_malloc\n"
"     "y"_copy to from\n"
"     return to\n"
}

#{let get_set_name x,y =
y"_get_name :: "x" -> IO String\n"
y"_get_name i = peekCString =<< with_"y" i snd_seq_"y"_get_name\n"
"\n"
"foreign import ccall \"alsa/asoundlib.h snd_seq_"y"_get_name\"\n"
"  snd_seq_"y"_get_name :: Ptr "x"_ -> IO CString\n"
"\n"
y"_set_name :: "x" -> String -> IO ()\n"
y"_set_name i c =\n"
"  withCAString c $ \\p -> with_"y" i (`snd_seq_"y"_set_name` p)\n"
"\n"
"foreign import ccall \"alsa/asoundlib.h snd_seq_"y"_set_name\"\n"
"  snd_seq_"y"_set_name :: Ptr "x"_ -> CString -> IO ()\n"
}

#{let get_set_bool x,y,z =
y"_get_"z" :: "x" -> IO Bool\n"
y"_get_"z" i =\n"
"  (1 ==) `fmap` with_"y" i snd_seq_"y"_get_"z"\n"
"\n"
"foreign import ccall \"alsa/asoundlib.h snd_seq_"y"_get_"z"\"\n"
"  snd_seq_"y"_get_"z" :: Ptr "x"_ -> IO CInt\n"
"\n"
y"_set_"z" :: "x" -> Bool -> IO ()\n"
y"_set_"z" i c =\n"
"  let x = if c then 1 else 0\n"
"  in with_"y" i (`snd_seq_"y"_set_"z"` x)\n"
"\n"
"foreign import ccall \"alsa/asoundlib.h snd_seq_"y"_set_"z"\"\n"
"  snd_seq_"y"_set_"z" :: Ptr "x"_ -> CInt -> IO ()\n"
}

#{let get_int x,y,z,t,mk =
y"_get_"z" :: "x" -> IO "t"\n"
y"_get_"z" i =\n"
"  "mk"\n"
"      `fmap` with_"y" i snd_seq_"y"_get_"z"\n"
"\n"
"foreign import ccall \"alsa/asoundlib.h snd_seq_"y"_get_"z"\"\n"
"  snd_seq_"y"_get_"z" :: Ptr "x"_ -> IO CInt\n"
}

#{let set_int x,y,z,t,brk =
y"_set_"z" :: "x" -> "t" -> IO ()\n"
y"_set_"z" i c =\n"
"  with_"y" i (`snd_seq_"y"_set_"z"` "brk" c)\n"
"\n"
"foreign import ccall \"alsa/asoundlib.h snd_seq_"y"_set_"z"\"\n"
"  snd_seq_"y"_set_"z"  :: Ptr "x"_ -> CInt -> IO ()\n"
}



#area "ClientInfo",  "client_info"

-- read/write
#get_set_name "ClientInfo", "client_info"
#get_set_bool "ClientInfo", "client_info", "broadcast_filter"
#get_set_bool "ClientInfo", "client_info", "error_bounce"
#{get_int "ClientInfo", "client_info", "client",
          "Client", "(imp_Client . fromIntegral)"}
#{set_int "ClientInfo", "client_info", "client",
          "Client", "exp_Client"}

-- read only
#{get_int "ClientInfo", "client_info", "type",
          "ClientType", "imp_ClientType"}
#{get_int "ClientInfo", "client_info", "num_ports",
          "Word", "fromIntegral"}
#{get_int "ClientInfo", "client_info", "event_lost",
          "Word", "fromIntegral"}


#area "PortInfo",    "port_info"

-- read/write
#get_set_name "PortInfo", "port_info"

#get_set_bool "PortInfo", "port_info", "port_specified"
#get_set_bool "PortInfo", "port_info", "timestamping"
#get_set_bool "PortInfo", "port_info", "timestamp_real"

#{get_int "PortInfo", "port_info", "port",
          "Port", "(imp_Port . fromIntegral)"}
#{set_int "PortInfo", "port_info", "port",
          "Port", "exp_Port"}
#{get_int "PortInfo", "port_info", "client",
          "Client","(imp_Client . fromIntegral)"}
#{set_int "PortInfo", "port_info", "client",
          "Client","exp_Client"}
#{get_int "PortInfo", "port_info", "capability",
          "PortCap","(PortCap . fromIntegral)"}
#{set_int "PortInfo", "port_info", "capability",
          "PortCap","(fromIntegral . unPortCap)"}

#{get_int "PortInfo", "port_info", "midi_channels",
          "Word","fromIntegral"}
#{set_int "PortInfo", "port_info", "midi_channels",
          "Word","fromIntegral"}
#{get_int "PortInfo", "port_info", "midi_voices",
          "Word","fromIntegral"}
#{set_int "PortInfo", "port_info", "midi_voices",
          "Word","fromIntegral"}
#{get_int "PortInfo", "port_info", "synth_voices",
          "Word","fromIntegral"}
#{set_int "PortInfo", "port_info", "synth_voices",
          "Word","fromIntegral"}

#{get_int "PortInfo", "port_info", "timestamp_queue",
          "Queue","(imp_Queue . fromIntegral)"}
#{set_int "PortInfo", "port_info", "timestamp_queue",
          "Queue","exp_Queue"}

-- read only
#{get_int "PortInfo", "port_info", "read_use",
          "Word","fromIntegral"}
#{get_int "PortInfo", "port_info", "write_use",
          "Word","fromIntegral"}


#area "QueueInfo",   "queue_info"
#get_set_name "QueueInfo", "queue_info"
#get_set_bool "QueueInfo", "queue_info", "locked"

#{get_int "QueueInfo", "queue_info", "owner",
          "Client", "(imp_Client . fromIntegral)"}
#{set_int "QueueInfo", "queue_info", "owner",
          "Client", "exp_Client"}
#{get_int "QueueInfo", "queue_info", "flags",
          "Word", "fromIntegral"}
#{set_int "QueueInfo", "queue_info", "flags",
          "Word", "fromIntegral"}

#area "QueueStatus", "queue_status"
#area "QueueTempo",  "queue_tempo"
#area "QueueTimer",  "queue_timer"





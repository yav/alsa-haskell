--------------------------------------------------------------------------------
-- |
-- Module    : Sound.Alsa.Sequencer.Marshal
-- Copyright : (c) Iavor S. Diatchki, 2007
-- License   : BSD3
--
-- Maintainer: Iavor S. Diatchki
-- Stability : provisional
--
-- PRIVATE MODULE.
--
-- Here we have the various types used by the library,
-- and how they are imported\/exported to C.
--
-- NOTE: In the translations bellow we make the following assumptions
-- about the sizes of C types.
-- CChar  = 8 bits
-- CShort = 16 bit
-- CInt   = 32 bits
--------------------------------------------------------------------------------

module Sound.Alsa.Sequencer.Marshal where

#include <alsa/asoundlib.h>
import Foreign
import Foreign.C.Types
import Data.Word
import Data.Array

-- | Read\/Write permissions for the sequencer device.
data OpenMode       = OpenOutput  -- ^ Open for writing.
                    | OpenInput   -- ^ Open for reading.
                    | OpenDuplex  -- ^ Open for both reading and writing.
                      deriving (Show,Eq)

exp_OpenMode       :: OpenMode -> CInt
exp_OpenMode x      = case x of
  OpenOutput  -> #{const SND_SEQ_OPEN_OUTPUT}
  OpenInput   -> #{const SND_SEQ_OPEN_INPUT}
  OpenDuplex  -> #{const SND_SEQ_OPEN_DUPLEX}


-- | Blocking behavior of the sequencer device.
data BlockMode      = Block     -- ^ Operations may block.
                    | Nonblock  -- ^ Throw exceptions instead of blocking.
                      deriving (Show,Eq)

exp_BlockMode      :: BlockMode -> CInt
exp_BlockMode x     = case x of
  Block     -> 0
  Nonblock  -> #{const SND_SEQ_NONBLOCK}


-- | The type of sequencer handles.
newtype SndSeq      = SndSeq (Ptr SndSeq_) deriving Eq
data SndSeq_


-- | The type of client identifiers.
newtype Client      = Client Word8 deriving (Show,Eq,Ord,Storable)


#{enum Client, Client
 , client_system      = SND_SEQ_CLIENT_SYSTEM
 , client_subscribers = SND_SEQ_ADDRESS_SUBSCRIBERS
 , client_broadcast   = SND_SEQ_ADDRESS_BROADCAST
 , client_unknown     = SND_SEQ_ADDRESS_UNKNOWN
 }



exp_Client         :: Client -> CInt
exp_Client (Client c) = fromIntegral c

imp_Client         :: Word -> Client
imp_Client p        = Client (fromIntegral p)

-- | The different types of clients.
data ClientType = UserClient | KernelClient

imp_ClientType :: CInt -> ClientType
imp_ClientType x = if x == #{const SND_SEQ_USER_CLIENT} then UserClient
                                                        else KernelClient

-- | Port capabilities.
newtype PortCap     = PortCap { unPortCap :: CUInt } deriving (Eq,Ord)

-- | Port types.
newtype PortType    = PortType { unPortType :: CUInt } deriving (Eq,Ord)

#{enum Port, Port
 , port_system_timer    = SND_SEQ_PORT_SYSTEM_TIMER
 , port_system_announce = SND_SEQ_PORT_SYSTEM_ANNOUNCE
 , port_unknown         = SND_SEQ_ADDRESS_UNKNOWN
 }

#{enum PortCap, PortCap
 , cap_read       = SND_SEQ_PORT_CAP_READ
 , cap_write      = SND_SEQ_PORT_CAP_WRITE
 , cap_sync_read  = SND_SEQ_PORT_CAP_SYNC_READ
 , cap_sync_write = SND_SEQ_PORT_CAP_SYNC_WRITE
 , cap_duplex     = SND_SEQ_PORT_CAP_DUPLEX
 , cap_subs_read  = SND_SEQ_PORT_CAP_SUBS_READ
 , cap_subs_write = SND_SEQ_PORT_CAP_SUBS_WRITE
 , cap_no_export  = SND_SEQ_PORT_CAP_NO_EXPORT
 }

caps               :: [PortCap] -> PortCap
caps cs             = PortCap (foldl (.|.) 0 (map unPortCap cs))

#{enum PortType, PortType
 , type_specific      = SND_SEQ_PORT_TYPE_SPECIFIC
 , type_midi_generic  = SND_SEQ_PORT_TYPE_MIDI_GENERIC
 , type_midi_gm       = SND_SEQ_PORT_TYPE_MIDI_GM
 , type_midi_gs       = SND_SEQ_PORT_TYPE_MIDI_GS
 , type_midi_xg       = SND_SEQ_PORT_TYPE_MIDI_XG
 , type_midi_mt32     = SND_SEQ_PORT_TYPE_MIDI_MT32
 , type_midi_gm2      = SND_SEQ_PORT_TYPE_MIDI_GM2

 , type_synth         = SND_SEQ_PORT_TYPE_SYNTH
 , type_direct_sample = SND_SEQ_PORT_TYPE_DIRECT_SAMPLE
 , type_sample        = SND_SEQ_PORT_TYPE_SAMPLE

 , type_hardware      = SND_SEQ_PORT_TYPE_HARDWARE
 , type_software      = SND_SEQ_PORT_TYPE_SOFTWARE
 , type_synthesizer   = SND_SEQ_PORT_TYPE_SYNTHESIZER
 , type_port          = SND_SEQ_PORT_TYPE_PORT
 , type_application   = SND_SEQ_PORT_TYPE_APPLICATION
 }

types              :: [PortType] -> PortType
types cs            = PortType (foldl (.|.) 0 (map unPortType cs))





-- The type of queue identifiers.
newtype Queue       = Queue Word8 deriving (Show,Eq,Ord,Storable)

imp_Queue          :: Word -> Queue
imp_Queue x         = Queue (fromIntegral x)

exp_Queue          :: Queue -> CInt
exp_Queue (Queue x) = fromIntegral x

#{enum Queue, Queue
 , queue_direct = SND_SEQ_QUEUE_DIRECT
 }

data QueueTimerType = TimerAlsa
                    | TimerMidiClock
                    | TimerMidiTick

exp_QueueTimerType :: QueueTimerType -> CInt
exp_QueueTimerType t  = case t of
  TimerAlsa       -> #{const SND_SEQ_TIMER_ALSA}
  TimerMidiClock  -> #{const SND_SEQ_TIMER_MIDI_CLOCK}
  TimerMidiTick   -> #{const SND_SEQ_TIMER_MIDI_TICK}

imp_QueueTimerType :: CInt -> QueueTimerType
imp_QueueTimerType t  = case t of
  #{const SND_SEQ_TIMER_ALSA}         -> TimerAlsa
  #{const SND_SEQ_TIMER_MIDI_CLOCK}   -> TimerMidiClock
  #{const SND_SEQ_TIMER_MIDI_TICK}    -> TimerMidiTick
  _ -> error ("imp_QueueTimerType: unknown timer type (" ++ show t ++ ")")


-- The type of client ports.
newtype Port        = Port Word8 deriving (Show,Eq,Ord,Storable)

exp_Port           :: Port -> CInt
exp_Port (Port p)   = fromIntegral p

imp_Port           :: Word -> Port
imp_Port p          = Port (fromIntegral p)


data Addr           = Addr { addr_client :: !Client
                           , addr_port   :: !Port
                           } deriving (Show,Eq,Ord)

exp_Addr           :: Addr -> (CInt,CInt)
exp_Addr a          = (exp_Client (addr_client a), exp_Port (addr_port a))


instance Storable Addr where
  sizeOf _    = #size snd_seq_real_time_t
  alignment _ = 4 -- XXX
  peek p      = do cl <- #{peek snd_seq_addr_t, client} p
                   po <- #{peek snd_seq_addr_t, port} p
                   return Addr { addr_client = cl, addr_port = po }
  poke p v    = #{poke snd_seq_addr_t, client} p (addr_client v)
             >> #{poke snd_seq_addr_t, port}   p (addr_port v)


data Connect        = Connect { conn_source :: !Addr
                              , conn_dest   :: !Addr
                              } deriving (Show,Eq,Ord)

instance Storable Connect where
  sizeOf _    = #size snd_seq_connect_t
  alignment _ = 4 -- XXX
  peek p      = do s <- #{peek snd_seq_connect_t, sender} p
                   d <- #{peek snd_seq_connect_t, dest} p
                   return Connect { conn_source = s, conn_dest = d }
  poke p v    = #{poke snd_seq_connect_t, sender} p (conn_source v)
             >> #{poke snd_seq_connect_t, dest}   p (conn_dest v)


-- XXX: to compare these we should first normalize them
data RealTime       = RT { rt_secs :: !Word32
                         , rt_nano :: !Word32
                         } deriving (Show)

instance Storable RealTime where
  sizeOf _    = #{size snd_seq_real_time_t}
  alignment _ = 4 -- XXX
  peek p      = do s <- #{peek snd_seq_real_time_t, tv_sec} p
                   n <- #{peek snd_seq_real_time_t, tv_nsec} p
                   return RT { rt_secs = s, rt_nano = n }
  poke p v    = #{poke snd_seq_real_time_t, tv_sec} p (rt_secs v)
             >> #{poke snd_seq_real_time_t, tv_nsec} p (rt_nano v)


data TimeStamp      = TickTime !Word32
                    | RealTime !RealTime
                      deriving Show

peek_timestamp :: Word8 -> Ptr TimeStamp -> IO TimeStamp
peek_timestamp flags p =
  case flags .&. #{const SND_SEQ_TIME_STAMP_MASK} of
    { #{const SND_SEQ_TIME_STAMP_TICK} -> TickTime `fmap` peek (castPtr p)
    ; _                                -> RealTime `fmap` peek (castPtr p)
    }

poke_timestamp :: Ptr TimeStamp -> TimeStamp -> IO Word8
poke_timestamp p ts = case ts of
  TickTime t -> poke (castPtr p) t >> return #{const SND_SEQ_TIME_STAMP_TICK}
  RealTime t -> poke (castPtr p) t >> return #{const SND_SEQ_TIME_STAMP_REAL}



newtype InstrCluster = InstrCluster CUInt
  deriving (Show,Eq,Ord,Num,Enum,Storable)

data Instr          = Instr { instr_cluster :: !InstrCluster
                             -- XXX: perhaps use Smaple?
                            , instr_std     :: !Word32
                            , instr_bank    :: !Word16
                            , instr_prg     :: !Word16
                            } deriving (Show)

instance Storable Instr where
  sizeOf _    = #{size snd_seq_instr_t}
  alignment _ = 4 -- XXX
  peek p      = do cl <- #{peek snd_seq_instr_t, cluster} p
                   st <- #{peek snd_seq_instr_t, std} p
                   ba <- #{peek snd_seq_instr_t, bank} p
                   pr <- #{peek snd_seq_instr_t, prg} p
                   return Instr { instr_cluster = cl
                                , instr_std     = st
                                , instr_bank    = ba
                                , instr_prg     = pr
                                }
  poke p v    = #{poke snd_seq_instr_t, cluster} p (instr_cluster v)
             >> #{poke snd_seq_instr_t, std}     p (instr_std v)
             >> #{poke snd_seq_instr_t, bank}    p (instr_bank v)
             >> #{poke snd_seq_instr_t, prg}     p (instr_prg v)



data Note           = Note { note_channel      :: !Word8
                           , note_note         :: !Word8
                           , note_velocity     :: !Word8
                           , note_off_velocity :: !Word8
                           , note_duration     :: !Word32
                           } deriving (Show)


instance Storable Note where
  sizeOf _    = #{size snd_seq_ev_note_t}
  alignment _ = 4 -- XXX
  peek p      = do c  <- #{peek snd_seq_ev_note_t, channel} p
                   n  <- #{peek snd_seq_ev_note_t, note} p
                   v  <- #{peek snd_seq_ev_note_t, velocity} p
                   ov <- #{peek snd_seq_ev_note_t, off_velocity} p
                   d  <- #{peek snd_seq_ev_note_t, duration} p
                   return Note { note_channel = c
                               , note_note = n
                               , note_velocity = v
                               , note_off_velocity = ov
                               , note_duration = d
                               }
  poke p v    = #{poke snd_seq_ev_note_t, channel}      p (note_channel v)
             >> #{poke snd_seq_ev_note_t, note}         p (note_note v)
             >> #{poke snd_seq_ev_note_t, velocity}     p (note_velocity v)
             >> #{poke snd_seq_ev_note_t, off_velocity} p (note_off_velocity v)
             >> #{poke snd_seq_ev_note_t, duration}     p (note_duration v)


data Ctrl           = Ctrl { ctrl_channel  :: !Word8
                           , ctrl_param    :: !Word32
                           , ctrl_value    :: !Int32
                           } deriving (Show)

instance Storable Ctrl where
  sizeOf _    = #{size snd_seq_ev_ctrl_t}
  alignment _ = 4 -- XXX
  peek p      = do ct <- #{peek snd_seq_ev_ctrl_t, channel} p
                   pa <- #{peek snd_seq_ev_ctrl_t, param} p
                   va <- #{peek snd_seq_ev_ctrl_t, value} p
                   return Ctrl { ctrl_channel = ct
                               , ctrl_param   = pa
                               , ctrl_value   = va
                               }
  poke p v    = #{poke snd_seq_ev_ctrl_t, channel} p (ctrl_channel v)
             >> #{poke snd_seq_ev_ctrl_t, param}   p (ctrl_param v)
             >> #{poke snd_seq_ev_ctrl_t, value}   p (ctrl_value v)



data Sample         = Sample { sample_std  :: !Word32
                             , sample_bank :: !Word16
                             , sample_prg  :: !Word16
                             } deriving (Show)

instance Storable Sample where
  sizeOf _    = #{size snd_seq_ev_sample_t}
  alignment _ = 4 -- XXX
  peek p      = do st <- #{peek snd_seq_ev_sample_t, std} p
                   ba <- #{peek snd_seq_ev_sample_t, bank} p
                   pr <- #{peek snd_seq_ev_sample_t, prg} p
                   return Sample { sample_std     = st
                                 , sample_bank    = ba
                                 , sample_prg     = pr
                                 }
  poke p v    = #{poke snd_seq_ev_sample_t, std}     p (sample_std v)
             >> #{poke snd_seq_ev_sample_t, bank}    p (sample_bank v)
             >> #{poke snd_seq_ev_sample_t, prg}     p (sample_prg v)


newtype Cluster     = Cluster { cluster_cluster :: InstrCluster
                              } deriving (Show,Eq,Storable)


-- | These are all 14 bit values.
data Volume         = Volume { volume_volume  :: !Int16
                             , volume_lr      :: !Int16
                             , volume_fr      :: !Int16
                             , volume_du      :: !Int16
                             } deriving (Show)


instance Storable Volume where
  sizeOf _    = #{size snd_seq_ev_volume_t}
  alignment _ = 4 -- XXX
  peek p      = do v <- #{peek snd_seq_ev_volume_t, volume} p
                   l <- #{peek snd_seq_ev_volume_t, lr} p
                   f <- #{peek snd_seq_ev_volume_t, fr} p
                   d <- #{peek snd_seq_ev_volume_t, du} p
                   return Volume { volume_volume  = v
                                 , volume_lr      = l
                                 , volume_fr      = f
                                 , volume_du      = d
                                 }
  poke p v    = #{poke snd_seq_ev_volume_t, volume} p (volume_volume v)
             >> #{poke snd_seq_ev_volume_t, lr}     p (volume_lr v)
             >> #{poke snd_seq_ev_volume_t, fr}     p (volume_fr v)
             >> #{poke snd_seq_ev_volume_t, du}     p (volume_du v)



data Event          = Event { ev_high_priority  :: !Bool
                            , ev_tag            :: !Word8
                            , ev_queue          :: !Queue
                            , ev_timestamp      :: !TimeStamp
                            , ev_source         :: !Addr
                            , ev_dest           :: !Addr
                            , ev_data           :: !EventData
                            } deriving Show

instance Storable Event where
  sizeOf _    = #{size snd_seq_event_t}
  alignment _ = 4 -- XXX
  peek p =
    do ty    <- #{peek snd_seq_event_t, type} p
       flags <- #{peek snd_seq_event_t, flags} p
       tag   <- #{peek snd_seq_event_t, tag} p
       q     <- #{peek snd_seq_event_t, queue} p
       time  <- peek_timestamp flags (#{ptr snd_seq_event_t, time} p)
       src   <- #{peek snd_seq_event_t, source} p
       dest  <- #{peek snd_seq_event_t, dest} p
       d     <- (peek_event_data ! ty) (#{ptr snd_seq_event_t, data} p)
       return Event
         { ev_high_priority = (flags .&. #{const SND_SEQ_PRIORITY_MASK}) /= 0
         , ev_tag = tag
         , ev_queue = q
         , ev_timestamp = time
         , ev_source = src
         , ev_dest = dest
         , ev_data = d
         }
  poke p e = do
    { ty <- poke_event_data (#{ptr snd_seq_event_t, data} p) (ev_data e)
    ; #{poke snd_seq_event_t, type} p ty
    ; #{poke snd_seq_event_t, tag} p (ev_tag e)
    ; #{poke snd_seq_event_t, queue} p (ev_queue e)
    ; real <- poke_timestamp (#{ptr snd_seq_event_t, time} p) (ev_timestamp e)
    ; #{poke snd_seq_event_t, source} p (ev_source e)
    ; #{poke snd_seq_event_t, dest} p (ev_dest e)
    ; let flags = (if ev_high_priority e
                     then #{const SND_SEQ_PRIORITY_HIGH}
                     else #{const SND_SEQ_PRIORITY_NORMAL})
               .|. real
               .|. #{const SND_SEQ_EVENT_LENGTH_FIXED}  -- XXX
    ; #{poke snd_seq_event_t, flags} p flags
    }

alloca_ev :: Event -> (Ptr Event -> IO a) -> IO a
alloca_ev e h = alloca (\p -> poke p e >> h p)

poke_event_data :: Ptr EventData -> EventData -> IO Word8
poke_event_data p dt = case dt of
  NoteEv e d  -> poke (castPtr p) d >> return (exp_note_ev e)
  CtrlEv e d  -> poke (castPtr p) d >> return (exp_ctrl_ev e)
  AddrEv e d  -> poke (castPtr p) d >> return (exp_addr_ev e)
  ConnEv e d  -> poke (castPtr p) d >> return (exp_conn_ev e)
  EmptyEv e   -> return (exp_empty_ev e)


peek_event_data :: Array Word8 (Ptr EventData -> IO EventData)
peek_event_data = accumArray (const id) unknown (0,255)
  [ -- result events (2)
    (#{const SND_SEQ_EVENT_SYSTEM}, unknown)
  , (#{const SND_SEQ_EVENT_RESULT}, unknown)

    -- note events (4)
  , (#{const SND_SEQ_EVENT_NOTE},     peek_note_ev ANote)
  , (#{const SND_SEQ_EVENT_NOTEON},   peek_note_ev NoteOn)
  , (#{const SND_SEQ_EVENT_NOTEOFF},  peek_note_ev NoteOff)
  , (#{const SND_SEQ_EVENT_KEYPRESS}, peek_note_ev KeyPress)

    -- control events (12)
  , (#{const SND_SEQ_EVENT_CONTROLLER},  peek_ctrl_ev Controller)
  , (#{const SND_SEQ_EVENT_PGMCHANGE},   peek_ctrl_ev PgmChange)
  , (#{const SND_SEQ_EVENT_CHANPRESS},   peek_ctrl_ev ChanPress)
  , (#{const SND_SEQ_EVENT_PITCHBEND},   peek_ctrl_ev PitchBend)
  , (#{const SND_SEQ_EVENT_CONTROL14},   peek_ctrl_ev Control14)
  , (#{const SND_SEQ_EVENT_NONREGPARAM}, peek_ctrl_ev NonRegParam)
  , (#{const SND_SEQ_EVENT_REGPARAM},    peek_ctrl_ev RegParam)
  , (#{const SND_SEQ_EVENT_SONGPOS},     peek_ctrl_ev SongPos)
  , (#{const SND_SEQ_EVENT_SONGSEL},     peek_ctrl_ev SongSel)
  , (#{const SND_SEQ_EVENT_QFRAME},      peek_ctrl_ev QFrame)
  , (#{const SND_SEQ_EVENT_TIMESIGN},    peek_ctrl_ev TimeSign)
  , (#{const SND_SEQ_EVENT_KEYSIGN},     peek_ctrl_ev KeySign)

  -- queue control (10)
  , (#{const SND_SEQ_EVENT_START}, unknown)
  , (#{const SND_SEQ_EVENT_CONTINUE}, unknown)
  , (#{const SND_SEQ_EVENT_STOP}, unknown)
  , (#{const SND_SEQ_EVENT_SETPOS_TICK}, unknown)
  , (#{const SND_SEQ_EVENT_SETPOS_TIME}, unknown)
  , (#{const SND_SEQ_EVENT_TEMPO}, unknown)
  , (#{const SND_SEQ_EVENT_CLOCK}, unknown)
  , (#{const SND_SEQ_EVENT_TICK}, unknown)
  , (#{const SND_SEQ_EVENT_QUEUE_SKEW}, unknown)
  , (#{const SND_SEQ_EVENT_SYNC_POS}, unknown)

  -- misc (3)
  , (#{const SND_SEQ_EVENT_TUNE_REQUEST}, peek_empty_ev TuneRequest)
  , (#{const SND_SEQ_EVENT_RESET},        peek_empty_ev Reset)
  , (#{const SND_SEQ_EVENT_SENSING},      peek_empty_ev Sensing)

  , (#{const SND_SEQ_EVENT_ECHO}, unknown)
  , (#{const SND_SEQ_EVENT_OSS}, unknown)

  -- networking (8)
  , (#{const SND_SEQ_EVENT_CLIENT_START},  peek_addr_ev ClientStart)
  , (#{const SND_SEQ_EVENT_CLIENT_EXIT},   peek_addr_ev ClientExit)
  , (#{const SND_SEQ_EVENT_CLIENT_CHANGE}, peek_addr_ev ClientChange)
  , (#{const SND_SEQ_EVENT_PORT_START},    peek_addr_ev PortStart)
  , (#{const SND_SEQ_EVENT_PORT_EXIT},     peek_addr_ev PortExit)
  , (#{const SND_SEQ_EVENT_PORT_CHANGE},   peek_addr_ev PortChange)
  , (#{const SND_SEQ_EVENT_PORT_SUBSCRIBED},   peek_conn_ev PortSubscribed)
  , (#{const SND_SEQ_EVENT_PORT_UNSUBSCRIBED}, peek_conn_ev PortUnsubscribed)


  , (#{const SND_SEQ_EVENT_SAMPLE}, unknown)
  , (#{const SND_SEQ_EVENT_SAMPLE_CLUSTER}, unknown)
  , (#{const SND_SEQ_EVENT_SAMPLE_START}, unknown)
  , (#{const SND_SEQ_EVENT_SAMPLE_STOP}, unknown)
  , (#{const SND_SEQ_EVENT_SAMPLE_FREQ}, unknown)
  , (#{const SND_SEQ_EVENT_SAMPLE_VOLUME}, unknown)
  , (#{const SND_SEQ_EVENT_SAMPLE_LOOP}, unknown)
  , (#{const SND_SEQ_EVENT_SAMPLE_POSITION}, unknown)
  , (#{const SND_SEQ_EVENT_SAMPLE_PRIVATE1}, unknown)

  , (#{const SND_SEQ_EVENT_USR0}, unknown)
  , (#{const SND_SEQ_EVENT_USR1}, unknown)
  , (#{const SND_SEQ_EVENT_USR2}, unknown)
  , (#{const SND_SEQ_EVENT_USR3}, unknown)
  , (#{const SND_SEQ_EVENT_USR4}, unknown)
  , (#{const SND_SEQ_EVENT_USR5}, unknown)
  , (#{const SND_SEQ_EVENT_USR6}, unknown)
  , (#{const SND_SEQ_EVENT_USR7}, unknown)
  , (#{const SND_SEQ_EVENT_USR8}, unknown)
  , (#{const SND_SEQ_EVENT_USR9}, unknown)

  , (#{const SND_SEQ_EVENT_INSTR_BEGIN}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_END}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_INFO}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_INFO_RESULT}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_FINFO}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_FINFO_RESULT}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_RESET}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_STATUS}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_STATUS_RESULT}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_PUT}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_GET}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_GET_RESULT}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_FREE}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_LIST}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_LIST_RESULT}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_CLUSTER}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_CLUSTER_GET}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_CLUSTER_RESULT}, unknown)
  , (#{const SND_SEQ_EVENT_INSTR_CHANGE}, unknown)

  , (#{const SND_SEQ_EVENT_SYSEX}, unknown)
  , (#{const SND_SEQ_EVENT_BOUNCE}, unknown)

  , (#{const SND_SEQ_EVENT_USR_VAR0}, unknown)
  , (#{const SND_SEQ_EVENT_USR_VAR1}, unknown)
  , (#{const SND_SEQ_EVENT_USR_VAR2}, unknown)
  , (#{const SND_SEQ_EVENT_USR_VAR3}, unknown)
  , (#{const SND_SEQ_EVENT_USR_VAR3}, unknown)

  , (#{const SND_SEQ_EVENT_NONE}, peek_empty_ev None)
  ]

  where unknown = peek_empty_ev Unknown


data NoteEv   = ANote | NoteOn | NoteOff | KeyPress
                deriving Show

data CtrlEv   = Controller | PgmChange | ChanPress
              | PitchBend | Control14
              | NonRegParam | RegParam
              | SongPos | SongSel
              | QFrame
              | TimeSign | KeySign
                deriving Show

data EmptyEv  = TuneRequest | Reset | Sensing | None | Unknown
                deriving Show

data AddrEv   = ClientStart | ClientExit | ClientChange
              | PortStart | PortExit | PortChange
                deriving Show

data ConnEv   = PortSubscribed | PortUnsubscribed
                deriving Show


exp_note_ev :: NoteEv -> Word8
exp_note_ev e = case e of
  ANote    -> #{const SND_SEQ_EVENT_NOTE}
  NoteOn   -> #{const SND_SEQ_EVENT_NOTEON}
  NoteOff  -> #{const SND_SEQ_EVENT_NOTEOFF}
  KeyPress -> #{const SND_SEQ_EVENT_KEYPRESS}

exp_ctrl_ev :: CtrlEv -> Word8
exp_ctrl_ev e = case e of
  Controller  -> #{const SND_SEQ_EVENT_CONTROLLER}
  PgmChange   -> #{const SND_SEQ_EVENT_PGMCHANGE}
  ChanPress   -> #{const SND_SEQ_EVENT_CHANPRESS}
  PitchBend   -> #{const SND_SEQ_EVENT_PITCHBEND}
  Control14   -> #{const SND_SEQ_EVENT_CONTROL14}
  NonRegParam -> #{const SND_SEQ_EVENT_NONREGPARAM}
  RegParam    -> #{const SND_SEQ_EVENT_REGPARAM}
  SongPos     -> #{const SND_SEQ_EVENT_SONGPOS}
  SongSel     -> #{const SND_SEQ_EVENT_SONGSEL}
  QFrame      -> #{const SND_SEQ_EVENT_QFRAME}
  TimeSign    -> #{const SND_SEQ_EVENT_TIMESIGN}
  KeySign     -> #{const SND_SEQ_EVENT_KEYSIGN}

exp_empty_ev :: EmptyEv -> Word8
exp_empty_ev e = case e of
  TuneRequest -> #{const SND_SEQ_EVENT_TUNE_REQUEST}
  Reset       -> #{const SND_SEQ_EVENT_RESET}
  Sensing     -> #{const SND_SEQ_EVENT_SENSING}
  None        -> #{const SND_SEQ_EVENT_NONE}
  Unknown     -> #{const SND_SEQ_EVENT_NONE}

exp_addr_ev :: AddrEv -> Word8
exp_addr_ev e = case e of
    ClientStart -> #{const SND_SEQ_EVENT_CLIENT_START}
    ClientExit -> #{const SND_SEQ_EVENT_CLIENT_EXIT}
    ClientChange -> #{const SND_SEQ_EVENT_CLIENT_CHANGE}
    PortStart -> #{const SND_SEQ_EVENT_PORT_START}
    PortExit -> #{const SND_SEQ_EVENT_PORT_EXIT}
    PortChange -> #{const SND_SEQ_EVENT_PORT_CHANGE}

exp_conn_ev :: ConnEv -> Word8
exp_conn_ev e = case e of
  PortSubscribed   -> #{const SND_SEQ_EVENT_PORT_SUBSCRIBED}
  PortUnsubscribed -> #{const SND_SEQ_EVENT_PORT_UNSUBSCRIBED}


peek_note_ev :: NoteEv -> Ptr EventData -> IO EventData
peek_note_ev e p = NoteEv e `fmap` peek (castPtr p)

peek_ctrl_ev :: CtrlEv -> Ptr EventData -> IO EventData
peek_ctrl_ev e p = CtrlEv e `fmap` peek (castPtr p)

peek_addr_ev :: AddrEv -> Ptr EventData -> IO EventData
peek_addr_ev e p = AddrEv e `fmap` peek (castPtr p)

peek_conn_ev :: ConnEv -> Ptr EventData -> IO EventData
peek_conn_ev e p = ConnEv e `fmap` peek (castPtr p)

peek_empty_ev :: EmptyEv -> Ptr EventData -> IO EventData
peek_empty_ev e _ = return (EmptyEv e)


data EventData
  = NoteEv NoteEv Note
  | CtrlEv CtrlEv Ctrl
  | AddrEv AddrEv Addr
  | ConnEv ConnEv Connect
  | EmptyEv EmptyEv
    deriving Show



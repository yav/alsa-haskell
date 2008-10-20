import Sound.Alsa.Sequencer

main :: IO ()
main =
  do putStrLn "Starting."
     h <- open default_seq_name open_output Block
     set_client_name h "HS3"
     putStrLn "Created sequencer."
     x <- get_client_id h
     putStrLn ("My id is: " ++ show x)
     let me = Addr { addr_client = x, addr_port = port_unknown }
     -- tgt_addr <- parse_address h "HS1:255"
     let tgt_addr = Addr client_broadcast port_unknown
     print tgt_addr
     getChar
     event_output_direct h (e1 me tgt_addr)
     getChar
     close h
     putStrLn "Closed sequencer."
  `alsa_catch` \e -> putStrLn ("Problem: " ++ exception_description e)




e1 from to = Event
  { ev_high_priority = False
  , ev_tag = 0
  , ev_queue = queue_direct
  , ev_timestamp = TickTime 0
  , ev_source = from
  , ev_dest = to
  , ev_data = NoteEv NoteOn (simple_note 0 60 100)
  }

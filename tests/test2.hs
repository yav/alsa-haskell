import Sound.Alsa.Sequencer

main :: IO ()
main =
  do putStrLn "Starting."
     h <- open default_seq_name OpenOutput Block
     set_client_name h "HS test client"
     putStrLn "Created sequencer."
     p <- create_simple_port h "one"
            (caps [cap_read,cap_subs_read]) type_midi_generic
     putStrLn "Created port."
     x <- get_client_id h
     putStrLn ("My id is: " ++ show x)
     let me = Addr { addr_client = x, addr_port = p }
     getChar
     event_output_direct h (e1 me)
     getChar
     delete_port h p
     putStrLn "Deleted port."
     close h
     putStrLn "Closed sequencer."
  `alsa_catch` \e -> putStrLn ("Problem: " ++ exception_description e)




e1 me = Event
  { ev_high_priority = False
  , ev_tag = 0
  , ev_queue = queue_direct
  , ev_timestamp = TickTime 0
  , ev_source = me
  , ev_dest = addr_subscribers
  , ev_data = NoteEv NoteOn (simple_note 0 60 100)
  }

import Sound.Alsa.Sequencer

main :: IO ()
main =
  do putStrLn "Starting."
     h <- open default_seq_name open_input Block
     set_client_name h "HS1"
     putStrLn "Created sequencer."
     p1 <- create_simple_port h "one"
            (caps [cap_write,cap_subs_write]) type_midi_generic

     p2 <- create_simple_port h "two"
            (caps [cap_write,cap_subs_write]) type_midi_generic
     putStrLn "Created ports."
     let loop = do putStrLn "waiting for an event:"
                   e <- event_input h
                   print e
                   loop
     loop
     delete_port h p1
     delete_port h p2
     putStrLn "Deleted ports."
     close h
     putStrLn "Closed sequencer."
  `alsa_catch` \e -> putStrLn ("Problem: " ++ exception_description e)

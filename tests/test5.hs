import Sound.Alsa.Sequencer

main = do h <- open default_seq_name open_output Block
          set_client_name h "HS5"
          p <- create_simple_port h "1" (caps [cap_read,cap_subs_read])
                                        type_midi_generic
          c <- get_client_id h
          q <- alloc_queue h
          let ev t e = Event
                         { ev_high_priority = False
                         , ev_tag = 0
                         , ev_queue = q
                         , ev_timestamp = TickTime t
                         , ev_source = Addr { addr_client = c, addr_port = p }
                         , ev_dest = addr_subscribers
                         , ev_data = e
                         }
              play t x y z =
                   do print =<< event_output h (ev 0 $ NoteEv NoteOn
                                                            $ simple_note x y z)

                      print =<< event_output h (ev t $ NoteEv NoteOn
                                                            $ simple_note x y 0)
          putStrLn "Please connect me to a synth"
          getChar
          play 4 0 60 127
          drain_output h
          print =<< event_output_pending h

          free_queue h q
          delete_port h p
          close h

  `alsa_catch` \e -> putStrLn $ "alsa_exception: (" ++ show (exception_code e)
                             ++ ") " ++ exception_description e







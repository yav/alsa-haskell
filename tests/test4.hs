import Sound.Alsa.Sequencer
import Control.Monad(when)

main = do h <- open default_seq_name open_output Block
          i <- query_first_client h
          let loop = do putStrLn =<< client_info_get_name i
                        more <- query_next_client h i
                        when more loop
          loop
          close h


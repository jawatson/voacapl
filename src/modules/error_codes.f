      module error_codes
         integer, parameter :: EC_OK          = 0 ! OK
         integer, parameter :: EC_FILE_ERROR  = 1 ! File errors
         integer, parameter :: EC_NUM_ERROR   = 2 ! Numerical errors
         integer, parameter :: EC_EXEC_ERROR  = 3 ! User excecute error    
         integer, parameter :: EC_EOF_ERROR   = 4 ! User excecute error      
         integer, parameter :: EC_READ_ERROR  = 5 ! User excecute error  
         integer, parameter :: EC_ITSHFBC_NOT_FOUND_ERROR  = 6 ! itshfbc file doesn't exist  
         integer, parameter :: EC_INPUT_FILE_NOT_FOUND_ERROR  = 7 ! User excecute error    
         integer, parameter :: EC_INPUT_FILE_READ_ERROR  = 8 ! 
         integer, parameter :: EC_OUTPUT_FILE_OPEN_ERROR  = 9 !    
         integer, parameter :: EC_SCRATCH_FILE_OPEN_ERROR  = 10 !                        
      end module error_codes


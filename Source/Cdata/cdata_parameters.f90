
    module pp_params
    
    use precision_parameters
    save
    
    integer, parameter :: mxpntsarray = 50, mxranddists = 7, mxgenerators = 10, mxvaltypes = 4, mxseeds = 2
    integer, parameter :: idx_uniform = 1, idx_trangle = 2, idx_user_defined_discrete = 3
    integer, parameter :: idx_user_defined_continous_interval = 4, idx_beta = 5, idx_normal = 6
    integer, parameter :: idx_log_normal = 7, mxfields = 10
    character(len=35), parameter :: rand_dist(mxranddists) =  (/'UNIFORM                            ',  &
                                                                'TRIANGLE                           ',  &
                                                                'USER_DEFINED_DISCRETE              ',  &
                                                                'USER_DEFINED_CONTINOUS_INTERVAL    ',  &
                                                                'BETA                               ',  &
                                                                'NORMAL                             ',  &
                                                                'LOG_NORMAL                         '/)
    
    integer, parameter :: idx_real = 1, idx_char = 2, idx_int = 3, idx_logic = 4
    character(len=9), parameter :: val_types(mxvaltypes) = (/'REAL(eb) ', &
                                                            'INTEGER  ', &
                                                            'CHARACTER', &
                                                            'LOGICAL  '/)
    
    integer :: rnd_seeds(2)
    integer :: restart_values(9)
    
    end module pp_params
    
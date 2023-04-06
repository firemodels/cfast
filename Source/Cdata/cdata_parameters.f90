
    module pp_params
    
    use precision_parameters
    
    use cparams, only: mxfires
    save
    
    integer, parameter :: mxpntsarray = 200, mxranddists = 13, mxgenerators = 200, mxvaltypes = 4, mxseeds = 2
    integer, parameter :: mxrndfires = mxfires
    integer, parameter :: mxfiresections = 50, mxrandfires = 50, mxfiregens = 100, mxiterations = 100000
    integer, parameter :: mxstats = 10, mxfirepnts = 20
    integer, parameter :: idx_uniform = 1, idx_triangle = 2, idx_user_defined_discrete = 3
    integer, parameter :: idx_user_defined_continous_interval = 4, idx_beta = 5, idx_normal = 6
    integer, parameter :: idx_log_normal = 7, idx_const = 8, idx_value = mxranddists, idx_linear = 9, mxfields = 1200
    integer, parameter :: idx_trun_normal = 10, idx_trun_log_normal = 11, idx_gamma = 12
    character(len=35), parameter :: rand_dist(mxranddists) =  (/'UNIFORM                            ',  &
                                                                'TRIANGLE                           ',  &
                                                                'USER_DEFINED_DISCRETE              ',  &
                                                                'USER_DEFINED_CONTINOUS_INTERVAL    ',  &
                                                                'BETA                               ',  &
                                                                'NORMAL                             ',  &
                                                                'LOG_NORMAL                         ',  &
                                                                'CONSTANT                           ',  &
                                                                'LINEAR                             ',  &
                                                                'TRUNCATED_NORMAL                   ',  &
                                                                'TRUNCATED_LOG_NORMAL               ',  &
                                                                'GAMMA                              ',  &
                                                                'VALUE                              '/)
    
    integer, parameter :: idx_real = 1, idx_char = 2, idx_int = 3, idx_logic = 4
    character(len=9), parameter :: val_types(mxvaltypes) = (/'REAL     ', &
                                                             'STRING   ', &
                                                             'INTEGER  ', &
                                                             'LOGICAL  '/)
    integer, parameter :: idx_firefiles = 1, idx_stagefires = 2
    character(len=35), parameter :: fire_generator_types(2) = (/'FIRES_FROM_FILES                  ', &
                                                                'MULTI-STAGE_FIRES                 '/)
    
    integer :: rnd_seeds(mxseeds)
    integer :: restart_values(9)
    
    integer, parameter :: mxanalys = 4
    character(len = 20), parameter :: analysis_list(mxanalys) = (/'DECISION_TREE      ', &
                                                                  'CONVERGENCE_OF_MEAN', &
                                                                  'HISTOGRAM          ', &
                                                                  'EMPERICAL_PDF      '/)
    
    integer, parameter :: mximgformats = 5, default_img = 5
    character(len=3), parameter :: imgformatext_list(mximgformats) = (/'jpg', 'svg', 'tif', 'pdf', 'png'/)
    character(len=5), parameter :: imgformat_list(mximgformats) = (/'jpeg', 'svg ', 'tiff', 'pdf ', 'png '/)
    
    integer, parameter :: mxdiags = 20, mxdiagcols = 1000
    
    end module pp_params
    
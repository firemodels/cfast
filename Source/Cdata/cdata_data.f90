!----------------------preprocessor_data-----------------------------------

module ppfilehandeling

    use precision_parameters
    
    implicit none
    save
    
    character(len=1) :: pp_num = '#', pp_digit = '*'
    character(len=128) :: ppinfile_prefix, ppinfile_suffix, ppinfile_numfrm, ppinfile_extension

    end module ppfilehandeling
    
    
    !----------------------------------------montecarlo_data------------------------------------------------
    module montecarlo_data
    
    use precision_parameters
    
    use cfast_types, only: cfast_type
    use preprocessor_types, only: random_generator_type, field_pointer, fire_generator_type
    
    implicit none
    save
    
    integer :: mc_number_of_cases
    character(len=512) :: mc_datapath
    character(len=128) :: mc_filename_pattern
    logical :: mc_write_seeds, validation_output
    integer :: mc_max_iterations
    
    integer :: n_generators
    type (random_generator_type), allocatable, dimension(:), target ::  generatorinfo
    
    integer :: n_fields
    type(field_pointer), allocatable, dimension(:), target :: fieldinfo
    integer, allocatable, dimension(:) :: fieldptr
    
    integer :: n_rndfires
    type (fire_generator_type), allocatable, dimension(:), target :: randfireinfo
    
    type (cfast_type), target :: dummy
    
    character(len=256) :: workpath, parameterfile 
    
    end module montecarlo_data
    
    !-------------------------------------------analysis_data--------------------------------------------    
    module analysis_data
    
    use precision_parameters
    
    use analysis_types, only:stat_type
    
    implicit none
    save
    
    integer :: n_stats
    type(stat_type), allocatable, dimension(:), target :: statinfo
    
    character(len=256) :: outpath
    
    end module analysis_data
    
    !-------------------------------------------diagnostic_data--------------------------------------------    
    module diagnostic_data
    
    use precision_parameters
    
    use preprocessor_types, only: preprocessor_type
    use diagnostic_types, only: diagnostic_type
    
    implicit none
    save
    
    integer :: n_diag
    type(diagnostic_type), allocatable, dimension(:), target :: diaginfo
    
    character(len=256) :: outpath
    
    type(diagnostic_type), pointer :: diagptr
    
    end module diagnostic_data
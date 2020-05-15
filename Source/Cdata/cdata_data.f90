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
    
    use preprocessor_types, only: random_generator_type, field_pointer
    
    implicit none
    save
    
    real(eb) :: mc_number_of_cases
    character(len=512) :: mc_datapath
    character(len=128) :: mc_filename_pattern
    logical :: mc_write_seeds
    
    integer :: n_generators
    type (random_generator_type), allocatable, dimension(:), target ::  generatorinfo
    
    integer :: n_fields
    type (field_pointer), allocatable, dimension(:), target :: fieldinfo
    
    end module montecarlo_data
    
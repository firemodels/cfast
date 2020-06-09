module preprocessor_types

    use precision_parameters
    use pp_params, only: mxpntsarray, idx_uniform, idx_trangle, idx_user_defined_discrete, &
                     idx_user_defined_continous_interval, idx_beta, idx_normal , idx_log_normal, rand_dist, &
                     val_types, idx_real, idx_char, idx_int, idx_logic, mxseeds
    
    use cfast_types, only: cfast_type
    
    intrinsic random_seed, random_number
    
    type preprocessor_type
        character(len=128) :: id = 'NULL'
        
    contains
        procedure errorcall
    end type preprocessor_type
    
    type, extends(preprocessor_type) :: random_generator_type
        
        character(len=128) :: fyi                       ! line available for comments or extra input
        character(len=35) :: type_dist                 ! only accepts a defined list of elements: UNIFORM, DISCRETE_UNIFORM, TRIANGLE, 
                                                        !                       USER_DEFINED_DISCRETE, USER_DEFINED_CONTINOUS_INTERVAL,
                                                        !                       BETA, NORMAL, LOG_NORMAL
        character(len=9) :: value_type                  ! what value type the generator produces: CHARACTER, REAL(eb), INTEGER, LOGICAL
        character(len=128) :: char_array(mxpntsarray)   ! values for discrete distributions with string type
        real(eb) :: real_array(mxpntsarray)             ! values for discrete distributions with real type
        integer :: int_array(mxpntsarray)               ! values for discrete distributions with integer type
        logical :: logic_array(mxpntsarray)             ! values for discrete distributions with logical type
        real(eb) :: prob_array(mxpntsarray)             ! user defined probablities for both descreet and continous 
        real(eb) :: max, min                            ! the ends of the interval used for the random generator
        real(eb) :: mean, stdev                         ! mean and standard deviation for normal distributions. 
        real(eb) :: alpha, beta                         ! for distributions like beta that use those parameters
        real(eb) :: peak                                ! for the triangle distribution where the peak of the triangle occurs
        logical :: first                                ! logical used in conjunction with 
        logical :: use_seeds                            ! determines if seeds have been supplied. 
        integer :: seeds(mxseeds)                       ! seed values
        integer :: base_seeds(mxseeds)                  ! first seeds used 
        real(eb) :: range
        
    contains
        procedure :: rand
    end type random_generator_type
    
    type, extends(preprocessor_type) :: value_wrapper_type
        logical :: add_to_parameters
        logical :: parameter_field_set = .false. 
        character(len=128) :: parameter_header
        character(len=128), pointer :: paramptr
    
    contains
        procedure :: add_header
        procedure :: write_value
    end type value_wrapper_type
    
    type, extends(value_wrapper_type) :: random_char_type
        
        character(len=9) :: value_type = val_types(idx_char)
        character, pointer :: val
    end type random_char_type
    
    type, extends(value_wrapper_type) :: random_real_type
        
        character(len=9) :: value_type = val_types(idx_real)
        real(eb), pointer :: val
    end type random_real_type
    
    type, extends(value_wrapper_type) :: random_int_type
        
        character(len=9) :: value_type = val_types(idx_int)
        integer, pointer :: val
    end type random_int_type
    
    type, extends(value_wrapper_type) :: random_logic_type
        
        character(len=9) :: value_type = val_types(idx_logic)
        logical, pointer :: val
    end type random_logic_type
    
    type, extends(value_wrapper_type) :: field_pointer
        character(len=9) :: value_type = 'NULL'
        class(cfast_type), pointer :: itemptr
        class(value_wrapper_type), pointer :: valptr
        type(random_generator_type), pointer :: genptr
        type(random_char_type) :: charval
        type(random_real_type) :: realval
        type(random_int_type) :: intval
        type(random_logic_type) :: logicval
    end type
    
    contains
    
    subroutine errorcall(me, location, number)
        class (preprocessor_type), intent(in) :: me
        character(len=*), intent(in) :: location
        integer, intent(in) :: number
        
        character(len=256) :: buf
        
        select type (me)
        type is (preprocessor_type) 
            buf = 'preprocessor_type:' // trim(me%id) // ':' // trim(location)
        type is (random_generator_type)
            buf = 'random_generator_type:' // trim(me%id) // ':' // trim(location)
        type is (value_wrapper_type)
            buf = 'value_wrapper_type:' // trim(me%id) // ':' // trim(location)
        type is (random_real_type)
            buf = 'random_real_type:' // trim(me%id) // ':' // trim(location)
        type is (random_char_type)
            buf = 'random_char_type:' // trim(me%id) // ':' // trim(location)
        type is (random_int_type)
            buf = 'random_int_type:' // trim(me%id) // ':' // trim(location)
        type is (random_logic_type)
            buf = 'random_logic_type:' // trim(me%id) // ':' // trim(location)
        type is (field_pointer)
            buf = 'field_pointer:' // trim(me%id) // ':' // trim(location)
        class default
             buf = 'UnKnown_type:' // trim(me%id) // ':' // trim(location)
        end select
        call cfastexit(buf,number)
        
    end subroutine errorcall
        
    
    subroutine rand(me, val)
    
        class(random_generator_type) :: me
        class(value_wrapper_type), intent(inout) :: val
        real(eb) :: x
        integer :: tmpseeds(mxseeds)
        
        if (me%first) then
            me%first = .false.
            if (me%use_seeds) then
                me%seeds = me%base_seeds
            else
                call RANDOM_NUMBER(x)
                call RANDOM_SEED(GET=tmpseeds)
                me%seeds(1) = mod((tmpseeds(1) + 1301)*104179,1073916995)
                me%seeds(2) = mod((tmpseeds(2) + 1303)*104173,1073916995)
                me%seeds(2) = mod((me%seeds(1) + 1301)*104179,1073916995)
                me%seeds(1) = mod((me%seeds(2) + 1303)*104173,1073916995)
                me%base_seeds = me%seeds
            end if
        end if
        if (me%value_type == val_types(idx_real)) then
            if (me%type_dist == rand_dist(idx_uniform)) then
                call RANDOM_SEED(GET=tmpseeds)
                call RANDOM_SEED(PUT=me%seeds)
                call RANDOM_NUMBER(x)
                call RANDOM_SEED(GET=me%seeds)
                call RANDOM_SEED(PUT = tmpseeds)
            else 
                call me%errorcall('RAND', 2)
            end if
        else
            call me%errorcall('RAND', 3)
        end if
        
        select type (val)
        type is (value_wrapper_type)
            call me%errorcall('RAND', 4)
        type is (random_real_type)
            if (me%value_type == val_types(idx_real)) then
                val%val = me%min + x*(me%max-me%min)
            else 
                call me%errorcall('RAND', 5)
            end if
        type is (random_int_type)
            call me%errorcall('RAND', 6)
        type is (random_logic_type)
            call me%errorcall('RAND', 7)
        type is (random_char_type)
            call me%errorcall('RAND', 8)
        class default
            call me%errorcall('RAND', 9)
        end select
        
        return
    end subroutine rand
    
    subroutine add_header(me, icol, array)
    
        class(value_wrapper_type), intent(inout) :: me
        integer, intent(in) :: icol
        character(len=*), intent(out), target :: array(*)
        
        if (me%add_to_parameters) then
            me%paramptr => array(icol)
            array(icol) = trim(me%parameter_header)
            me%parameter_field_set = .true.
        end if 
        
    end subroutine add_header
    
    subroutine write_value(me)
    
        character(len=100) :: buf
    
        class(value_wrapper_type), intent(inout) :: me
    
        if (me%add_to_parameters .and. me%parameter_field_set) then
            select type (me)
            type is (value_wrapper_type)
                write(me%paramptr,'(a)') 'Values not set'
            type is (random_real_type)
                write(me%paramptr,'(e13.6)') me%val
            type is (random_int_type)
                write(me%paramptr,'(i10)') me%val
            type is (random_char_type)
                write(me%paramptr,'(a)') me%val
            type is (random_logic_type)
                if (me%val) then
                    write(me%paramptr,'(a)') '.TRUE.'
                else
                     write(me%paramptr,'(a)') '.FALSE.'
                end if
            type is (field_pointer)
                select case (me%value_type)
                case ('NULL')
                    call me%errorcall('write_value',1)
                case (val_types(idx_real))
                    write(me%paramptr,'(e13.6)') me%realval%val
                case (val_types(idx_int))
                    write(me%paramptr,'(i10)') me%intval%val
                case (val_types(idx_char))
                    write(me%paramptr,'(a)') me%charval%val
                case (val_types(idx_logic))
                    if (me%logicval%val) then
                        write(me%paramptr,'(a)') '.TRUE.'
                    else
                        write(me%paramptr,'(a)') '.FALSE.'
                    end if
                case default
                    call me%errorcall('write_value',2)
                end select
            class default
                call me%errorcall('write_value',3)
            end select
        end if
    end subroutine write_value
    
    end module preprocessor_types
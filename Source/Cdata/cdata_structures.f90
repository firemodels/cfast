module preprocessor_types

    use precision_parameters
    use exit_routines, only: cfastexit
    use pp_params, only: mxpntsarray, idx_uniform, idx_triangle, idx_user_defined_discrete, &
                     idx_user_defined_continous_interval, idx_beta, idx_normal , idx_log_normal, rand_dist, &
                     val_types, idx_real, idx_char, idx_int, idx_logic, mxseeds, idx_const, idx_linear, &
                     idx_trun_normal, idx_trun_log_normal, idx_gamma
    
    use cparams
    use cfast_types, only: cfast_type, fire_type
    
    use random, only: random_normal, random_gamma, random_beta
    
    intrinsic random_seed, random_number
    
    logical :: debug
    character(len=256) :: errormessage
    
    !
    !------preprocessor_type---------
    !
    
    type preprocessor_type                              !Base type all other types extend 
        character(len=128) :: id = 'NULL'               ! Name of a particulear intant of an object
        character(len=128) :: fyi = 'NULL'              ! line available for comments or extra input
    contains
        procedure errorcall
    end type preprocessor_type
    
    !
    !--------value_wrapper_type--------
    !
    
    type, extends(preprocessor_type) :: value_wrapper_type
        logical :: add_to_parameters
        logical :: parameter_field_set = .false. 
        character(len=128) :: parameter_column_label
        character(len=128), pointer :: paramptr
        logical :: pointer_set = .false.
        logical :: temp_flag, kilo_flag
    contains
        procedure :: add_header
        procedure :: write_value
    end type value_wrapper_type
    
    type, extends(value_wrapper_type) :: random_char_type
        character(len=9) :: value_type = val_types(idx_char)
        character(len=128), pointer :: val
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
        character(len=7), dimension(5) :: fld_types = (/'VALUE  ', &
                                                        'INDEX  ', &
                                                        'SCALING', &
                                                        'LABEL  ', &
                                                        'POINTER'/)
        integer :: idx_value = 1, idx_index = 2, idx_scale = 3, idx_label = 4, idx_pointer = 5
        character(len=7) :: field_type = 'NULL'
        character(len=9) :: value_type = 'NULL'
        class(cfast_type), pointer :: itemptr
        type(random_generator_type), pointer :: genptr
        class(value_wrapper_type), pointer :: valptr
        type(random_char_type) :: charval
        type(random_real_type) :: realval
        type(random_int_type) :: intval
        type(random_logic_type) :: logicval
        type(random_real_type) :: randptr
        integer :: nidx, nlabel
        real(eb) :: scale_value, scale_base_value, rand_value
        real(eb), dimension(mxpntsarray) :: real_array
        integer, dimension(mxpntsarray) :: int_array
        logical, dimension(mxpntsarray) :: logic_array
        character(len=128), dimension(mxpntsarray) :: char_array
        character(len=128) :: labelval
        logical :: conditional_min, conditional_max
        integer :: position
    contains
        procedure :: do_rand
        procedure :: dependencies => field_dependencies
        procedure :: dependencies_set => field_dependencies_set
        procedure :: min_dependent => field_min_dependent
        procedure :: max_dependent => field_max_dependent
        procedure :: add_dependent => field_add_dependent
        procedure :: min_dependency => field_min_dependency
        procedure :: max_dependency => field_max_dependency 
        procedure :: add_dependency => field_add_dependency 
        procedure :: set_min_value => field_set_min_value
        procedure :: set_max_value => field_set_max_value
        procedure :: set_add_value => field_set_add_value
        procedure :: set_min_field => field_set_min_field
        procedure :: set_max_field => field_set_max_field
        procedure :: set_add_field => field_set_add_field
        procedure :: max_dependency_set => field_max_dependency_set
        procedure :: min_dependency_set => field_min_dependency_set
        procedure :: add_dependency_set => field_add_dependency_set
    end type
    
    !
    !-----------random_generator_type--------
    !
    
    type, extends(preprocessor_type) :: random_generator_type
        character(len=35) :: type_dist                  ! only accepts a defined list of elements: UNIFORM, DISCRETE_UNIFORM, 
                                                        !           TRIANGLE, USER_DEFINED_DISCRETE, 
                                                        !           USER_DEFINED_CONTINOUS_INTERVAL, BETA, NORMAL, LOG_NORMAL, 
                                                        !           CONSTANT, VALUE
        character(len=9) :: value_type                  ! what value type the generator produces: CHARACTER, REAL, INTEGER, 
                                                        !           LOGICAL
        character(len=128) :: char_array(mxpntsarray)   ! values for discrete distributions with string type
        integer :: num_discrete_values                  ! number of values in arrays for discrere probablities 
        real(eb) :: real_array(mxpntsarray)             ! values for discrete distributions with real type
        integer :: int_array(mxpntsarray)               ! values for discrete distributions with integer type
        logical :: logic_array(mxpntsarray)             ! values for discrete distributions with logical type
        real(eb) :: prob_array(mxpntsarray)             ! user defined probablities for both descreet and continous 
        real(eb) :: maxvalue, minvalue, addvalue   
        real(eb) :: max_offset = 0.0_eb, min_offset = 0.0_eb
        type(random_real_type) :: maxval, minval, addval
        class(value_wrapper_type), pointer :: minptr, maxptr, addptr
        real(eb) :: mean, stdev                         ! mean and standard deviation for normal distributions. 
        real(eb) :: alpha, beta                         ! for distributions like beta that use those parameters
        real(eb) :: peak                                ! for the triangle distribution where the peak of the triangle occurs
        real(eb) :: constant                            ! for constant value functions
        logical :: first = .true.                       ! logical used in conjunction with 
        logical :: use_seeds                            ! determines if seeds have been supplied. 
        integer :: seeds(mxseeds)                       ! seed values
        integer :: base_seeds(mxseeds)                  ! first seeds used 
        integer :: current_iteration
        real(eb) :: linear_delta
        type(field_pointer) :: current_val
        real(eb) :: current_real_val
        integer :: current_int_val
        character(len=128) :: current_char_val
        logical :: current_logic_val
        real(eb) :: range
        character(len=128) :: maxfieldid = 'NULL', minfieldid = 'NULL', addfieldid = 'NULL'
        logical :: mindependent = .false., maxdependent = .false., adddependent = .false.
        logical :: min_set = .true., max_set = .true., add_set = .true.
    contains
        procedure :: rand
        procedure :: maximum => rand_max
        procedure :: minimum => rand_min
        procedure :: add => rand_add
        procedure :: set_current_value
        procedure :: set_min_value 
        procedure :: set_max_value
        procedure :: set_add_value
        procedure :: set_min_field
        procedure :: set_max_field
        procedure :: set_add_field
        procedure :: dependencies
        procedure :: dependencies_set
        procedure :: max_dependent
        procedure :: min_dependent
        procedure :: add_dependent
        procedure :: min_dependency_set
        procedure :: max_dependency_set
        procedure :: add_dependency_set
        procedure :: min_dependency
        procedure :: max_dependency
        procedure :: add_dependency
        procedure :: set_min_to_use_field
        procedure :: set_max_to_use_field
        procedure :: set_add_to_use_field
        procedure, private :: rand_mm_sub1
        procedure, private :: rand_mm_sub2
    end type random_generator_type
    
     
    
    !
    !-----------------------fire_generator_type---------
    !
    
    type, extends(value_wrapper_type) :: fire_generator_type
        character(len=128) :: fireid, basefireid, hrrscalegenid, timescalegenid, smoldergenid, &
            smoldertimegenid
        character(len=10), dimension(3) :: fir_typs = (/'UNMODIFIED', &
                                                        'SCALED    ', &
                                                        'NEW       ' /)
        character(len=10) :: fire_type
        integer :: idx_fire_unmod = 1, idx_fire_scale = 2, idx_fire_new = 3
        character(len=128), dimension(mxrooms) :: compname 
        integer, dimension(mxrooms) :: compindex
        logical :: first_time_point_smoldering, scalehrr, scaletime, dostime, copy_base_to_fire
        type(fire_type), pointer :: fire, base
        type(random_generator_type), pointer :: hrrscale, timescale, smoldergen, stimegen
        real(eb) :: hrrscalevalue, timescalevalue, stimevalue
        type(random_real_type) :: hrrscaleval, timescaleval, stimeval
        logical :: modifyfirearea
        type(field_pointer) :: fire_comp, fire_label
        logical :: do_fire_comp = .false. 
        character(len = 128) :: incipient_type, incipient_growth
        character(len = 10), dimension(4) :: incip_typ = (/'NONE      ', &
                                                           'FLAMING   ', &
                                                           'SMOLDERING', &
                                                           'RANDOM    ' /)
        integer :: idx_none = 1, idx_flame = 2, idx_smolder = 3, idx_random = 4
        type(field_pointer) :: fs_fire_ptr, smolder_hrr_ptr, flame_hrr_ptr, smolder_time_ptr, flame_time_ptr
        logical :: smoldering_fire
        character(len=10) :: incep_value
        real(eb) :: growthexpo, decayexpo
        integer :: growth_npts, decay_npts, last_growth_pt, first_decay_pt
        integer :: n_firepoints, n_firegenerators
        logical :: generate_fire = .false.
        logical :: fire_generator_is_time_to_1054_kW
        type(field_pointer), dimension(2,mxpts) :: firegenerators
        real(eb), dimension(2, mxpts) :: firevals
        
    contains
        procedure :: do_rand => fire_do_rand
        procedure :: copybasetofire
        procedure :: copytimebasedprop
    end type
    
    !
    !-----CONTAINS--------
    !
    
    contains
    
    !
    !--------------errorcall-----------------
    !
    
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
    
    !
    !--------rand---------------
    !
    
    subroutine rand(me, val, iteration)
    
        class(random_generator_type) :: me
        type(random_real_type), intent(inout) :: val
        integer, intent(in) :: iteration
        real(eb) :: x
        real :: a, b, p1, p2
        integer :: tmpseeds(mxseeds), ii, idx
        logical :: first
        
        if (me%first) then
            me%first = .false.
            first = .true.
            me%current_iteration = iteration - 1
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
        else 
            first = .false.
        end if
        
        if (me%current_iteration == iteration) then
            if (me%value_type == val_types(idx_real)) then
                val%val = me%current_real_val
            end if
            return
        end if
        
        me%current_iteration = iteration
        
        if (me%type_dist == rand_dist(idx_uniform)) then
            call RANDOM_SEED(GET=tmpseeds)
            call RANDOM_SEED(PUT=me%seeds)
            call RANDOM_NUMBER(x)
            call RANDOM_SEED(GET=me%seeds)
            call RANDOM_SEED(PUT = tmpseeds)
            if (me%value_type == val_types(idx_real)) then
                me%current_real_val = me%minimum() + me%min_offset + & 
                            x*(me%maximum() +me%max_offset - me%minimum() - me%min_offset)
                 me%current_real_val = me%current_real_val + me%add()
                val%val = me%current_real_val
            else 
                    call me%errorcall('RAND', 6)
            end if
        else if (me%type_dist == rand_dist(idx_user_defined_discrete)) then
            call RANDOM_SEED(GET=tmpseeds)
            call RANDOM_SEED(PUT=me%seeds)
            call RANDOM_NUMBER(x)
            call RANDOM_SEED(GET=me%seeds)
            call RANDOM_SEED(PUT = tmpseeds)
            do ii = 1, me%num_discrete_values
                if (x < me%prob_array(ii)) then
                    idx = ii
                    exit
                end if
            end do 
            if (me%value_type == val_types(idx_real)) then
                me%current_real_val = me%real_array(idx) + me%add()
                val%val = me%current_real_val
            else 
                call me%errorcall('RAND', 8)
            end if
        else if (me%type_dist == rand_dist(idx_user_defined_continous_interval)) then
            call RANDOM_SEED(GET=tmpseeds)
            call RANDOM_SEED(PUT=me%seeds)
            call RANDOM_NUMBER(x)
            call RANDOM_SEED(GET=me%seeds)
            call RANDOM_SEED(PUT = tmpseeds)
            do ii = 1, me%num_discrete_values
                if (x < me%prob_array(ii)) then
                    idx = ii
                    a = me%real_array(idx - 1)
                    b = me%real_array(idx)
                    p1 = me%prob_array(idx - 1)
                    p2 = me%prob_array(idx)
                    exit
                end if
            end do 
            if (me%value_type == val_types(idx_real)) then
                me%current_real_val = a + (x-p1)/(p2-p1)*(b-a) 
                me%current_real_val = me%current_real_val + me%add()
                val%val = me%current_real_val
            else 
                call me%errorcall('RAND', 12)
            end if
        else if (me%type_dist == rand_dist(idx_const)) then
            if (me%value_type == val_types(idx_real)) then
                me%current_real_val = me%constant + me%add()
                val%val = me%current_real_val
            end if
        else if (me%type_dist == rand_dist(idx_linear)) then
            if (me%value_type == val_types(idx_real)) then
                me%constant = me%constant + me%linear_delta
                me%current_real_val = me%constant + me%add()
                val%val = me%current_real_val
            else
                call me%errorcall('RAND', 16)
            end if
        else if (me%type_dist == rand_dist(idx_log_normal)) then
            call RANDOM_SEED(GET=tmpseeds)
            call RANDOM_SEED(PUT=me%seeds)
            x = random_normal()
            call RANDOM_SEED(GET=me%seeds)
            call RANDOM_SEED(PUT = tmpseeds)
            if (me%value_type == val_types(idx_real)) then
                me%current_real_val = exp(log(me%stdev)*x+log(me%mean))
                me%current_real_val = me%current_real_val + me%add()
                val%val = me%current_real_val
            end if
        else if (me%type_dist == rand_dist(idx_normal)) then
            call RANDOM_SEED(GET=tmpseeds)
            call RANDOM_SEED(PUT=me%seeds)
            x = random_normal()
            call RANDOM_SEED(GET=me%seeds)
            call RANDOM_SEED(PUT = tmpseeds)
            if (me%value_type == val_types(idx_real)) then
                me%current_real_val = me%stdev*x + me%mean + me%add()
                val%val = me%current_real_val
            end if
        else if (me%type_dist == rand_dist(idx_trun_normal)) then
            call RANDOM_SEED(GET=tmpseeds)
            call RANDOM_SEED(PUT=me%seeds)
            val%val = me%minimum() + me%min_offset - 100.0_eb
            if (me%value_type == val_types(idx_real)) then
                do while(val%val < me%minimum() + me%min_offset .or. val%val > me%maximum() + me%max_offset)
                    x = random_normal()
                    me%current_real_val = me%stdev*x + me%mean + me%add()
                    val%val = me%current_real_val
                end do
            end if
            call RANDOM_SEED(GET=me%seeds)
            call RANDOM_SEED(PUT = tmpseeds)
        else if (me%type_dist == rand_dist(idx_trun_log_normal)) then
            call RANDOM_SEED(GET=tmpseeds)
            call RANDOM_SEED(PUT=me%seeds)
            val%val = me%minimum() - 100.0_eb
            if (me%value_type == val_types(idx_real)) then
                do while(val%val < me%minimum() + me%min_offset .or. val%val > me%maximum() + me%max_offset)
                    x = random_normal()
                    me%current_real_val = exp(log(me%stdev)*x+log(me%mean)) + me%add()
                    val%val = me%current_real_val
                end do
            end if
            call RANDOM_SEED(GET=me%seeds)
            call RANDOM_SEED(PUT = tmpseeds)
        else if (me%type_dist == rand_dist(idx_triangle)) then
            call RANDOM_SEED(GET=tmpseeds)
            call RANDOM_SEED(PUT=me%seeds)
            call RANDOM_NUMBER(x)
            call RANDOM_SEED(GET=me%seeds)
            call RANDOM_SEED(PUT = tmpseeds)
            if (x <= (me%peak - me%minimum() - me%min_offset)/ &
                (me%maximum() + me%max_offset - me%minimum() - me%min_offset)) then
                me%current_real_val = sqrt(x*(me%maximum() + me%max_offset - me%minimum() - me%min_offset)* &
                    (me%peak - me%minimum() - me%min_offset)) + me%minimum() + me%min_offset + me%add()
            else
                 me%current_real_val = me%maximum() +  me%max_offset - &
                    sqrt((1 - x)*(me%maximum() + me%max_offset - me%minimum() - me%min_offset)* &
                    (me%maximum() + me%max_offset - me%peak)) + me%add()
            end if
            val%val = me%current_real_val
        else if (me%type_dist == rand_dist(idx_beta)) then
            call RANDOM_SEED(GET=tmpseeds)
            call RANDOM_SEED(PUT=me%seeds)
            a = me%alpha
            b = me%beta
            x = random_beta(a, b, first)
            call RANDOM_SEED(GET=me%seeds)
            call RANDOM_SEED(PUT = tmpseeds)
            if (me%value_type == val_types(idx_real)) then
                me%current_real_val = me%minimum() + me%min_offset + & 
                    x*(me%maximum() +me%max_offset - me%minimum() - me%min_offset)
                me%current_real_val = me%current_real_val + me%add()
                val%val = me%current_real_val
            else 
                call me%errorcall('RAND', 23)
            end if
        else if (me%type_dist == rand_dist(idx_gamma)) then
            call RANDOM_SEED(GET=tmpseeds)
            call RANDOM_SEED(PUT=me%seeds)
            a = me%alpha
            x = random_gamma(a, first)
            call RANDOM_SEED(GET=me%seeds)
            call RANDOM_SEED(PUT = tmpseeds)
            if (me%value_type == val_types(idx_real)) then
                me%current_real_val = x/me%beta
                me%current_real_val = me%current_real_val + me%add()
                val%val = me%current_real_val
            else 
                call me%errorcall('RAND', 25)
            end if
        else
            call me%errorcall('RAND', 27)
        end if
            
        return
    end subroutine rand
    
    !
    !---------add_header-----------
    !
    
    subroutine add_header(me, icol, array)
    
        class(value_wrapper_type), intent(inout) :: me
        integer, intent(inout) :: icol
        character(len=*), intent(out), target :: array(*)
        
        integer :: i
        
        if (me%add_to_parameters) then
            select type (me)
            type is (fire_generator_type)
                call me%hrrscaleval%add_header(icol, array)
                call me%timescaleval%add_header(icol, array)
                call me%fire_comp%add_header(icol, array)
                call me%fire_label%add_header(icol, array)
                call me%fs_fire_ptr%add_header(icol, array)
                !call me%flame_hrr_ptr%add_header(icol, array)
                !call me%flame_time_ptr%add_header(icol, array)
                !call me%smolder_hrr_ptr%add_header(icol, array)
                !call me%smolder_time_ptr%add_header(icol, array)
                do i = 1, me%n_firepoints
                    call me%firegenerators(2,i)%add_header(icol, array)
                    call me%firegenerators(1,i)%add_header(icol, array)
                end do 
                me%parameter_field_set = .true. 
            class default
                icol = icol + 1
                me%paramptr => array(icol)
                array(icol) = trim(me%parameter_column_label)
                me%parameter_field_set = .true.
            end select
        end if 
        
    end subroutine add_header
    
    !
    !--------write_value---------
    !
    
    subroutine write_value(me)
    
        class(value_wrapper_type), intent(inout) :: me
        
        integer :: i
        real(eb) :: tmp
    
        if (me%add_to_parameters .and. me%parameter_field_set) then
            select type (me)
            type is (value_wrapper_type)
                write(me%paramptr,'(a)') 'Values not set'
            type is (random_real_type)
                if (me%temp_flag) then 
                    tmp = me%val - kelvin_c_offset
                else if (me%kilo_flag) then
                    tmp = me%val/1000.0_eb
                else 
                    tmp = me%val
                end if 
                write(me%paramptr,'(e13.6)') tmp
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
                if (trim(me%field_type) == trim(me%fld_types(me%idx_label))) then
                    write(me%paramptr, '(a)') me%char_array(floor(me%rand_value))
                else
                    select case (me%value_type)
                    case ('NULL')
                        call me%errorcall('write_value',1)
                    case (val_types(idx_real))
                        if (me%temp_flag) then 
                            tmp = me%realval%val - kelvin_c_offset
                        else if (me%kilo_flag) then
                            tmp = me%realval%val/1000.0_eb
                        else 
                            tmp = me%realval%val
                        end if 
                        write(me%paramptr,'(e13.6)') tmp
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
                end if
            type is (fire_generator_type)
                if (me%hrrscaleval%add_to_parameters) then
                    write(me%hrrscaleval%paramptr,'(e13.6)') me%hrrscaleval%val
                end if
                if (me%timescaleval%add_to_parameters) then
                    write(me%timescaleval%paramptr,'(e13.6)') me%timescaleval%val
                end if
                call me%fire_label%write_value
                call me%fs_fire_ptr%write_value
                do i = 1, me%n_firepoints
                    call me%firegenerators(2,i)%write_value
                    call me%firegenerators(1,i)%write_value
                end do
            class default
                call me%errorcall('write_value',3)
            end select
        end if
    end subroutine write_value
    
    subroutine do_rand(me, iteration)
    
        class(field_pointer) :: me
        integer, intent(in) :: iteration
        integer :: idx
        character(len=128) :: id
    
        if (trim(me%field_type) == trim(me%fld_types(me%idx_pointer))) then
            return
        elseif (trim(me%field_type) == 'NULL') then
            return
        elseif (trim(me%field_type) == trim(me%fld_types(me%idx_value))) then 
            call me%genptr%rand(me%randptr, iteration)
            id = me%id
            if (me%value_type == val_types(idx_real)) then
                if (me%temp_flag) then
                    me%realval%val = me%rand_value + kelvin_c_offset
                else if (me%kilo_flag) then
                    me%realval%val = me%rand_value*1000.0_eb
                else 
                    me%realval%val = me%rand_value
                end if
            else
                call me%errorcall('DO_RAND',1)
            end if
        elseif (trim(me%field_type) == trim(me%fld_types(me%idx_index))) then
            call me%genptr%rand(me%randptr, iteration)
            idx = floor(me%randptr%val)
            if (me%value_type == val_types(idx_real)) then
                me%realval%val = me%real_array(idx)
            elseif (me%value_type == val_types(idx_char)) then
                me%charval%val = me%char_array(idx)
            elseif (me%value_type == val_types(idx_int)) then
                me%intval%val = me%int_array(idx)
            elseif (me%value_type == val_types(idx_logic)) then
                me%logicval%val = me%logic_array(idx)
            else
                call me%errorcall('DO_RAND',2)
            end if 
        elseif (trim(me%field_type) == trim(me%fld_types(me%idx_scale))) then
            call me%genptr%rand(me%randptr, iteration)
            if (me%value_type == val_types(idx_real)) then
                me%realval%val = me%scale_value * me%scale_base_value
            else
                call me%errorcall('DO_RAND',3)
            end if
        elseif (trim(me%field_type) == trim(me%fld_types(me%idx_label))) then
            call me%genptr%rand(me%randptr, iteration)
            idx = floor(me%randptr%val)
            if (me%value_type == val_types(idx_char)) then
                me%charval%val = me%char_array(idx)
            else
                call me%errorcall('DO_RAND',4)
            end if
        else
            call me%errorcall('DO_RAND',100)
        end if 
        
    end subroutine do_rand
    
    function rand_max(me) result(max)
    
        class(random_generator_type) :: me
        real(eb) :: max
        integer :: iflag
        
        call rand_sub(me%maxptr, max, iflag)
        if (iflag /= 0) then
            call me%errorcall('RAND_MAX',1)
        end if
        
    end function rand_max
    
    function rand_min(me) result(min)
    
        class(random_generator_type) :: me
        real(eb) :: min
        integer :: iflag
        
        call rand_sub(me%minptr, min, iflag)
        if (iflag /= 0) then
            call me%errorcall('RAND_MIN',1)
        end if
        
    end function rand_min
    
    function rand_add(me) result(add)
    
        class(random_generator_type) :: me
        real(eb) :: add
        integer :: iflag
        
        call rand_sub(me%addptr, add, iflag)
        if (iflag /= 0) then
            call me%errorcall('RAND_add',1)
        end if
        
    end function rand_add
    
    subroutine rand_sub(ptr, val, iflag)
        class(value_wrapper_type), intent(in) :: ptr
        real(eb), intent(out) :: val
        integer, intent(out) :: iflag
    
        select type(ptr)
            type is (random_real_type)
                val = ptr%val
                iflag = 0
            class default
                iflag = -1
            end select
    end subroutine rand_sub
    
    subroutine rand_mm_sub1(me, ptr, val)
    
        class(random_generator_type) :: me
        type(field_pointer) :: ptr
        real(eb) :: val
        
        call me%rand_mm_sub2(ptr%valptr, val)
         
    end subroutine rand_mm_sub1
    
    subroutine rand_mm_sub2(me, ptr, val)
    
        class(random_generator_type) :: me
        class(value_wrapper_type), pointer :: ptr
        real(eb) :: val
        
        select type(ptr)
            type is (random_real_type)
                val = ptr%val
            class default
                call me%errorcall('RAND_MM_SUB2',1)
        end select
            
    end subroutine rand_mm_sub2
    
    subroutine set_min_to_use_field(me, field_id)
    
        class(random_generator_type), target :: me
        character(len=128) :: field_id
        
        me%minfieldid = field_id
        me%mindependent = .true.
        me%min_set = .false. 
        
    end subroutine set_min_to_use_field
    
    subroutine set_max_to_use_field(me, field_id)
    
        class(random_generator_type), target :: me
        character(len=128) :: field_id
        
        me%maxfieldid =  field_id
        me%maxdependent = .true.
        me%max_set = .false. 
        
    end subroutine set_max_to_use_field
    
    subroutine set_add_to_use_field(me, field_id)
    
        class(random_generator_type), target :: me
        character(len=128) :: field_id
        
        me%addfieldid =  field_id
        me%adddependent = .true.
        me%add_set = .false. 
        
    end subroutine set_add_to_use_field
    
    subroutine set_min_value(me, minimum)
        
        class(random_generator_type), target :: me
        real(eb) :: minimum
        
        me%minvalue = minimum
        me%minval%val => me%minvalue
        me%minptr => me%minval
        me%mindependent = .false.
        me%min_set = .true. 
        me%minfieldid = 'NULL'
    
    end subroutine set_min_value
    
    subroutine set_max_value(me, maximum)
        
        class(random_generator_type), target :: me
        real(eb) :: maximum
        
        me%maxvalue = maximum
        me%maxval%val => me%maxvalue
        me%maxptr => me%maxval
        me%maxdependent = .false.
        me%max_set = .true.
        me%maxfieldid = 'NULL'
    
    end subroutine set_max_value
    
    subroutine set_add_value(me, add)
        
        class(random_generator_type), target :: me
        real(eb) :: add
        
        me%addvalue = add
        me%addval%val => me%addvalue
        me%addptr => me%addval
        me%adddependent = .false.
        me%add_set = .true.
        me%addfieldid = 'NULL'
    
    end subroutine set_add_value
    
    subroutine set_min_field(me, field)
    
        class(random_generator_type) :: me
        class(field_pointer) :: field
        
        me%minptr => field%valptr
        me%min_set = .true. 
        
    end subroutine set_min_field
    
    subroutine set_max_field(me, field)
    
        class(random_generator_type), target :: me
        class(field_pointer), target :: field
        
        me%maxptr => field%valptr
        me%max_set = .true.
        
    end subroutine set_max_field
    
    subroutine set_add_field(me, field)
    
        class(random_generator_type), target :: me
        class(field_pointer), target :: field
        
        me%addptr => field%valptr
        me%add_set = .true.
        
    end subroutine set_add_field
    
    function min_dependent(me) result(dependent)
        
        class(random_generator_type) :: me
        logical :: dependent
            
        dependent = me%mindependent
            
    end function min_dependent
    
    function max_dependent(me) result(dependent)
        
        class(random_generator_type) :: me
        logical :: dependent
            
        dependent = me%maxdependent
            
    end function max_dependent
    
    function add_dependent(me) result(dependent)
        
        class(random_generator_type) :: me
        logical :: dependent
            
        dependent = me%adddependent
            
    end function add_dependent
    
    function dependencies(me) result(dependent)
    
        class(random_generator_type) :: me
        logical :: dependent
        
        dependent = me%mindependent .or. me%maxdependent .or. me%adddependent
    
    end function dependencies
    
    function dependencies_set(me) result(set)
    
        class(random_generator_type) :: me
        logical :: set
        
        set = me%min_set .and. me%max_set .and. me%add_set
    
    end function dependencies_set
    
    function min_dependency(me) result(field_id)
    
        class(random_generator_type) :: me
        character(len=128) :: field_id
        
        field_id = me%minfieldid
        
    end function min_dependency
    
    function max_dependency(me) result(field_id)
    
        class(random_generator_type) :: me
        character(len=128) :: field_id
        
        field_id = me%maxfieldid
        
    end function max_dependency
    
    function add_dependency(me) result(field_id)
    
        class(random_generator_type) :: me
        character(len=128) :: field_id
        
        field_id = me%addfieldid
        
    end function add_dependency
    
    function min_dependency_set(me) result(set)
    
        class(random_generator_type) :: me
        logical :: set
        
        set = me%min_set
    
    end function min_dependency_set
    
    function max_dependency_set(me) result(set)
    
        class(random_generator_type) :: me
        logical :: set
        
        set = me%max_set
    
    end function max_dependency_set
    
    function add_dependency_set(me) result(set)
    
        class(random_generator_type) :: me
        logical :: set
        
        set = me%add_set
    
    end function add_dependency_set
    
    function field_dependencies(me) result(dependent)
        
        class(field_pointer) :: me
        logical :: dependent
        
        if (trim(me%field_type) == trim(me%fld_types(me%idx_pointer))) then
            dependent = .false.
        else
            dependent = me%genptr%dependencies()
        end if
    
    end function field_dependencies
    
    function field_dependencies_set(me) result(set)
        
        class(field_pointer) :: me
        logical :: set
        
        if (trim(me%field_type) == trim(me%fld_types(me%idx_pointer))) then
            set = .true.
        else
            set = me%genptr%dependencies_set()
        end if
    
    end function field_dependencies_set
    
    function field_min_dependent(me) result(dependent)
        
        class(field_pointer) :: me
        logical :: dependent
        
        if (trim(me%field_type) == trim(me%fld_types(me%idx_pointer))) then
            dependent = .false.
        else
            dependent = me%genptr%min_dependent()
        end if
    
    end function field_min_dependent
    
    function field_max_dependent(me) result(dependent)
        
        class(field_pointer) :: me
        logical :: dependent
        
        if (trim(me%field_type) == trim(me%fld_types(me%idx_pointer))) then
            dependent = .false.
        else
            dependent = me%genptr%max_dependent()
        end if
    
    end function field_max_dependent
    
    function field_add_dependent(me) result(dependent)
        
        class(field_pointer) :: me
        logical :: dependent
        
        if (trim(me%field_type) == trim(me%fld_types(me%idx_pointer))) then
            dependent = .false.
        else
            dependent = me%genptr%add_dependent()
        end if
    
    end function field_add_dependent
    
    function field_min_dependency(me) result(field_id)
        
        class(field_pointer) :: me
        character(len=28) :: field_id
        
        if (trim(me%field_type) == trim(me%fld_types(me%idx_pointer))) then
            field_id = 'NULL'
        else
            field_id = me%genptr%min_dependency()
        end if
    
    end function field_min_dependency
    
    function field_max_dependency(me) result(field_id)
        
        class(field_pointer) :: me
        character(len=28) :: field_id
        
        if (trim(me%field_type) == trim(me%fld_types(me%idx_pointer))) then
            field_id = 'NULL'
        else
            field_id = me%genptr%max_dependency()
        end if
    
    end function field_max_dependency
    
    function field_add_dependency(me) result(field_id)
        
        class(field_pointer) :: me
        character(len=28) :: field_id
        
        if (trim(me%field_type) == trim(me%fld_types(me%idx_pointer))) then
            field_id = 'NULL'
        else
            field_id = me%genptr%add_dependency()
        end if
    
    end function field_add_dependency
    
    function field_max_dependency_set(me) result(set)
    
        class(field_pointer) :: me
        logical :: set
        
        if (trim(me%field_type) == trim(me%fld_types(me%idx_pointer))) then
            set = .true.
        else
            set = me%genptr%max_dependency_set()
        end if
    
    end function field_max_dependency_set
    
    function field_min_dependency_set(me) result(set)
    
        class(field_pointer) :: me
        logical :: set
        
        if (trim(me%field_type) == trim(me%fld_types(me%idx_pointer))) then
            set = .true.
        else
            set = me%genptr%min_dependency_set()
        end if
    
    end function field_min_dependency_set
    
    function field_add_dependency_set(me) result(set)
    
        class(field_pointer) :: me
        logical :: set
        
        if (trim(me%field_type) == trim(me%fld_types(me%idx_pointer))) then
            set = .true.
        else
            set = me%genptr%add_dependency_set()
        end if
    
    end function field_add_dependency_set
    
    subroutine field_set_min_value(me, minimum)
    
        class(field_pointer) :: me
        real(eb) :: minimum
        
        if (trim(me%field_type) == trim(me%fld_types(me%idx_pointer))) then
            minimum = -1001._eb
        else
            call set_min_value(me%genptr, minimum)
        end if
        
    end subroutine field_set_min_value
    
    subroutine field_set_max_value(me, maximum)
    
        class(field_pointer) :: me
        real(eb) :: maximum
        
        if (trim(me%field_type) == trim(me%fld_types(me%idx_pointer))) then
            maximum = -1001._eb
        else
            call me%genptr%set_max_value(maximum)
        end if
        
    end subroutine field_set_max_value
    
    subroutine field_set_add_value(me, add)
    
        class(field_pointer) :: me
        real(eb) :: add
        
        if (trim(me%field_type) == trim(me%fld_types(me%idx_pointer))) then
            
        else
            call me%genptr%set_add_value(add)
        end if
        
    end subroutine field_set_add_value
    
    subroutine field_set_min_field(me, field)
    
        class(field_pointer) :: me
        type(field_pointer) :: field
        
        call me%genptr%set_min_field(field)
        
    end subroutine field_set_min_field
    
    subroutine field_set_max_field(me, field)
    
        class(field_pointer) :: me
        type(field_pointer) :: field
        
        call me%genptr%set_max_field(field)
        
    end subroutine field_set_max_field
    
    subroutine field_set_add_field(me, field)
    
        class(field_pointer) :: me
        type(field_pointer) :: field
        
        call me%genptr%set_add_field(field)
        
    end subroutine field_set_add_field
    
    subroutine set_current_value(me)
        
        class(random_generator_type), target :: me
        
        if (trim(me%value_type) == trim(val_types(idx_real))) then
            me%current_val%realval%val => me%current_real_val
            me%current_val%value_type = val_types(idx_real)
            me%current_val%valptr => me%current_val%realval
        else if (trim(me%value_type) == trim(val_types(idx_int))) then
            me%current_val%intval%val => me%current_int_val
            me%current_val%value_type = val_types(idx_int)
            me%current_val%valptr => me%current_val%intval
        else if (trim(me%value_type) == trim(val_types(idx_char))) then
            me%current_val%charval%val => me%current_char_val
            me%current_val%value_type = val_types(idx_char)
            me%current_val%valptr => me%current_val%charval
        else if (trim(me%value_type) == trim(val_types(idx_logic))) then
            me%current_val%logicval%val => me%current_logic_val
            me%current_val%value_type = val_types(idx_logic)
            me%current_val%valptr => me%current_val%logicval
        else
            call me%errorcall('rand_geneator_type: set_current_value', 1)
        end if
        
    end subroutine set_current_value
    
    subroutine fire_do_rand(me, iteration)
    
        class(fire_generator_type) :: me
        integer, intent(in) :: iteration
        
        integer :: i, tmp, tmp1
        real(eb) :: deltat, a, b, c, t1, t0
        
        if (me%copy_base_to_fire) then
            call me%copybasetofire(me%fire, me%base, me%copy_base_to_fire)
        end if 
        
        if (me%scalehrr) then 
            call me%hrrscale%rand(me%hrrscaleval,iteration)
            do i = 1, mxpts
                me%fire%qdot(i) = me%hrrscalevalue*me%base%qdot(i)
            end do
            do i = 1, mxpts
                me%fire%mdot(i) = me%hrrscalevalue*me%base%mdot(i)
            end do 
        end if 
        
        if (me%scaletime) then
            call me%timescale%rand(me%timescaleval,iteration)
            do i = 1, mxpts
                me%fire%t_qdot(i) = me%timescalevalue*me%base%t_qdot(i)
            end do
            do i = 1, mxpts
                me%fire%t_mdot(i) = me%timescalevalue*me%base%t_mdot(i)
            end do
            me%fire%flaming_transition_time = me%timescalevalue*me%base%flaming_transition_time
        end if 
        
        if (me%do_fire_comp) then
            call me%fire_comp%do_rand(iteration)
            call me%fire_label%do_rand(iteration)
        end if 
        
        tmp = 0
        
        ! Doing the incipient growth model
        ! determing if it is flaming or smoldering if that is set up
        if (trim(me%incipient_type) == trim(me%incip_typ(me%idx_random))) then
            call me%fs_fire_ptr%do_rand(iteration)
        end if 
        ! Determing the actual values for peak HRR and length of time
        if (trim(me%incipient_growth) == trim(me%incip_typ(me%idx_flame))) then
            tmp = me%flame_time_ptr%realval%val
            call me%flame_time_ptr%do_rand(iteration)
            tmp = me%flame_time_ptr%realval%val - tmp
            call me%flame_hrr_ptr%do_rand(iteration)
            me%fire%flaming_transition_time = 0
        else if (trim(me%incipient_growth) == trim(me%incip_typ(me%idx_smolder))) then
            tmp = me%smolder_time_ptr%realval%val
            call me%smolder_time_ptr%do_rand(iteration)
            tmp = me%smolder_time_ptr%realval%val - tmp
            call me%smolder_hrr_ptr%do_rand(iteration)
            me%fire%flaming_transition_time = me%smolder_time_ptr%realval%val
        end if
        
        if (me%generate_fire) then
            do i = 1, me%n_firepoints
                call me%firegenerators(1,i)%do_rand(iteration)
                call me%firegenerators(2,i)%do_rand(iteration)
            end do
            if (me%growth_npts > 0) then
                if (trim(me%incipient_type) /= trim(me%incip_typ(me%idx_none))) then
                    tmp1 = 1
                else 
                    tmp1 = 0
                end if
                if (.not. me%fire_generator_is_time_to_1054_kW) then
                    t1 = me%firegenerators(2,me%last_growth_pt+1)%realval%val
                    c = me%firegenerators(1,1 + tmp1)%realval%val
                    a = (me%firegenerators(1, me%last_growth_pt + 1)%realval%val - c)/ t1**me%growthexpo
                    t0 = 0.0_eb
                    deltat = (t1 - t0)/(me%growth_npts + 1)
                else 
                    t1 = me%firegenerators(2,me%last_growth_pt+1)%realval%val
                    c = me%firegenerators(1,me%last_growth_pt + 1)%realval%val
                    a = 1054000.0_eb/t1**me%growthexpo
                    t1 = (c/a)**(1/me%growthexpo)
                    c = me%firegenerators(1,1 + tmp1)%realval%val
                    t0 = (c/a)**(1/me%growthexpo)
                    deltat = (t1 - t0)/(me%growth_npts + 1)
                end if
                do i = 2 + tmp1, me%last_growth_pt
                    me%fire%t_qdot(i) = deltat
                    me%fire%qdot(i) = a*((i - 1 - tmp1)*deltat + t0)**me%growthexpo + c
                end do
                me%fire%t_qdot(me%last_growth_pt + 1) = deltat
            end if
            if (me%decay_npts > 0) then
                t1 = me%firegenerators(2,me%n_firepoints)%realval%val
                c = me%firegenerators(1,me%n_firepoints)%realval%val
                b = me%firegenerators(1,me%first_decay_pt-1)%realval%val - c
                a = b/t1**me%decayexpo
                deltat = (t1)/(me%decay_npts + 1)
                tmp = 0
                do i = me%first_decay_pt, me%n_firepoints - 1
                    tmp = tmp + 1
                    me%fire%t_qdot(i) = deltat
                    me%fire%qdot(i) = a*(t1 - tmp*deltat)**me%decayexpo + c
                end do
                me%fire%t_qdot(me%n_firepoints) = deltat
            end if
            me%fire%n_qdot = me%n_firepoints
            do i = 2 + tmp1, me%n_firepoints
                me%fire%t_qdot(i) = me%fire%t_qdot(i-1) + me%fire%t_qdot(i)
            end do
        end if
        
        ! Updating other times after having calculated the new t_qdot. tmp is the change in time due to incipient model
        ! it is set to zero before the incipient stuff in case the incipient model is not being used
        
        if (trim(me%incipient_type) /= trim(me%incip_typ(me%idx_none)) .or. &
            me%n_firegenerators > 0) then
            if (me%fire%n_qdot /= me%fire%n_mdot) then
                if (me%fire%n_qdot > me%fire%n_mdot) then
                    do i = me%fire%n_mdot + 1, me%fire%n_qdot
                        me%fire%mdot(i) = me%fire%mdot(me%fire%n_mdot)
                        me%fire%area(i) = me%fire%area(me%fire%n_area)
                        me%fire%height(i) = me%fire%height(me%fire%n_height)
                        me%fire%y_soot(i) = me%fire%y_soot(me%fire%n_soot)
                        me%fire%y_co(i) = me%fire%y_co(me%fire%n_co)
                        me%fire%y_trace(i) = me%fire%y_trace(me%fire%n_trace)
                        me%fire%hoc(i) = me%fire%hoc(me%fire%n_hoc)
                    end do
                end if
                me%fire%n_mdot = me%fire%n_qdot
                me%fire%n_area = me%fire%n_qdot
                me%fire%n_height = me%fire%n_qdot
                me%fire%n_soot = me%fire%n_qdot
                me%fire%n_co = me%fire%n_qdot
                me%fire%n_soot = me%fire%n_qdot
                me%fire%n_trace = me%fire%n_qdot
                me%fire%n_hoc = me%fire%n_qdot
            end if 
            do i = 3, me%fire%n_qdot
                me%fire%t_mdot(i) = me%fire%t_qdot(i)
                me%fire%t_area(i) = me%fire%t_qdot(i)
                me%fire%t_height(i) = me%fire%t_qdot(i)
                me%fire%t_soot(i) = me%fire%t_qdot(i)
                me%fire%t_co(i) = me%fire%t_qdot(i)
                me%fire%t_hcn(i) = me%fire%t_qdot(i)
                me%fire%t_trace(i) = me%fire%t_qdot(i)
                me%fire%t_hoc(i) = me%fire%t_qdot(i)
            end do
        end if
        if (me%modifyfirearea) then
            do i = 1, me%fire%n_qdot
                me%fire%area(i) = (me%fire%qdot(i)/(352.981915_eb*1012._eb*sqrt(9.80665_eb)))**(4./5.)/4._eb
                if (me%fire%area(i) < 0.001_eb) me%fire%area(i) = 0.001_eb
            end do
        end if
        
    end subroutine fire_do_rand
    
    subroutine copybasetofire(me, fire, base, flag)
    
        class(fire_generator_type) :: me
        type(fire_type), intent(inout) :: fire, base
        logical, intent(inout) :: flag
        
        fire%chemistry_type = base%chemistry_type
        fire%n_C = base%n_C
        fire%n_H = base%n_H
        fire%n_N = base%n_N
        fire%n_O = base%n_O
        fire%n_Cl = base%n_Cl
        call me%copytimebasedprop(fire%n_mdot, fire%mdot, fire%t_mdot, base%n_mdot, base%mdot, base%t_mdot)
        call me%copytimebasedprop(fire%n_qdot, fire%qdot, fire%t_qdot, base%n_qdot, base%qdot, base%t_qdot)
        call me%copytimebasedprop(fire%n_area, fire%area, fire%t_area, base%n_area, base%area, base%t_area)
        call me%copytimebasedprop(fire%n_height, fire%height, fire%t_height, base%n_height, base%height, &
            base%t_height)
        call me%copytimebasedprop(fire%n_soot, fire%y_soot, fire%t_soot, base%n_soot, base%y_soot, base%t_soot)
        call me%copytimebasedprop(fire%n_co, fire%y_co, fire%t_co, base%n_co, base%y_co, base%t_co)
        call me%copytimebasedprop(fire%n_hcn, fire%y_hcn, fire%t_hcn, base%n_hcn, base%y_hcn, base%t_hcn)
        call me%copytimebasedprop(fire%n_trace, fire%y_trace, fire%t_trace, base%n_trace, base%y_trace, base%t_trace)
        call me%copytimebasedprop(fire%n_hoc, fire%hoc, fire%t_hoc, base%n_hoc, base%hoc, base%t_hoc)
        fire%z_offset = base%z_offset
        fire%characteristic_length = base%characteristic_length
        fire%molar_mass = base%molar_mass
        fire%ignition_time = base%ignition_time
        fire%flaming_transition_time = base%flaming_transition_time
        flag = .false.
    
    end subroutine copybasetofire
    
    subroutine copytimebasedprop(me, nx, x, xt, nb, b, bt)
    
        class(fire_generator_type) :: me
        integer :: nx, nb
        real(eb), dimension(mxpts) :: x, b, xt, bt
        
        integer :: i
        
        ! line used to supress compiler warning
        me%id = me%id
        
        nx = nb
        do i = 1, mxpts
            x(i) = b(i)
            xt(i) = bt(i)
        end do
        
    end subroutine copytimebasedprop
    
    end module preprocessor_types
    
    !
    !--------------------analysis_types module----------------------------------
    !
    module analysis_types
    
    use precision_parameters
    
    use preprocessor_types, only: preprocessor_type
    
    !
    !-------------------stat_type--------------------------
    !
    type, extends(preprocessor_type) :: stat_type
        character(len=128) :: analysis_type
        character(len=256) :: infile
        character(len=256) :: outfile
        character(len=256) :: errfile
        character(len=256) :: logfile
        character(len=256) :: col_title
        character(len=5) :: img_format 
    end type stat_type
    
    end module analysis_types
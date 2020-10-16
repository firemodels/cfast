module preprocessor_types

    use precision_parameters
    use exit_routines, only: cfastexit
    use pp_params, only: mxpntsarray, idx_uniform, idx_trangle, idx_user_defined_discrete, &
                     idx_user_defined_continous_interval, idx_beta, idx_normal , idx_log_normal, rand_dist, &
                     val_types, idx_real, idx_char, idx_int, idx_logic, mxseeds, idx_const
    
    use cparams
    use cfast_types, only: cfast_type, fire_type
    
    intrinsic random_seed, random_number
    
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
        character(len=128) :: parameter_header
        character(len=128), pointer :: paramptr
        logical :: pointer_set = .false.
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
        character(len=7), dimension(4) :: fld_types = (/'VAULE  ', &
                                                        'INDEX  ', &
                                                        'SCALING', &
                                                        'LABEL  '/)
        character(len=7) :: field_type = 'NULL'
        integer :: idx_value = 1, idx_index = 2, idx_scale = 3, idx_label = 4
        character(len=9) :: value_type = 'NULL'
        class(cfast_type), pointer :: itemptr
        type(random_generator_type), pointer :: genptr
        class(value_wrapper_type), pointer :: valptr
        type(random_char_type) :: charval
        type(random_real_type) :: realval
        type(random_int_type) :: intval
        type(random_logic_type) :: logicval
        class(value_wrapper_type), pointer :: randptr
        type(random_int_type) :: indexval
        type(random_real_type) :: scaleval
        integer :: index, nidx, nlabel
        real(eb) :: scale_value
        real(eb) :: scale_base_value
        real(eb), dimension(mxpntsarray) :: real_array
        integer, dimension(mxpntsarray) :: int_array
        logical, dimension(mxpntsarray) :: logic_array
        character(len=128), dimension(mxpntsarray) :: char_array
        logical :: conditional_min, conditional_max
        integer :: position
    contains
        procedure :: do_rand
        procedure :: dependencies => field_dependencies
        procedure :: dependencies_set => field_dependencies_set
        procedure :: min_dependent => field_min_dependent
        procedure :: max_dependent => field_max_dependent
        procedure :: min_dependency => field_min_dependency
        procedure :: max_dependency => field_max_dependency 
        procedure :: set_min_value => field_set_min_value
        procedure :: set_max_value => field_set_max_value
        procedure :: set_min_field => field_set_min_field
        procedure :: set_max_field => field_set_max_field
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
        real(eb) :: maxval, minval                      ! the ends of the interval used for the random generator
        real(eb) :: mean, stdev                         ! mean and standard deviation for normal distributions. 
        real(eb) :: alpha, beta                         ! for distributions like beta that use those parameters
        real(eb) :: peak                                ! for the triangle distribution where the peak of the triangle occurs
        real(eb) :: constant                            ! for constant value functions
        logical :: first = .true.                       ! logical used in conjunction with 
        logical :: use_seeds                            ! determines if seeds have been supplied. 
        integer :: seeds(mxseeds)                       ! seed values
        integer :: base_seeds(mxseeds)                  ! first seeds used 
        integer :: current_iteration
        type(field_pointer) :: current_val
        real(eb) :: current_real_val
        integer :: current_int_val
        character(len=128) :: current_char_val
        logical :: current_logic_val
        real(eb) :: range
        type(field_pointer), pointer :: maxptr, minptr
        character(len=128) :: maxfieldid = 'NULL', minfieldid = 'NULL'
        logical :: mindependent = .false., maxdependent = .false. 
        logical :: min_set = .true., max_set = .true.
    contains
        procedure :: rand
        procedure :: max => rand_max
        procedure :: min => rand_min
        procedure :: set_current_value
        procedure :: set_min_value 
        procedure :: set_max_value
        procedure :: set_min_field
        procedure :: set_max_field
        procedure :: dependencies
        procedure :: dependencies_set
        procedure :: max_dependent
        procedure :: min_dependent
        procedure :: min_dependency_set
        procedure :: max_dependency_set
        procedure :: min_dependency
        procedure :: max_dependency
        procedure :: set_min_to_use_field
        procedure :: set_max_to_use_field
        procedure, private :: rand_mm_sub1
        procedure, private :: rand_mm_sub2
    end type random_generator_type
    
     
    
    !
    !-----------------------fire_generator_type---------
    !
    
    type, extends(value_wrapper_type) :: fire_generator_type
        character(len=128) :: fireid, basefireid, hrrscalegenid, timescalegenid, smoldergenid, &
            smoldertimegenid
        character(len=128), dimension(mxrooms) :: compname 
        integer, dimension(mxrooms) :: compindex
        logical :: first_time_point_smoldering, scalehrr, scaletime, dostime, copy_base_to_fire
        type(fire_type), pointer :: fire, base
        type(random_generator_type), pointer :: hrrscale, timescale, smoldergen, stimegen
        real(eb) :: hrrscalevalue, timescalevalue, stimevalue
        type(random_real_type) :: hrrscaleval, timescaleval, stimeval
        logical :: smoldervalue, modifyfirearea
        type(random_logic_type) :: smolderval
        type(field_pointer) :: fire_comp
        type(field_pointer) :: fire_label
        logical :: rand_comp = .false. 
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
        class(value_wrapper_type), intent(inout) :: val
        integer :: iteration
        real(eb) :: x
        integer :: tmpseeds(mxseeds), ii, idx
        
        if (me%first) then
            me%first = .false.
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
        end if
        
        if (me%current_iteration == iteration) then
            select type(val)
                type is (random_real_type)
                    if (me%value_type == val_types(idx_real)) then
                        select type(val)
                            type is (random_real_type)
                                val%val = me%current_real_val
                            class default
                                call me%errorcall('RAND', 1)
                        end select
                    else 
                        call me%errorcall('Rand', 1)
                    end if
                type is (random_int_type)
                    if (me%value_type == val_types(idx_int)) then
                        val%val = me%current_int_val
                    else 
                        call me%errorcall('Rand', 2)
                    end if
                type is (random_char_type)
                    if (me%value_type == val_types(idx_char)) then
                        val%val = me%current_char_val
                    else 
                        call me%errorcall('Rand', 3)
                    end if
                type is (random_logic_type)
                    if (me%value_type == val_types(idx_logic)) then
                        val%val = me%current_logic_val
                    else 
                        call me%errorcall('Rand', 4)
                    end if
                class default
                    call me%errorcall('Rand',5)
            end select
            return
        end if
        
        me%current_iteration = iteration
        
        if (me%type_dist == rand_dist(idx_uniform)) then
            call RANDOM_SEED(GET=tmpseeds)
            call RANDOM_SEED(PUT=me%seeds)
            call RANDOM_NUMBER(x)
            call RANDOM_SEED(GET=me%seeds)
            call RANDOM_SEED(PUT = tmpseeds)
            select type (val)
                type is (random_real_type)
                    if (me%value_type == val_types(idx_real)) then
                        me%current_real_val = me%min()+ x*(me%max() - me%min())
                        val%val = me%current_real_val
                    else 
                        call me%errorcall('RAND', 6)
                    end if
                class default
                        call me%errorcall('RAND', 7)
            end select 
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
            select type (val)
                type is (random_real_type)
                    if (me%value_type == val_types(idx_real)) then
                        me%current_real_val = me%real_array(idx)
                        val%val = me%current_real_val
                    else 
                        call me%errorcall('RAND', 8)
                    end if
                type is (random_int_type)
                    if (me%value_type == val_types(idx_int)) then
                        me%current_int_val = me%int_array(idx)
                        val%val = me%current_int_val
                    else 
                        call me%errorcall('RAND', 9)
                    end if
                type is (random_char_type)
                    if (me%value_type == val_types(idx_char)) then
                        me%current_char_val = me%char_array(idx) 
                        val%val = me%current_char_val
                    else 
                        call me%errorcall('RAND', 10)
                    end if
                class default 
                    call me%errorcall('RAND', 11)
            end select
        else if (me%type_dist == rand_dist(idx_const)) then
            select type(val)
                type is (random_real_type)
                    if (me%value_type == val_types(idx_real)) then
                        val%val = me%current_real_val
                    else
                        call me%errorcall('RAND', 12)
                    end if
                class default
                    call me%errorcall('RAND',13)
            end select
        else
            call me%errorcall('RAND', 14)
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
        
        if (me%add_to_parameters) then
            select type (me)
            type is (fire_generator_type)
                if (me%hrrscaleval%add_to_parameters) then
                    icol = icol + 1
                    me%hrrscaleval%paramptr => array(icol)
                    array(icol) = trim(me%hrrscaleval%parameter_header)
                    me%hrrscaleval%parameter_field_set = .true.
                    me%parameter_field_set = .true.
                end if 
                if (me%timescaleval%add_to_parameters) then
                    icol = icol + 1
                    me%timescaleval%paramptr => array(icol)
                    array(icol) = trim(me%timescaleval%parameter_header)
                    me%timescaleval%parameter_field_set = .true.
                    me%parameter_field_set = .true.
                end if
            class default
                icol = icol + 1
                me%paramptr => array(icol)
                array(icol) = trim(me%parameter_header)
                me%parameter_field_set = .true.
            end select
        end if 
        
    end subroutine add_header
    
    !
    !--------write_value---------
    !
    
    subroutine write_value(me)
    
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
                if (trim(me%field_type) == trim(me%fld_types(me%idx_label))) then
                    write(me%paramptr, '(a)') me%char_array(me%index)
                else
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
                end if
            type is (fire_generator_type)
                if (me%hrrscaleval%add_to_parameters) then
                    write(me%hrrscaleval%paramptr,'(e13.6)') me%hrrscaleval%val
                end if
                if (me%timescaleval%add_to_parameters) then
                    write(me%timescaleval%paramptr,'(e13.6)') me%timescaleval%val
                end if
            class default
                call me%errorcall('write_value',3)
            end select
        end if
    end subroutine write_value
    
    subroutine do_rand(me, val, iteration)
    
        class(field_pointer) :: me
        class(value_wrapper_type), intent(inout) :: val
        integer, intent(in) :: iteration
    
        if (trim(me%field_type) == trim(me%fld_types(me%idx_value))) then 
            call me%genptr%rand(me%valptr, iteration)
        elseif (trim(me%field_type) == trim(me%fld_types(me%idx_index))) then
            call me%genptr%rand(me%randptr, iteration)
            if (me%value_type == val_types(idx_real)) then
                select type(val)
                    type is (random_real_type)
                        val%val = me%real_array(me%index)
                    class default
                        call me%errorcall('DO_RAND',1)
                end select
            elseif (me%value_type == val_types(idx_char)) then
                select type(val)
                    type is (random_char_type)
                        val%val = me%char_array(me%index)
                    class default
                        call me%errorcall('DO_RAND',2)
                end select
            elseif (me%value_type == val_types(idx_int)) then
                select type(val)
                    type is (random_int_type)
                        val%val = me%int_array(me%index)
                    class default
                        call me%errorcall('DO_RAND',3)
                end select
            elseif (me%value_type == val_types(idx_logic)) then
                select type(val)
                    type is (random_logic_type)
                        val%val = me%logic_array(me%index)
                    class default
                        call me%errorcall('DO_RAND',4)
                end select
            else
                call me%errorcall('DO_RAND',5)
            end if 
        elseif (trim(me%field_type) == trim(me%fld_types(me%idx_scale))) then
            call me%genptr%rand(me%randptr, iteration)
                select type(val)
                    type is (random_real_type)
                        val%val = me%scale_value * me%scale_base_value
                    class default
                        call me%errorcall('DO_RAND',6)
                    end select
        elseif (trim(me%field_type) == trim(me%fld_types(me%idx_label))) then
            call me%genptr%rand(me%randptr, iteration)
        else
            call me%errorcall('DO_RAND',7)
        end if 
        
    end subroutine do_rand
    
    function rand_max(me) result(max)
    
        class(random_generator_type) :: me
        real(eb) :: max
        
        if (me%maxdependent) then
            call me%rand_mm_sub1(me%maxptr, max)
        else
            max = me%maxval
        end if
        
    end function rand_max
    
    function rand_min(me) result(min)
    
        class(random_generator_type) :: me
        real(eb) :: min
        
        if (me%mindependent) then
            call me%rand_mm_sub1(me%minptr, min)
        else
            min = me%minval
        end if
        
    end function rand_min
    
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
    
        class(random_generator_type) :: me
        character(len=128) :: field_id
        
        me%minfieldid = field_id
        me%mindependent = .true. 
        
    end subroutine set_min_to_use_field
    
    subroutine set_max_to_use_field(me, field_id)
    
        class(random_generator_type) :: me
        character(len=128) :: field_id
        
        me%maxfieldid = field_id
        me%maxdependent = .true. 
        
    end subroutine set_max_to_use_field
    
    subroutine set_min_value(me, minimum)
        
        class(random_generator_type), target :: me
        real(eb) :: minimum
        
        me%minval = minimum
        me%min_set = .true. 
    
    end subroutine set_min_value
    
    subroutine set_max_value(me, maximum)
        
        class(random_generator_type), target :: me
        real(eb) :: maximum
        
        me%maxval = maximum
        me%max_set = .true.
    
    end subroutine set_max_value
    
    subroutine set_min_field(me, field)
    
        class(random_generator_type), target :: me
        type(field_pointer), target :: field
        
        me%minptr => field
        me%min_set = .true. 
        
    end subroutine set_min_field
    
    subroutine set_max_field(me, field)
    
        class(random_generator_type), target :: me
        class(field_pointer), target :: field
        
        me%maxptr => field
        me%max_set = .true.
        
    end subroutine set_max_field
    
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
    
    function dependencies(me) result(dependent)
    
        class(random_generator_type) :: me
        logical :: dependent
        
        dependent = me%mindependent .or. me%maxdependent
    
    end function dependencies
    
    function dependencies_set(me) result(set)
    
        class(random_generator_type) :: me
        logical :: set
        
        set = me%min_set .or. me%max_set
    
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
    
    function field_dependencies(me) result(dependent)
        
        class(field_pointer) :: me
        logical :: dependent
        
        dependent = me%genptr%dependencies()
    
    end function field_dependencies
    
    function field_dependencies_set(me) result(set)
        
        class(field_pointer) :: me
        logical :: set
        
        set = dependencies_set(me%genptr)
    
    end function field_dependencies_set
    
    function field_min_dependent(me) result(dependent)
        
        class(field_pointer) :: me
        logical :: dependent
        
        dependent = me%genptr%min_dependent()
    
    end function field_min_dependent
    
    function field_max_dependent(me) result(dependent)
        
        class(field_pointer) :: me
        logical :: dependent
        
        dependent = me%genptr%max_dependent()
    
    end function field_max_dependent
    
    function field_min_dependency(me) result(field_id)
        
        class(field_pointer) :: me
        character(len=28) :: field_id
        
        field_id = min_dependency(me%genptr)
    
    end function field_min_dependency
    
    function field_max_dependency(me) result(field_id)
        
        class(field_pointer) :: me
        character(len=28) :: field_id
        
        field_id = max_dependency(me%genptr)
    
    end function field_max_dependency
    
    subroutine field_set_min_value(me, minimum)
    
        class(field_pointer) :: me
        real(eb) :: minimum
        
        call set_min_value(me%genptr, minimum)
        
    end subroutine field_set_min_value
    
    subroutine field_set_max_value(me, maximum)
    
        class(field_pointer) :: me
        real(eb) :: maximum
        
        call set_max_value(me%genptr, maximum)
        
    end subroutine field_set_max_value
    
    subroutine field_set_min_field(me, field)
    
        class(field_pointer) :: me
        type(field_pointer) :: field
        
        call set_min_field(me%genptr, field)
        
    end subroutine field_set_min_field
    
    subroutine field_set_max_field(me, field)
    
        class(field_pointer) :: me
        type(field_pointer) :: field
        
        call set_max_field(me%genptr, field)
        
    end subroutine field_set_max_field
    
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
        
        integer :: i
        
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
                me%fire%t_mdot(i) = me%hrrscalevalue*me%base%t_mdot(i)
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
        flag = .false.
    
    end subroutine copybasetofire
    
    subroutine copytimebasedprop(me, nx, x, xt, nb, b, bt)
    
        class(fire_generator_type) :: me
        integer :: nx, nb
        real(eb), dimension(mxpts) :: x, b, xt, bt
        
        integer :: i
        
        nx = nb
        do i = 1, mxpts
            x(i) = b(i)
            xt(i) = bt(i)
        end do
        
    end subroutine copytimebasedprop
        
    subroutine fire_write_value()
    
    end subroutine fire_write_value
    
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
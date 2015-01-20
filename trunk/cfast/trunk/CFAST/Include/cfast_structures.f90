module cfast_types
    
    use precision_parameters
    use cparams, only: mxpts, ns
    
    !  room data structure
    type room_type
      real(eb) :: yflor, yceil
      real(eb) :: wall_center(10,3)
    end type room_type
    
    ! ramp data structure
    type ramp_type
        character :: type
        integer :: from_room, to_room, vent_number, npoints
        real(eb) :: time(mxpts), value(mxpts)
    end type ramp_type
    
    ! fire data structure

    type fire_type
        
        ! These are the fire definitions from the input
        integer(eb) :: room, object
        real(eb) :: n_C, n_H, n_O, n_N, n_Cl, molar_mass
        real(eb) :: mdot_i(mxpts), qdot_i(mxpts), area_i(mxpts), height_i(mxpts), y_soot_i(mxpts), y_co_i(mxpts), y_trace_i(mxpts)
        
        ! These are calculated values for the current time step
        real(eb) :: x_position, y_position, z_position, area
        real(eb) :: plume_entrained, plume_flow, species_flow(2,ns)
        real(eb) :: hrr_desired, hrr_convective, hrr_radiative, hrr_lower, hrr_upper, hrr_total, heat_of_combustion
    end type fire_type
    
    ! vent data structure

    type vent_type
        real(eb) :: sill, soffit, width
        real(eb) :: from_hall_offset, to_hall_offset
        real(eb) :: wind_dp
        real(eb) :: mflow(2,2,2), mflow_mix(2,2)  ! (1>2 or 2>1, upper or lower, in or out)
        integer :: from, to, counter
        integer :: is_from_hall, is_to_hall
        integer :: face
    end type vent_type
     
end module cfast_types
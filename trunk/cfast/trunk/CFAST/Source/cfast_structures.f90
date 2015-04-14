module cfast_types
    
    use precision_parameters
    use cparams, only: mxpts, ns, mxfslab
    
    !  room data structure
    type room_type
      real(eb) :: yflor, yceil
      real(eb) :: wall_center(10,3)
      real(eb) :: x0, y0, z0
      real(eb) :: x1, y1, z1
      real(eb), allocatable, dimension(:) :: xplt, yplt, zplt
      real(fb), allocatable, dimension(:) :: xpltf, ypltf, zpltf
      real(eb) :: dx, dy, dz
      integer :: ibar, jbar, kbar
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
        ! These define a wall vent
        real(eb) :: sill, soffit, width
        real(eb) :: from_hall_offset, to_hall_offset
        real(eb) :: mflow(2,2,2), mflow_mix(2,2)  ! (1>2 or 2>1, upper or lower, in or out)
        integer :: from, to 
        integer :: face
        
        ! These define a ceiling/floor vent
        real(eb) :: area
        real(eb) :: top, bottom
        
        ! These are common to all vent types
        integer :: counter
        real(eb) :: temp_slab(mxfslab), flow_slab(mxfslab), ybot_slab(mxfslab), ytop_slab(mxfslab)
        integer :: n_slabs
    end type vent_type
    
    ! slice file data structure

    type slice_type
       character(256) :: filename
       character(64) :: menu_label, colorbar_label, unit_label
       real(eb) :: xb(6)
       integer :: ijk(6), roomnum
    end type slice_type
     
    type iso_type
       character(256) :: filename
       character(64) :: menu_label, colorbar_label, unit_label
       integer :: roomnum
       real(eb) :: value
    end type iso_type
    
    type visual_type
        integer :: vtype        ! 1 = 2-D slice, 2 = 3-D slice, 3 = isosurface
        integer :: axis         ! for 2-D slice, axis slice is parallel to, 1 = X, 2 = Y, 3 = Z
        real(eb) :: position    ! for 2-D slice, distance from axis for slice (m)
        real(eb) :: value       ! for isosurface, temperature for surface (K)
        integer :: roomnum      ! compartment
    end type visual_type

   end module cfast_types
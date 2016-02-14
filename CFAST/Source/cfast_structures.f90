module cfast_types
    
    use precision_parameters
    use cparams, only: mxpts, ns, mxfslab, nnodes_trg, mxthrmplen, nwal
    
    !  room data structure
    type room_type
        ! These are room definitions from or calculated from the input
        character(128) :: name                          ! user selected name for the compartment
        character(mxthrmplen), dimension(nwal) :: matl  ! surface materials for ceiling, floor, upper wall, lower wall
                   
        integer :: ibar, jbar, kbar                     ! number of grids in x, y, and z direction in compartment
        integer :: deadroom                             ! if compartment is only connected to a single other compartment
                                                        ! pressure of other compartment is used for the dead compartment
        logical :: hall                                 ! true if compartment is a hallway (modified ceiling jet)
        logical :: shaft                                ! true if compartment is a shaft (one zone calculation)
        logical, dimension(nwal) :: surface_on          ! true if heat conduction is calculated; otherwise adiabatic
        real(eb) :: x0, y0, z0                          ! absolute coordinates of lower left front corner of compartment       
        real(eb) :: width, depth, height                ! width, depth, and height for the compartment
        real(eb) :: x1, y1, z1                          ! absolute coordinate of upper right rear corner of compartment
        real(eb) :: area                                ! compartment floor area
        real(eb) :: volume                              ! compartment volume
        real(eb) :: vmin, vmax                          ! minimum and maximum layer volume for compartment
        real(eb) :: wall_center(3,10)                   ! coordinates of center of each surface in compartment
        real(eb), allocatable, dimension(:) :: xplt, yplt, zplt     ! grid for slice / isosurface files
        real(fb), allocatable, dimension(:) :: xpltf, ypltf, zpltf
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
        real(eb) :: time_i(mxpts), mdot_i(mxpts), qdot_i(mxpts), area_i(mxpts), height_i(mxpts), y_soot_i(mxpts), &
            y_co_i(mxpts), y_trace_i(mxpts)
        
        ! These are calculated values for the current time step
        real(eb) :: x_position, y_position, z_position, area
        real(eb) :: plume_entrained, plume_flow, species_flow(2,ns)
        real(eb) :: hrr_desired, hrr_convective, hrr_radiative, hrr_lower, hrr_upper, hrr_total, heat_of_combustion
    end type fire_type
    
    ! target data structure
    type target_type
        character(128) :: name          ! user selected name for the target
        character(mxthrmplen) :: material ! material for the target (used to match materials properties)
        
        real(eb) :: center(3)           ! position of target center
        real(eb) :: normal(3)           ! target normal vector
        real(eb) :: k                   ! target thermal conductivity (from matching thermal properties input)
        real(eb) :: rho                 ! target density (from matching thermal properties input)
        real(eb) :: cp                  ! target heat capacity (from matching thermal properties input)
        real(eb) :: emissivity          ! target emissivity (from matching thermal properties input)
        real(eb) :: thickness           ! target thickness (from matching thermal properties input)
        real(eb) :: depth_loc           ! depth location for output of internal temperature 
                                        !       (from user input with default of 0.5*thickness)
        real(eb) :: flux_front          ! incident heat flux to front surface of target (calculated)
        real(eb) :: flux_back           ! incident heat flux to back surface of target (calculated)
        real(eb) :: flux_net_front      ! net heat flux to front surface of target (calculated)
        real(eb) :: flux_net_back       ! net heat flux to back surface of target (calculated)
        real(eb), dimension(nnodes_trg) :: temperature  ! target temperatures from front to back
        
        integer :: room                 ! compartment where the target is located (user input)
        integer :: equaton_type         ! equation type for calculation (ODE, PDE) (user input)
        integer :: back                 ! whether the back surface of the target is exposed to interior or exterior temperatures
        integer :: wall                 ! wall surface the target is located on. Normal wall numbering

        ! these are the results of the target calculations that are used for printout and spreadsheet output
        integer :: layer                ! layer (within the compartment) where the target is located (calculated)
        real(eb) :: tgas                ! gas temperature near target
        real(eb) :: tinternal           ! target temperature at depth_loc
        real(eb) :: tfront              ! target front surface temperature (= ...%temperature(1) for plate,                                         
                                        !                                   = ...%temperature(nnodes_trg) for cylinder)
        real(eb) :: tback               ! target back surface temperature  (= ...%temperature(nnodes_trg) for plate, 
                                        !                                   = ...%temperature(1) for cylinder)
        real(eb), dimension(2) :: flux_net, flux_fire, flux_gas, flux_surface, flux_radiation, flux_convection, flux_target
        real(eb), dimension(2) :: flux_net_gauge, flux_radiation_gauge, flux_convection_gauge, flux_target_gauge
        
    end type target_type
    
    ! detector / sprinkler structure
    type detector_type
        real(eb) :: center(3)           ! position of detector center (user input)
        real(eb) :: trigger             ! activation value for detector; % obscuration or temperature (user input)
        real(eb) :: rti                 ! rti value for heat detector or sprinkler (user input)
        real(eb) :: spray_density       ! sprinkler spray density (user input)
        
        integer :: room                 ! compartment where the detector is located (user input)
        integer :: dtype                ! detector type; 1=smoke, 2=heat, 3=sprinkler (user input)
        logical :: quench               ! true if type is sprinkler and spray is non-zero (user input)
        
        ! these are the results of the detector calculations that are used for printout and spreadsheet output
        real(eb) :: value               ! current link temperature or detector obscuration (calculated)
        real(eb) :: value_o             ! link temperature or detector obscuration from previous time step (calculated)
        real(eb) :: temp_gas            ! current gas temperature near detector (calculated)
        real(eb) :: temp_gas_o          ! gas temperature neat detector from previous time step (calculated)
        real(eb) :: velocity            ! current gas velocity near detector (calculated)
        real(eb) :: velocity_o          ! gas velocity near detector from previous time step (calculated)
        real(eb) :: obscuration         ! smoke obscuration near detector (calculated)
        real(eb) :: activation_time     ! time of detector activation (calculated)
        real(eb) :: tau                 ! characteristice quencing time (calculated)
        real(eb) :: half_life           ! time for fire to diminish by a factor of two (calculated)
        logical :: activated            ! true if detector has activated (calculated)
        logical :: reported             ! true if detector activation has already been reported (calculated)
    end type detector_type
    
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
        integer :: shape
        
        ! These define a mechanical vent
        
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
       integer :: ijk(6), roomnum, skip
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
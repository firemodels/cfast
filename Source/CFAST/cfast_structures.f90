module cfast_types

    use precision_parameters
    use cparams, only: mxpts, ns, mxfslab, nnodes_trg, mxthrmplen, nwal, mxcross, mxslb, nnodes, mxrooms, mxcoeff

    ! detector / sprinkler structure
    type detector_type
        character(64) :: name           ! user selected name for the detector (user input)
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

    ! fire data structure
    type fire_type
        ! These are the fire definitions from the input
        character(64) :: name                           ! user selected name for the fire (user input)
        integer :: room                                 ! compartment where the fire is located (user input)
        integer :: ignition_target                      ! target number associated with fire (user input)
        integer :: ignition_type                        ! ignition type for fire (user input)
                                                        ! (1 = time, 2 = temperature, 3 = heat flux)
        real(eb) :: ignition_criterion                  ! ignition criteria for fire. Units depend on ignition type (user input)
        integer :: chemistry_type                       ! fire type. Currently, only constrained fire (user input)
        real(eb) :: n_C, n_H, n_O, n_N, n_Cl            ! stociometry of the fuel (user input)
        real(eb) :: chirad                              ! fraction of fire HRR released as radiation (user input)
        real(eb) :: x_position                          ! initial X position of the base of fire (user input)
        real(eb) :: y_position                          ! initial Y position of the base of fire (user input)
        real(eb) :: z_position                          ! initial Z position of the base of fire (user input)

        integer :: npoints                              ! actual number of time points for fire (user input)
        real(eb), dimension(mxpts) :: time              ! time points for fire inputs (user input)
        real(eb), dimension(mxpts) :: mdot              ! pyrolysis rate as a function of time (user input)
        real(eb), dimension(mxpts) :: qdot              ! heat release rate of the fire as a function of time (user input)
        real(eb), dimension(mxpts) :: area              ! area of the base of the fire as a function of time (user input)
        real(eb), dimension(mxpts) :: height            ! height of the base of the fire as a function of time (user input)
        real(eb), dimension(mxpts) :: y_soot            ! soot production rate as a funciton of time (user input)
        real(eb), dimension(mxpts) :: y_co              ! CO production rate as a function of time (user input)
        real(eb), dimension(mxpts) :: y_trace           ! trace species production rate as a funciton of time (user input)
        real(eb), dimension(mxpts) :: hoc               ! heat of combustion as a function of time

        real(eb) :: z_offset                            ! current height of the fire above initial Z position
        real(eb) :: characteristic_length               ! characteristic length for fire = max fire diameter
        real(eb) :: molar_mass                          ! molar mass calculated from fuel composition
        real(eb) :: ignition_time                       ! time at ignition of fire, default is 0
        logical :: ignited                              ! true if fire has ignited
        logical :: reported                             ! true if fire has ignited and has already been reported (calculated)
        integer :: modified_plume                       ! fire plume flag, 1 = center, 2 = wall, 3 = corner

        ! These are calculated results for the current time step
        real(eb) :: firearea                            ! area of the base of the fire
        real(eb) :: mdot_trace                          ! trace species production rate
        real(eb) :: mdot_pyrolysis                      ! mass pyrolysis rate of the fire
        real(eb) :: mdot_entrained                      ! mass entrainment rate into the plume
        real(eb) :: mdot_plume                          ! mass rate from plume into upper layer = pyrolysis + entrained

        real(eb) :: total_pyrolysate                    ! total pyroysate released by fire up to the current time
        real(eb) :: total_trace                         ! total trace species released by fire up to the current time

        real(eb) :: qdot_actual                         ! actual HRR (limited by available oxygen)
        real(eb) :: qdot_radiative                      ! actual radiative HRR = qdot_actual * chirad
        real(eb) :: qdot_convective                     ! actual convective HRR = qdot_actual * (1 - chirad)
        real(eb), dimension(2) :: qdot_at_activation    ! HRR at sprinkler activation (1=upper layer, 2=lower layer)
        real(eb), dimension(2) :: qdot_layers           ! HRR into each layer (1=upper layer, 2=lower layer)

        real(eb) :: temperature                         ! surface temperature on attached target (only for ignition)
        real(eb) :: incident_flux                       ! flux to attached target (only for ignition)
    end type fire_type

    ! ramp data structure
    type ramp_type
        character(64) :: id  
        character(64) :: type  
        integer :: room1, room2, counter, npoints
        real(eb) :: time(mxpts), value(mxpts)
    end type ramp_type

    ! room data structure
    type room_type
        ! These are room definitions from or calculated from user input
        character(64) :: name                           ! user selected name for the compartment
        character(64), dimension(nwal) :: matl          ! surface materials for ceiling, floor, upper wall, lower wall

        integer :: compartment                          ! compartment number assigned automatically for namelist inputs
        integer :: ibar, jbar, kbar                     ! number of grids in x, y, and z direction in compartment
        integer :: deadroom                             ! if compartment is only connected to a single other compartment
                                                        ! pressure of other compartment is used for the dead compartment
        logical :: hall                                 ! true if compartment is a hallway (modified ceiling jet)
        logical :: shaft                                ! true if compartment is a shaft (one zone calculation)
        logical, dimension(nwal) :: surface_on          ! true if heat conduction is calculated; otherwise adiabatic
        real(eb) :: x0, y0, z0                          ! absolute coordinates of lower left front corner of compartment
        real(eb) :: cwidth, cdepth, cheight             ! width, depth, and height for the compartment
        real(eb) :: x1, y1, z1                          ! absolute coordinate of upper right rear corner of compartment
        real(eb) :: floor_area                          ! compartment floor area
        real(eb) :: cvolume                             ! compartment volume
        real(eb), allocatable, dimension(:) :: xplt, yplt, zplt     ! grid for slice / isosurface files
        real(fb), allocatable, dimension(:) :: xpltf, ypltf, zpltf
        real(eb) :: vmin, vmax                          ! minimum and maximum layer volume for compartment
        real(eb) :: wall_center(3,10)                   ! coordinates of center of each surface in compartment
        real(eb) :: interior_relp_initial               ! initial value of interior pressure relative to minimum pressure
        real(eb) :: exterior_relp_initial               ! initial value of exterior pressure relative to minimum pressure
        logical :: is_connection                        ! true if there is a natural flow vent connection in the room that
                                                        ! connects to the outside (perhaps through other intermediate rooms)
        logical :: is_hvac                              ! true if there is an HVAC vent connection in the room

        ! cross-sectional area variables
        integer :: nvars                                ! number of data points for variable cross-secitonal area
        real(eb), dimension(mxcross) :: var_volume      ! variable cross-secitonal area volume from floor to var_height(i)
        real(eb), dimension(mxcross) :: var_area        ! variable cross-sectional area base area
        real(eb), dimension(mxcross) :: var_height      ! variable cross-sectional area heights

        ! compartment surfaces
        real(eb), dimension(nwal) :: eps_w              ! emissivity of wall surface
        real(eb), dimension(nwal) :: total_thick_w      ! total thickness of wall
        integer, dimension(nwal) :: nslab_w             ! number of slabs for wall
        real(eb), dimension(mxslb,nwal) :: k_w          ! thermal conductivity of each slab
        real(eb), dimension(mxslb,nwal) :: c_w          ! specific heat of each slab
        real(eb), dimension(mxslb,nwal) :: rho_w        ! density of each slab
        real(eb), dimension(mxslb,nwal) :: thick_w      ! thickness of each slab
        integer, dimension(mxslb+1,nwal) :: nodes_w     ! number of nodes in each slab
        real(eb), dimension(nnodes,nwal) :: walldx      ! thickness of each node in each slab

        integer :: nheats                               ! number of horizontal heat transfer connections to this room
        integer :: iheat                                ! type of horizontal heat connections to this room
                                                        !   1 = all rooms connected here by vents
                                                        !   2 = only user specified vents
        integer, dimension(mxrooms) :: hheat_connections! list of connected compartments for horizontal heat transfer
        real(eb), dimension(mxrooms) :: heat_frac       ! fractions of wall surface of this room connected to other rooms in list


        ! These are calculated results for the current time step
        real(eb) :: relp                                ! pressure at floor level relative to exterior
        real(eb) :: absp                                ! absolute pressure at floor level
        real(eb), dimension(2) :: volume                ! volume of each layer
        real(eb), dimension(2) :: depth                 ! thickness of each layer
        real(eb), dimension(2) :: temp                  ! temperature of each layer
        real(eb), dimension(2) :: rho                   ! density of each layer
        real(eb), dimension(2) :: mass                  ! total mass of each layer
        real(eb), dimension(2) :: abs_length            ! characteristic length for absorbtivity in each layer
        real(eb), dimension(2) :: absorb                ! layer absorbtivity

        integer :: sprinkler_activated                  ! sprinkler number that fist activated in compartment

        real(eb), dimension(2,ns) :: species_mass       ! mass of species in each layer
        real(eb), dimension(2,ns) :: species_fraction   ! mass fraction of species in each layer
        real(eb), dimension(2,ns) :: species_rho        ! density of species in each layer
        real(eb), dimension(2,ns) :: species_output     ! species converted to output units

        real(eb), dimension(4) :: wall_area4            ! area of 4 compartment surfaces (ceiling, upper wall, lower wall, floor)
        real(eb), dimension(10) :: wall_area10          ! area of 10 wall surfaces (ceiling, 4 upper walls, 4 lower walls, floor)
        real(eb), dimension(nnodes,nwal) :: t_profile   ! temperature profile within compartment surfaces
        real(eb), dimension(2,nwal) :: t_surfaces       ! compartment surface temperatures (interior, exterior)
        real(eb), dimension(nwal) :: rad_qout           ! flux radiated from compartment surfaces
        real(eb) :: qdot_doorjet                        ! HRR of door jet fires at the current time

    end type room_type

    ! target data structure
    type target_type
        character(64) :: name           ! user selected name for the target
        character(64) :: material       ! material for the target (used to match materials properties)

        real(eb) :: center(3)           ! position of target center
        real(eb) :: normal(3)           ! target normal vector
        real(eb) :: k                   ! target thermal conductivity (from matching thermal properties input)
        real(eb) :: rho                 ! target density (from matching thermal properties input)
        real(eb) :: c                   ! target specific heat (from matching thermal properties input)
        real(eb) :: emissivity          ! target emissivity (from matching thermal properties input)
        real(eb) :: thickness           ! target thickness (from matching thermal properties input)
        real(eb) :: depth_loc           ! depth location for output of internal temperature
                                        !       (from user input with default of 0.5*thickness)
        real(eb) :: flux_incident_front ! incident heat flux to front surface of target (calculated)
        real(eb) :: flux_incident_back  ! incident heat flux to back surface of target (calculated)
        real(eb) :: flux_net_front      ! net heat flux to front surface of target (calculated)
        real(eb) :: flux_net_back       ! net heat flux to back surface of target (calculated)
        real(eb), dimension(nnodes_trg) :: temperature  ! target temperatures from front to back

        integer :: room                 ! compartment where the target is located (user input)
        integer :: equaton_type         ! equation type for calculation (ODE, PDE) (user input)
        integer :: back                 ! whether the back surface of the target is exposed to interior or exterior temperatures
        integer :: wall                 ! wall surface the target is located on. Normal wall numbering

        ! These are calculated results for the current time step
        integer :: layer                ! layer (within the compartment) where the target is located (calculated)
        real(eb) :: tgas                ! gas temperature near target
        real(eb) :: tinternal           ! target temperature at depth_loc
        real(eb) :: fed_gas             ! accumulated gas tenability at target location
        real(eb) :: dfed_gas            ! current accumulated increment (since last ss output) of gas tenability at target location
        real(eb) :: fed_heat            ! accumulated heat tenability at target location
        real(eb) :: dfed_heat           ! current accumulated increment (since last ss output) of heat tenability at target location
        real(eb) :: tfront              ! target front surface temperature (= ...%temperature(1) for plate,
                                        !                                   = ...%temperature(nnodes_trg) for cylinder)
        real(eb) :: tback               ! target back surface temperature  (= ...%temperature(nnodes_trg) for plate,
                                        !                                   = ...%temperature(1) for cylinder)
        real(eb), dimension(2) :: flux_net, flux_fire, flux_gas, flux_surface, flux_radiation, flux_convection, flux_target
        real(eb), dimension(2) :: flux_net_gauge, flux_radiation_gauge, flux_convection_gauge, flux_target_gauge

    end type target_type

    ! thermal properties structure
    type thermal_type
        character(64) :: name                           ! user selected name for the material
        integer :: nslab                                ! number of slabs
        real(eb), dimension(mxslb) :: k                 ! thermal conductivity of each slab
        real(eb), dimension(mxslb) :: rho               ! density of each slab
        real(eb), dimension(mxslb) :: c                 ! specific heat of each slab
        real(eb), dimension(mxslb) :: thickness         ! thickness of each slab
        real(eb) :: eps                                 ! surface emissivity
    end type thermal_type

    ! vent data structure
    type vent_type
        ! These define a wall vent
        real(eb) :: sill                                ! height of vent bottom relative to compartment floor
        real(eb) :: soffit                              ! height of vent top relative to compartment floor
        real(eb) :: width                               ! width of sill
        integer :: face                                 ! wall where went is located, 1 = x(-), 2 = y(+), 3 = x(+), 4 = y(-)
        real(eb) :: absolute_sill                       ! absolute height of the sill
        real(eb) :: absolute_soffit                     ! absolute height of the soffit
        real(eb), dimension(2) :: offset                ! vent offset from wall origin (1 = from room, 2 = to room)

        real(eb) :: h_mflow(2,2,2), h_mflow_mix(2,2)    ! (1>2 or 2>1, u or l, in or out)

        ! These define a ceiling/floor vent
        integer :: shape                                ! vent shape, 1 = circular, 2 = square (from user input)

        ! These define a mechanical vent
        integer :: orientation(2)                       ! orientation of vent diffusers (1 = V, 2 = H)
        real(eb) :: height(2)                           ! center height of vent diffusers
        real(eb) :: diffuser_area(2)                    ! cross-sectional area of vent diffusers
        integer :: n_coeffs                             ! number of fan coefficients for this fan (currently set to 1 in input.f90)
        real, dimension(mxcoeff) :: coeff               ! coefficients of fan curve, flow vs pressure
        real(eb) :: maxflow                             ! peak specified fan flow in mv system (m^3/s)
        real(eb) :: min_cutoff_relp                     ! pressure at beginning of fan cutoff; full flow below this pressure
        real(eb) :: max_cutoff_relp                     ! pressure and end of fan cutoff; flow is zero above this pressure
        real(eb) :: filter_initial_time                 ! beginning time for filter fraction change
        real(eb) :: filter_initial_fraction             ! beginning fraction for filter (filter fraction up to initial time)
        real(eb) :: filter_final_time                   ! ending time for filter fraction change
        real(eb) :: filter_final_fraction               ! final fraction for filter (filter fraction after final time)

        real(eb) :: relp                                ! pressure difference across vent (room2 - room1)
        real(eb), dimension(2) :: mv_mflow              ! vent mass flow at compartment connection (u,l)
        real(eb), dimension(2) :: temp                  ! temperature at compartment connection (u,l)
        real(eb), dimension(2) :: flow_fraction         ! fraction of flow to or from each layer (<-> u, <-> l)
        real(eb), dimension(2) :: total_flow            ! total mass flow at compartment connection (<-> u, <-> l)
        real(eb), dimension(2) :: total_trace_flow      ! total trace species flow up to current time  (u,l)
        real(eb), dimension(2) :: total_trace_filtered  ! total trace species filtered out up to current time  (u,l)
        real(eb), dimension(2,ns) :: species_fraction   ! species fraction at compartment connection (<-> u, <-> l)

        ! These are common to more than one vent types

        character(64) :: ramp_id            ! ramp id assocated with vent
        character(64) :: filter_id            ! filter id assocated with vent
        integer :: room1                    ! first or top compartment for connecting vent
        integer :: room2                    ! second or bottom compartment for connecting vent
        integer :: counter                  ! counter for vents connecting the same two compartments, 1, 2, ...
        integer :: opening_target           ! target number associated with vent (user input)
        integer :: opening_type             ! open/close type for fire (user input)
                                            ! (1 = time, 2 = temperature, 3 = heat flux)
        real(eb) :: opening_criterion       ! open/close criteria for vent change based on temperature or flux
        logical :: opening_triggered        ! true if opening_criterion has been met
        real(eb) :: opening_temperature     ! current temeprature of target associate with vent
        real(eb) :: opening_flux            ! current incident flux of target associate with vent
        real(eb) :: opening_initial_time    ! beginning time of vent opening fraction change
        real(eb) :: opening_initial_fraction! beginning fraction for vent opening (vent fraction up to initial time)
        real(eb) :: opening_final_time      ! ending time for vent opening fraction change
        real(eb) :: opening_final_fraction  ! final fraction for vent opening (vent fraction after final time)
                                            ! between initial and final, open fraction changes linearly
        real(eb) :: area                    ! cross-sectional area of vent
        real(eb) :: xoffset                 ! offset from origin to vent center in width (x) direction
        real(eb) :: yoffset                 ! offset from origin to vent center in depth (y) direction

        ! These are calculated results for the current time step

        real(eb) :: current_area                        ! vent area at current time step accounting for opening fraction
        real(eb) :: mflow(2,2)                          ! vent mass flow (room1/top,room2/bottom, u,l)

        integer :: n_slabs
        real(eb) :: temp_slab(mxfslab), flow_slab(mxfslab), ybot_slab(mxfslab), ytop_slab(mxfslab)
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
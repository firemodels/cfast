module cfast_types

    use precision_parameters
    
    use cparams, only: mxpts, ns, mxfslab, nnodes_trg, mxthrmplen, nwal, mxpts, mxslb, nnodes, mxrooms, &
        mxcoeff, mxtablcols
    
    ! base type that all other types extend
    type cfast_type
        character(len=128) :: id                    ! user selected heading for output column
        character(len=128) :: fyi                   ! line available for comments or extra input
    end type cfast_type
    
    ! user-customized data and calculation output data structure
    type, extends(cfast_type) :: dump_type
        ! these are for user-specified calculations on data. output goes to _calculations.csv
        character(len=24) :: file                   ! 'compartments', 'devices', 'masses', 'vents', or 'walls'
        character(len=24) :: type                   ! 'trigger_greater', 'trigger_lesser', 'minimum', 'maximum', 'integrate', 
                                                    !      'check_total_hrr'
        real(eb) :: criterion                       ! Value used in 'trigger_...' analysis

        character(len=128) :: first_field(2)        ! Name of instrument, third row in spreadsheet and 
                                                    !   name of measurement for first device, second row in spreadsheet
        character(len=128) :: second_field(2)       ! Name of second instrument and second measurement, 
                                                    !   needed for 'trigger' and 'integrate',
                                                    !   ignored for 'maximum', 'minimum', and 'check_total_hrr'
        integer :: relative_column                  ! Order of columns. This is just the order in the input file
        
        logical :: found                            ! The input channels are found in the requested csv files
    end type dump_type

    ! detector / sprinkler structure
    type, extends(cfast_type) :: detector_type
        real(eb) :: center(3)           ! position of detector center (user input)
        real(eb) :: trigger             ! activation value for detector; % obscuration or temperature (user input)
        real(eb) :: trigger_smolder     ! activation value for dual_detector smoke for the smoldering smoke % obscuration 
                                        !     (user input)
        real(eb) :: rti                 ! rti value for heat detector or sprinkler (user input)
        real(eb) :: spray_density       ! sprinkler spray density (user input)

        integer :: room                 ! compartment where the detector is located (user input)
        character(len=128) :: room_id   ! compartment id
        integer :: dtype                ! detector type; 1=smoke, 2=heat, 3=sprinkler (user input)
        logical :: quench               ! true if type is sprinkler and spray is non-zero (user input)

        ! these are the results of the detector calculations that are used for printout and spreadsheet output
        real(eb) :: value               ! current link temperature or detector obscuration (calculated)
        real(eb) :: value_smolder       ! current detectore obscuration due to smoldering smoke (calculated)
        real(eb) :: value_o             ! link temperature or detector obscuration from previous time step (calculated)
        real(eb) :: value_o_smolder     ! detector obscuration from previous time step for smoldering smoke (calculated)
        real(eb) :: temp_gas            ! current gas temperature near detector (calculated)
        real(eb) :: temp_gas_o          ! gas temperature near detector from previous time step (calculated)
        real(eb) :: velocity            ! current gas velocity near detector (calculated)
        real(eb) :: velocity_o          ! gas velocity near detector from previous time step (calculated)
        real(eb) :: obscuration         ! total smoke obscuration near detector (calculated)
        real(eb) :: obscuration_flaming ! smoke obscuration due to flaming fires (calculated)
        real(eb) :: obscuration_smolder ! smoke obscuration due to smoldering fires (calculated)
        real(eb) :: activation_time     ! time of detector activation (calculated)
        real(eb) :: tau                 ! characteristice quencing time (calculated)
        real(eb) :: half_life           ! time for fire to diminish by a factor of two (calculated)
        logical :: activated            ! true if detector has activated (calculated)
        logical :: reported             ! true if detector activation has already been reported (calculated)
        logical :: dual_detector        ! true if smoke detector with different triggers for flaming and smoldering smoke 
                                        !                 (determined based on user input)
    end type detector_type

    ! fire data structure
    type, extends(cfast_type) :: fire_type
        ! These are the fire definitions from the input
        character(len=128) :: fire_id                   ! user selected name for the data associated with this fire instance
        integer :: room                                 ! compartment where the fire is located (user input)
        integer :: ignition_target                      ! target number associated with fire (user input)
        integer :: ignition_type                        ! ignition type for fire (user input)
                                                        ! (1 = time, 2 = temperature, 3 = heat flux)
        real(eb) :: ignition_criterion                  ! ignition criterion for fire. Units depend on ignition type (user input)
        integer :: chemistry_type                       ! fire type. Currently, only constrained fire (user input)
        real(eb) :: n_C, n_H, n_O, n_N, n_Cl            ! stociometry of the fuel (user input)
        real(eb) :: chirad                              ! fraction of fire HRR released as radiation (user input)
        real(eb) :: x_position                          ! initial X position of the base of fire (user input)
        real(eb) :: y_position                          ! initial Y position of the base of fire (user input)
        real(eb) :: z_position                          ! initial Z position of the base of fire (user input)
        real(eb) :: flaming_transition_time             ! time in sec at which fire transitions to flaming, realtive to fire start. 
                                                        !                default is 0 (user input)

        integer :: n_mdot, n_qdot, n_area, n_height, n_soot, n_co, n_hcn, n_trace, n_hoc ! number of time points (user input)
        real(eb), dimension(mxpts) :: mdot, t_mdot      ! pyrolysis rate (user input)
        real(eb), dimension(mxpts) :: qdot, t_qdot      ! heat release rate of the fire (user input)
        real(eb), dimension(mxpts) :: area, t_area      ! area of the base of the fire (user input)
        real(eb), dimension(mxpts) :: height, t_height  ! height of the base of the fire (user input)
        real(eb), dimension(mxpts) :: y_soot, t_soot    ! soot production rate (user input)
        real(eb), dimension(mxpts) :: y_co, t_co        ! CO production rate (user input)
        real(eb), dimension(mxpts) :: y_hcn, t_hcn      ! HCN production rate (user input)
        real(eb), dimension(mxpts) :: y_trace, t_trace  ! trace species production rate (user input)
        real(eb), dimension(mxpts) :: hoc, t_hoc        ! heat of combustion

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

        real(eb) :: qdot_theoretical                    ! HRR as input by the user, unmodified by available oxygen or sprinklers
        real(eb) :: qdot_actual                         ! actual HRR (limited by available oxygen)
        real(eb) :: qdot_radiative                      ! actual radiative HRR = qdot_actual * chirad
        real(eb) :: qdot_convective                     ! actual convective HRR = qdot_actual * (1 - chirad)
        real(eb), dimension(2) :: qdot_at_activation    ! HRR at sprinkler activation (1=upper layer, 2=lower layer)
        real(eb), dimension(2) :: qdot_layers           ! HRR into each layer (1=upper layer, 2=lower layer)

        real(eb) :: temperature                         ! surface temperature on attached target (only for ignition)
        real(eb) :: incident_flux                       ! flux to attached target (only for ignition)
        
        logical :: CData_modifying_fire_flag            ! Flag for CData
        
    contains
        procedure :: pop_table
    end type fire_type

    ! ramp data structure
    type, extends(cfast_type) :: ramp_type
        character(len=128) :: type  
        integer :: room1, room2, counter, npoints
        real(eb) :: x(mxpts), f_of_x(mxpts)
    end type ramp_type

    ! room data structure
    type, extends(cfast_type) :: room_type
        ! These are room definitions from or calculated from user input

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
        
        real(eb), dimension(2) :: leak_area_ratios      ! leakage area ratio in m^2/m^2; (1) walls and (2) floor
        real(eb), dimension(2) :: leak_areas            ! leakage area in m^2; (1) walls and (2) floor

        ! cross-sectional area variables
        integer :: nvars                                ! number of data points for variable cross-secitonal area
        real(eb), dimension(mxpts) :: var_volume        ! variable cross-secitonal area volume from floor to var_height(i)
        real(eb), dimension(mxpts) :: var_area          ! variable cross-sectional area base area
        real(eb), dimension(mxpts) :: var_height        ! variable cross-sectional area heights

        ! compartment surfaces
        real(eb), dimension(nwal) :: eps_w              ! emissivity of wall surface
        real(eb), dimension(nwal) :: total_thick_w      ! total thickness of wall
        integer, dimension(nwal) :: nslab_w             ! number of slabs for wall
        character(128), dimension(mxslb,nwal) :: matl   ! surface materials for ceiling, floor, upper wall, lower wall
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
        real(eb), dimension(10) :: chi                  ! surface opening ratio of a particular surface based on 10-wall model
        integer, dimension(mxrooms) :: room_connections ! list of connected compartments, number of compartments to travel through
                                                        ! to get to each compartment from the current compartment


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
        real(eb) :: qdot_doorjet                        ! HRR of vent jet fires at the current time
    end type room_type
    
    ! time-dependent fire parameters table input data structure
    type, extends(cfast_type) :: table_type
        character(len=128), dimension(ns+3) :: labels   ! column labels for columns of data in the table
        real(eb), dimension(mxpts,ns+3) :: data         ! actual input data for the table
        integer :: n_points                             ! number of data points (rows) in the table
        integer :: n_columns                            ! number of columns of data in the table
    end type table_type

    ! target data structure
    type, extends(cfast_type) :: target_type
        character(len=128) :: material      ! material for the target (used to match materials properties)
        character(len=128) :: depth_units   ! specify units for temperature depth location, 'FRACTION' or 'M'
                                            ! default is 'FRACTION' for backwards compatibility
        character(len=128) :: surface_orientation
        
        real(eb) :: surface_temperature   ! fixed front surface temperature for calculation of gauge heat flux.
        real(eb) :: center(3)           ! position of target center
        real(eb) :: normal(3)           ! target normal vector
        real(eb) :: k                   ! target thermal conductivity (from matching thermal properties input)
        real(eb) :: rho                 ! target density (from matching thermal properties input)
        real(eb) :: c                   ! target specific heat (from matching thermal properties input)
        real(eb) :: emissivity          ! target emissivity (from matching thermal properties input)
        real(eb) :: thickness           ! target thickness (from matching thermal properties input)
        real(eb) :: depth_loc           ! depth location for output of internal temperature
                                        !       (from user input with default of 0.5*thickness)

        integer :: room                 ! compartment where the target is located (user input)
        character(len=128) :: room_id   ! compartment id 
        integer :: equaton_type         ! equation type for calculation (ODE, PDE) (user input)
        integer :: back                 ! whether the back surface of the target is exposed to interior or exterior temperatures
        integer :: wall                 ! wall surface the target is located on. Normal wall numbering

        ! These are calculated results for the current time step
        real(eb) :: flux_incident_front ! incident heat flux to front surface of target (calculated)
        real(eb) :: flux_incident_back  ! incident heat flux to back surface of target (calculated)
        real(eb) :: flux_net_front      ! net heat flux to front surface of target (calculated)
        real(eb) :: flux_net_back       ! net heat flux to back surface of target (calculated)
        real(eb), dimension(nnodes_trg) :: temperature  ! target temperatures from front to back
        
        integer :: layer                ! layer (within the compartment) where the target is located (calculated)
        real(eb) :: tgas                ! gas temperature near target
        real(eb) :: tinternal           ! target temperature at depth_loc
        real(eb) :: fed_gas             ! accumulated gas tenability at target location
        real(eb) :: dfed_gas            ! current accumulated increment (since last ss output) of gas tenability at target location
        real(eb) :: fed_heat            ! accumulated heat tenability at target location
        real(eb) :: dfed_heat           ! current accumulated increment (since last ss output) of heat tenability at target location
        real(eb) :: fed_obs             ! current smoke obscuration at target location
        real(eb) :: tfront              ! target front surface temperature (= ...%temperature(1) for plate,
                                        !                                   = ...%temperature(nnodes_trg) for cylinder)
        real(eb) :: tback               ! target back surface temperature  (= ...%temperature(nnodes_trg) for plate,
                                        !                                   = ...%temperature(1) for cylinder)
        real(eb), dimension(2) :: flux_net, flux_fire, flux_gas, flux_surface, flux_radiation, flux_convection, flux_target
        real(eb), dimension(2) :: flux_net_gauge, flux_radiation_gauge, flux_convection_gauge, flux_target_gauge
        real(eb), dimension(2) :: h_conv ! user-defined convective heat transfer coefficient for adiabatic surface temperature
        logical  :: adiabatic           ! true if target is adiabatic
    end type target_type

    ! thermal properties structure
    type, extends(cfast_type) :: material_type
        character(len=128) :: material                  ! long descripter for material 
        integer :: nslab                                ! number of slabs
        real(eb), dimension(mxslb) :: k                 ! thermal conductivity of each slab
        real(eb), dimension(mxslb) :: rho               ! density of each slab
        real(eb), dimension(mxslb) :: c                 ! specific heat of each slab
        real(eb), dimension(mxslb) :: thickness         ! thickness of each slab
        real(eb) :: eps                                 ! surface emissivity
    end type material_type

    ! vent data structure
    type, extends(cfast_type) :: vent_type
        character(len=1) :: vtype           ! 'H', 'V'. or 'M', set in initialization
        character(len=128) :: ramp_id       ! ramp id assocated with vent
        integer :: room1                    ! first or top compartment for connecting vent
        integer :: room2                    ! second or bottom compartment for connecting vent
        integer :: counter                  ! counter for vents connecting the same two compartments, 1, 2, ...
        integer :: opening_target           ! target number associated with vent (user input)
        integer :: opening_type             ! open/close type for fire (user input)
                                            ! (1 = time, 2 = temperature, 3 = heat flux)
        real(eb) :: opening_criterion       ! open/close criterion for vent change based on temperature or flux
        logical :: opening_triggered        ! true if opening_criterion has been met
        real(eb) :: opening_temperature     ! current temeprature of target associate with vent
        real(eb) :: opening_flux            ! current incident flux of target associate with vent
        real(eb) :: opening_initial_time    ! beginning time of vent opening fraction change
        real(eb) :: opening_initial_fraction! beginning fraction for vent opening (vent fraction up to initial time)
        real(eb) :: opening_final_time      ! ending time for vent opening fraction change
        real(eb) :: opening_final_fraction  ! final fraction for vent opening (vent fraction after final time)
                                            ! between initial and final, open fraction changes linearly
        real(eb), dimension(mxpts) :: t, f  ! times in t and fraction open of vent in f. For opening on TEMP and FLUX
                                            ! f(1) and f(2) are used for opening_initial_fraction and opening_final_fraction
        integer :: npoints                  ! number of points in arrays t, and f
        real(eb) :: area                    ! cross-sectional area of vent
        real(eb) :: xoffset                 ! offset from origin to vent center in width (x) direction
        real(eb) :: yoffset                 ! offset from origin to vent center in depth (y) direction
        
        ! These define a wall vent
        real(eb) :: sill                                ! height of vent bottom relative to compartment floor
        real(eb) :: soffit                              ! height of vent top relative to compartment floor
        real(eb) :: width                               ! width of sill
        integer :: face                                 ! wall where went is located, 1 = x(-), 2 = y(+), 3 = x(+), 4 = y(-)
        real(eb) :: absolute_sill                       ! absolute height of the sill
        real(eb) :: absolute_soffit                     ! absolute height of the soffit
        real(eb), dimension(2) :: offset                ! vent offset from wall origin (1 = from room, 2 = to room)

        real(eb) :: h_mflow(2,2,2), h_mflow_mix(2,2)    ! (1>2 or 2>1, u or l, in or out)
        
        integer :: n_slabs
        real(eb) :: temp_slab(mxfslab), flow_slab(mxfslab), ybot_slab(mxfslab), ytop_slab(mxfslab)

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
        real(eb), dimension(2) :: temp                  ! temperature at compartment connection (u,l)
        real(eb), dimension(2) :: flow_fraction         ! fraction of flow to or from each layer (<-> u, <-> l)
        real(eb), dimension(2) :: total_flow            ! total mass flow at compartment connection (<-> u, <-> l)
        real(eb), dimension(2) :: total_trace_flow      ! total trace species flow up to current time  (u,l)
        real(eb), dimension(2) :: total_trace_filtered  ! total trace species filtered out up to current time  (u,l)
        real(eb), dimension(2,ns) :: species_fraction   ! species fraction at compartment connection (<-> u, <-> l)

        ! These are calculated results for the current time step

        real(eb) :: opening_fraction        ! fraction vent is open at current time step (0 --> 1)
        real(eb) :: filter_fraction         ! fraction of trace and soot that get filtered out at the current time step (0 --> 1)
        real(eb) :: current_area            ! vent area at current time step accounting for opening fraction
        real(eb) :: mflow(2,2)              ! vent mass flow (room1/top,room2/bottom, u,l)
    end type vent_type

    ! output and visualization data structure
    type ssout_type
        character(len=128) :: short         ! short name for output column that includes both device and measurement
        character(len=128) :: measurement   ! identifies which measurement within a device (i.e., upper layer temperature)
        character(len=128) :: device        ! identifies where the measurements are coming from (i.e., Room 1 or Target 1)
        character(len=128) :: units         ! identifies the measurement units for the output
    end type ssout_type
    
    type slice_type
       character(len=256) :: filename
       character(len=64) :: menu_label, colorbar_label, unit_label
       real(eb) :: xb(6)
       integer :: ijk(6), roomnum, skip
    end type slice_type

    type iso_type
       character(len=256) :: filename
       character(len=64) :: menu_label, colorbar_label, unit_label
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
    
    contains 
    
        subroutine pop_table(me, table)
            class(fire_type) :: me
            type(table_type), intent(in) :: table
            
            integer :: np, i, tidx
            
            np = table%n_points
            tidx = 0
            time_label_loop: do i = 1, mxtablcols
                if (trim(table%labels(i)) == 'TIME') then
                    tidx = i
                    exit time_label_loop
                end if
            end do time_label_loop
            if (tidx <= 0) then
                stop 'ERROR in pop_table in cfast_structures'
            end if 

            do i = 1, mxtablcols
                select case (trim(table%labels(i)))
                case ('TIME')
                    ! place holder at this point. 
                case ('HRR')
                    me%qdot(1:np) = table%data(1:np,i)*1000._eb
                    me%t_qdot(1:np) = table%data(1:np,tidx)
                    me%n_qdot = np
                case ('HEIGHT')
                    me%height(1:np) = table%data(1:np,i)
                    me%t_height(1:np) = table%data(1:np,tidx)
                    me%n_height = np
                case ('AREA')
                    me%area(1:np) = max(table%data(1:np,i),pio4*0.2_eb**2)
                    me%t_area(1:np) = table%data(1:np,tidx)
                    me%n_area = np
                case ('CO_YIELD')
                    me%y_co(1:np) = table%data(1:np,i)
                    me%t_co(1:np) = table%data(1:np,tidx)
                    me%n_co = np
                case ('SOOT_YIELD')
                    me%y_soot(1:np) = table%data(1:np,i)
                    me%t_soot(1:np) = table%data(1:np,tidx)
                    me%n_soot = np
                case ('HCN_YIELD')
                    me%y_hcn(1:np) = table%data(1:np,i)
                    me%t_hcn(1:np) = table%data(1:np,tidx)
                    me%n_hcn = np
                case ('HCL_YIELD')
                    ! with nothing here, all chlorine in the fuel is assumed to go to HCl.
                case ('TRACE_YIELD')
                    me%y_trace(1:np) = table%data(1:np,i)
                    me%t_trace(1:np) = table%data(1:np,tidx)
                    me%n_trace = np
                end select
            end do
        
        end subroutine pop_table

end module cfast_types

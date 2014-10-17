module cfast_types
    
    use precision_parameters
    use cparams, only: nv, ns
    
    !  room data structure
    type room_type
      real(eb) :: yflor, yceil
      real(eb) :: wall_center(10,3)
    end type room_type
    
    ! ramp data structure
    type ramp_type
        character :: type
        integer :: from_room, to_room, vent_number, npoints
        real(eb) :: time(nv), value(nv)
    end type ramp_type
    
        
!            xfire(nfire,1) = objpos(1,iobj)
!            xfire(nfire,2) = objpos(2,iobj)
!            xfire(nfire,3) = objpos(3,iobj) + ohight
!            xfire(nfire,4) = oplume(3,iobj)
!            xfire(nfire,5) = oplume(1,iobj)
!            xfire(nfire,6) = oplume(2,iobj)
!            xfire(nfire,7) = qfc(1,iroom)
!            xfire(nfire,8) = xqfr
!            xfire(nfire,9) = heatlp(iroom) + heatup(iroom)
!            xfire(nfire,10) = heatlp(iroom)
!            xfire(nfire,11) = heatup(iroom)
!            xfire(nfire,12) = objhct
!            xfire(nfire,13) = y_soot
!            xfire(nfire,14) = y_co
!            !xfire(nfire,15) = hcratt
!            !xfire(nfire,16) = ocratt
!            !xfire(nfire,17) = clfrat
!            !xfire(nfire,18) = cnfrat
!            xfire(nfire,19) = objclen(iobj)
!            xfire(nfire,20) = oareat

    ! fire data structure

    type fire_type
        
        ! These are the fire definitions from the input
        integer(eb) :: room, object
        real(eb) :: n_C, n_H, n_O, n_N, n_Cl, molar_mass
        real(eb) :: mdot_i(nv), qdot_i(nv), area_i(nv), height_i(nv), y_soot_i(nv), y_co_i(nv), y_trace_i(nv)
        
        ! These are calculated values for the current time step
        real(eb) :: x_position, y_position, z_position, area
        real(eb) :: plume_entrained, plume_flow, species_flow(2,ns)
        real(eb) :: hrr_desired, hrr_convective, hrr_radiative, hrr_lower, hrr_upper, hrr_total, heat_of_combustion
    end type fire_type
     
end module cfast_types
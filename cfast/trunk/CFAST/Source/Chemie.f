      subroutine chemie (pyrolysis_rate, molar_mass,
     . entrainment_rate, source_room, h_c, y_soot, y_co,
     . n_C, n_H, n_O, n_N, n_Cl, source_o2, lower_o2_limit,
     . activated_room, activated_sprinkler, activated_time, 
     . activated_rate, model_time,
     . hrr_at_activation, hrr_constrained, pyrolysis_rate_constrained, 
     . species_rates)
      
!     routine: chemie
!     purpose: do the combustion chemistry - for plumes in both the upper and lower layers.
!         note that the kinetics scheme is implemented here.  however, applying it to the
!         various pieces, namely the lower layer plume, the upper layer plume, and the door jet fires, is 
!         somewhat complex.

!         care should be exercised in making changes either here or in the source interface routine.

!     revision: $Revision: 352 $
!     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
!     arguments:  pyrolysis_rate: calculated pyrolysis rate of the fuel (kg/s)
!                 molar_mass: molar mass of the fuel (kg/mol)
!                 entrainment_rate: calculated entrainment rate (kg/s)
!                 source_room: compartment that contains this fire
!                 h_c: heat of combustion of the fuel (W/kg)
!                 y_soot, y_co: species yields for soot and CO; others are calculated from the molecular formula of the fuel (kg species produced/kg fuel pyrolyzed)
!                 n_C, n_H, n_O, n_N, n_Cl: molecular formula for the fuel; these can be fractional; yields of O2, HCl, and HCN are determined from this
!                 source_o2, lower_o2_limit: oxygen concentration in the source layer of the compartment; lower oxygen limit for combustion (as a fraction)
!                 activated_room: if zero, a sprinkler has gone off in this compartment.  If equal to the source room, HRR is saved for future quenching
!                 activated_sprinkler: sprinkler that has activated
!                 activated_time: time of sprinkler activaiton (s)
!                 activated_rate: sprinkler suppression rate
!                 model_time: current simulation time (s)

!                 hrr_at_activation (output): saved hrr in case of future activation (W)
!                 hrr_constrained (output): actual HRR of the fire constrained by available oxygen (W)
!                 pyrolysis_rate_constrained (output): actual pyrolysis rate of the fuel constrained by available oxygen (kg/s)
!                 species_rates (output): production rates of species based on calculated yields and constrained pyrolysis rate (kg/s); fuel and oxygen are naturally negative

      implicit none
      integer, intent(in) :: source_room, activated_room,
     . activated_sprinkler
      real*8, intent(in) :: pyrolysis_rate, molar_mass,
     . entrainment_rate, h_c, y_soot, y_co, 
     . n_C, n_H, n_O, n_N, n_Cl, source_o2, lower_o2_limit,
     . activated_time, activated_rate, model_time
      real*8, intent(out) :: hrr_constrained, 
     . hrr_at_activation, 
     . pyrolysis_rate_constrained, species_rates(:)
      
      logical :: first=.TRUE.
      real*8 :: o2f, o2fi, o2_entrained, o2_factor, 
     . o2_available, quenching_factor
      real*8 :: nu_o2, nu_co2, nu_h2o, nu_co, nu_soot, nu_hcl,
     . nu_hcn
      real*8 :: net_o2, net_co2, net_h2o, net_co, net_soot, 
     . net_hcl, net_hcn, net_fuel, net_ct, net_trace
      
      if (first) then
          o2f = 1.31d+7
          o2fi = 1.0d0 / o2f
          first = .false.
      endif
      
      ! calculate the actual burning rate constrained by available o2.

      ! note the scaling in the tanh function.  tanh approaches ~2 at
      ! about ~4. the function inside the tanh scales the ordinate to
      ! ~o2range.  the remainder of the function scales the result 
      ! to 0-1
      o2_entrained = entrainment_rate * source_o2
      o2_factor = tanh(800.0d0*(source_o2-lower_o2_limit)-4.0d0) * 
     . 0.5d0 + 0.5d0
      o2_available = o2_entrained * o2_factor
      hrr_constrained = 
     . max(0.0d0,min(pyrolysis_rate*h_c,o2_available*o2f))
      pyrolysis_rate_constrained = hrr_constrained / h_c
      
      
      ! Here we do a reduction for sprinklers if activation has occurred. Otherwise we just save the current value of the HRR
      if (activated_room==source_room) then
          ! if idset=source then save value of fire for later quenching
          hrr_at_activation = hrr_constrained
      else if (activated_room==0) then
          ! a sprinkler reduces the hrr from a fire. the reduction factor is determined by the sprinkler characteristics.
          ! this factor is applied to the fire based on hrr at activation.
          ! however, the hrr might be reduced for other reasons, so the arithmetic min function is used.
          ! the value used is the value at activation. the quenching factor is then a reduction based on time since activation
          if (activated_sprinkler/=0) then
              quenching_factor = 
     .         exp(-(model_time-activated_time)/activated_rate)
              if (hrr_at_activation>0.0d0) hrr_constrained = 
     .         min(hrr_constrained,quenching_factor*hrr_at_activation)
          endif
      endif
      
      ! now do the chemistry balance with supplied inputs.  
      nu_soot = molar_mass/0.01201d0*y_soot
      nu_hcn = n_N
      nu_hcl = n_Cl
      nu_co = molar_mass/0.02801d0*y_co
      nu_h2o = (n_H - nu_hcl - nu_hcn)/2.0d0
      nu_co2 = n_C  - nu_co - nu_hcn - nu_soot
      nu_o2 = nu_co2 + (nu_h2o + nu_co - n_O)/2.0d0
      
      ! chemistry balance is molar-based so convert back to mass rates. fuel and o2 are consumed, so negative. Others are produced, so positive
      net_fuel = -pyrolysis_rate_constrained
      net_o2 = -pyrolysis_rate_constrained*nu_o2*0.032d0/molar_mass
      net_co2 = pyrolysis_rate_constrained*nu_co2*0.04401d0/molar_mass
      net_co = pyrolysis_rate_constrained*nu_co*0.02801d0/molar_mass
      net_h2o = pyrolysis_rate_constrained*nu_h2o*0.018016d0/molar_mass
      net_hcl = pyrolysis_rate_constrained*nu_hcl*0.036458d0/molar_mass
      net_hcn = pyrolysis_rate_constrained*nu_hcn*0.027028d0/molar_mass
      net_soot = pyrolysis_rate_constrained*nu_soot*0.01201d0/molar_mass
      net_ct = pyrolysis_rate_constrained
      
      ! set mass "generation" rates in the cfast structure for species
      species_rates(2) = net_o2
      species_rates(3) = net_co2
      species_rates(4) = net_co
      species_rates(5) = net_hcn
      species_rates(6) = net_hcl
      species_rates(7) = net_fuel
      species_rates(8) = net_h2o
      species_rates(9) = net_soot
      species_rates(10) = net_ct

      end subroutine chemie 
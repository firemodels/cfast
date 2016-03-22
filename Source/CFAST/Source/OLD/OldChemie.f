
      subroutine chemie(qqspray,pyrol,entrain,source,layer,hcombt,cco2t,
     +coco2t,hcratt,ocratt,clfrat,cnfrat,crfrat,qpyrol,netfuel,xmass)


!     routine: chemie
!     revision: $revision: 352 $
!     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
!     purpose: do the combustion chemistry - for plumes in both the upper and lower layers.
!         note that the kinetics scheme is implemented here.  however, applying it to the
!         various pieces, namely the lower layer plume, the upper layer plume, and the door jet fires, is 
!         somewhat complex.

!         care should be exercised in making changes either here or in the source interface routine.

!     arguments: qqspray  heat release rate at sprinkler activation time
!                pyrol   pyrolysis rate of the fuel (kg/s)
!                entrain plume entrainment rate (kg/s)
!                source  room number from which the mass flux comes
!                layer   layer mass is coming from (1=lower, 2=upper)
!                hcombt  current heat of combustion (j/kg)
!                cco2t   current carbon/co2 production ratio (kg/kg)
!                coco2t  current co/co2 production ratio (kg/kg)
!                hcratt  current hydrogen/carbon ratio in fuel (kg/kg)
!                ocratt  current oxygen/carbon ratio in fuel (kg/kg)
!                clfrat  current hcl production rate (kg/kg pyrolized )
!                cnfrat  current hcn production rate (kg/kg pyrolized)
!                crfrat  current trace species production (kg/kg pyrolized)
!                qpyrol  net heat release rate constrained by available oxygen (w)
!                netfuel net burning rate of fuel constrained by available oxygen (kg/s)
!                xmass   net rate of production of species into layers in the room containing the fire (kg/s)

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"

      dimension xmass(ns)
      integer source
      double precision netfuel, netful
      double precision neth2o, netco2, netco, netc, neto2, netcl, netcn
      double precision mdotnet, mdotnetactual
      logical first
      save first, o2f, o2fi, o2range, xx0, xxfour, xxhalf, xxrange
      data first /.true./
      if (first) then
          o2f = 1.31d+7
          o2fi = 1.0d0 / o2f
          o2range = 0.01d0
          first = .false.
          xx0 = 0.0d0
          xx1 = 1.0d0
          xxfour = 4.0d0
          xxhalf = 0.5d0
          xxrange = 8.0d0 / o2range
      end if

      ! calculate the actual burning rate constrained by available o2.

      ! note the scaling in the tanh function.  tanh approaches ~2 at
      ! about ~4. the function inside the tanh scales the ordinate to
      ! ~o2range.  the remainder of the function scales the abscissa 
      ! to 0-1 for o2index.
      o2frac = zzcspec(source,layer,2)
      o2entr = entrain * o2frac
      o2index = tanh(xxrange*(o2frac-limo2)-xxfour) * xxhalf + xxhalf
      o2mass = o2entr * o2index
      qpyrol = max(xx0,min(pyrol*hcombt,o2mass*o2f))

      ! first convert chlorine and cyanide production to carbon based ratios

      fact = (1.0d0+hcratt+ocratt) / (1.0d0-clfrat-cnfrat)
      clcrat = clfrat * fact
      cncrat = cnfrat * fact

      fcratt = (1.0d0+ocratt+hcratt+clcrat+cncrat)

      mdotnet = pyrol
      mdotnetactual = min (mdotnet, o2mass*o2f/hcombt)
      qpyrol = mdotnetactual * hcombt

      ! Here we do a reduction for sprinklers if activation has occurred. Otherwise we just save the current value of the HRR

      if (idset==source) then
          ! if idset=source then save value of fire for later quenching
          qqspray = qpyrol
      else if (idset==0) then
          ! a sprinkler reduces the hrr from a fire. the reduction factor is determined by the sprinkler characteristics.
          ! this factor is applied to the fire based on hrr at activation.
          ! however, the hrr might be reduced for other reasons, so the arithmetic min function is used.
          ! the value of qqspray is the value at activation. tfact is then a reduction based on time since activation
          id = iquench(source)
          if (id/=0) then
              tdrate = xdtect(id,drate)
              timef = xdtect(id,dtact)
              tfact = exp(-(stime-timef)/tdrate)
              if (qqspray>0.0d0) qpyrol = min(qpyrol,tfact*qqspray)
          end if
      end if

      netfuel = mdotnetactual
      neto2 = -qpyrol * o2fi 

      ! now do the "kinetics scheme"
      neth2o = 9.0d0 * netfuel * hcratt / fcratt
      factor1 = 1.0d0 + hcombt * o2fi - ocratt / fcratt
      factor2 = (clcrat+cncrat+9.d0*hcratt) / fcratt
      netco2 = (factor1-factor2) * netfuel / (1.d0+coco2t+cco2t)

      xmass(2) = neto2
      xmass(3) = netco2
      xmass(4) = netco2 * coco2t
      xmass(7) = -netfuel ! this adjusts tuhc when combustion occurs
      xmass(8) = 9.0d0 * netfuel * hcratt / fcratt
      xmass(9) = netco2 * cco2t

      return
      end subroutine chemie

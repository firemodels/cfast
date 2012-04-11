      subroutine fires(tsec,flwf,update)

!     routine: fires
!     purpose: physical interface routine to calculate the current
!              rates of mass and energy flows into the layers from
!              all fires in the building.
!     revision: $revision: 352 $
!     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
!     arguments: tsec   current simulation time (s)
!                flwf   mass and energy flows into layers due to fires.
!                       standard source routine data structure.
!                nfire  total number of fires
!                ifroom room numbers for each of the fires
!                xfire  fire related quantities used by other routines.
!                       (i,1 to 3) = x, y, and z position for fire i
!                       (i,4) = mass into upper layer from fire i (ems)
!                       (i,5) = pyrolysis rate from fire i (emp)
!                       (i,6) = mass entrained in plume by fire i (eme)
!                       (i,7 & 8) = convective, and radiative heat into
!                               upper layer, fire i
!                       (i,9) = total heat released by fire i
!                       (i,10) = total heat into lower layer by fire i
!                       (i,11) = total heat into upper layer by fire i
!                       (i,12 to 18) = heat of combustion, c/co2,
!                                co/co2, h/c, o/c, hcl, hcn yields for
!                                fire i
!					   (i,19) characteristic length of the burning volume


      include "precis.fi"
      include "cfast.fi"
      include "flwptrs.fi"
      include "params.fi"
      include "cenviro.fi"
      include "objects1.fi"
      include "objects2.fi"
      include "opt.fi"
      include "fltarget.fi"

      dimension flwf(nr,ns+2,2), xntms(2,ns), stmass(2,ns)
      dimension xxfire(1), yyfire(1), zzfire(1), zzloc(1)
      dimension ftemp(1), fvel(1)
      double precision n_C,n_H,n_O,n_N,n_Cl
      integer cjetopt
      integer update

      ! initialize summations and local data
      do lsp = 1, ns + 2
          do iroom = 1, n
              flwf(iroom,lsp,upper) = 0.0d0
              flwf(iroom,lsp,lower) = 0.0d0
          end do
      end do
      nfire = 0

      if (option(ffire)/=fcfast) return

      ! Check to see if there is a main fire specified. We should not be able to get here
      if (lfbo>0.and.lfbo<n.and.lfbt>0) then
          write (logerr,*) 'Stop MAINF keyword is outdated. ',
     .        'Update input file'
      end if

      nobj = 0
      do i = 1, numobjl
          if (objpnt(i)>0) then
              iroom = objrm(i)
              iobj = objpnt(i)
              call pyrols(i,tsec,iroom,omasst,oareat,ohight,
     +        oqdott,objhct,n_C,n_H,n_O,n_N,n_Cl,
     +        y_soot,y_co,y_trace)
!              call pyrols(i,tsec,iroom,omasst,oareat,hf0t,
!     +        qpyrol,hcombt,cco2t,coco2t,hcratt,mfiret,ocratt,
!     +        clfrat,cnfrat,crfrat,update)
              oplume(1,iobj) = omasst

              do lsp = 1, ns
                  stmass(upper,lsp) = zzgspec(iroom,upper,lsp)
                  stmass(lower,lsp) = zzgspec(iroom,lower,lsp)
              end do

              call dofire(i,iroom,oplume(1,iobj),hr(iroom),br(iroom),
     +        dr(iroom),objhct,y_soot,y_co,y_trace,n_C,n_H,n_O,n_N,n_Cl,
     +        objgmw(i),stmass,objpos(1,iobj),objpos(2,iobj),
     +        objpos(3,iobj)+ohight,objclen(i),oplume(2,iobj),
     +        oplume(3,iobj),oqdott,xntms,qf(iroom),qfc(1,iroom),
     +        xqfr,heatlp(iroom),heatup(iroom))

              ! sum the flows for return to the source routine
              xtl = zztemp(iroom,lower)
              flwf(iroom,m,upper) = flwf(iroom,m,upper) + oplume(3,iobj)
              flwf(iroom,m,lower) = flwf(iroom,m,lower) - oplume(2,iobj)
              q1 = cp * oplume(1,iobj) * te
              q2 = cp * oplume(2,iobj) * xtl
              flwf(iroom,q,upper) = flwf(iroom,q,upper) + 
     +        qfc(upper,iroom) + q1 + q2
              flwf(iroom,q,lower) = flwf(iroom,q,lower) - q2
              do lsp = 1, ns
                  flwf(iroom,lsp+2,upper) = flwf(iroom,lsp+2,upper) +
     +            xntms(upper,lsp)
                  flwf(iroom,lsp+2,lower) = flwf(iroom,lsp+2,lower) +
     +            xntms(lower,lsp)
              end do

              ! put the object information to arrays - xfire and froom, ...
              ! note that we are carrying parallel data structures for the fire information
              ! output uses the unsorted arrays, froom, ..., ordered by object
              ! fire physics uses the sorted arrays, sorted by compartment
              nfire = nfire + 1
              ifroom(nfire) = iroom
              xfire(nfire,1) = objpos(1,iobj)
              xfire(nfire,2) = objpos(2,iobj)
              xfire(nfire,3) = objpos(3,iobj) + ohight
              xfire(nfire,4) = oplume(3,iobj)
              xfire(nfire,5) = oplume(1,iobj)
              xfire(nfire,6) = oplume(2,iobj)
              xfire(nfire,7) = qfc(1,iroom)
              xfire(nfire,8) = xqfr
              xfire(nfire,9) = heatlp(iroom) + heatup(iroom)
              xfire(nfire,10) = heatlp(iroom)
              xfire(nfire,11) = heatup(iroom)
              xfire(nfire,12) = hcombt
              xfire(nfire,13) = cco2t
              xfire(nfire,14) = coco2t
              xfire(nfire,15) = hcratt
              xfire(nfire,16) = ocratt
              xfire(nfire,17) = clfrat
              xfire(nfire,18) = cnfrat
              xfire(nfire,19) = objclen(iobj)
              nobj = nobj + 1
              froom(nobj) = iroom
              femp(nobj) = oplume(1,iobj)
              fems(nobj) = oplume(3,iobj)
              ! note that cnfrat is not reduced by sprinklers, but oplume(1) is so femr is. (see code in chemie and pyrols)
              femr(nobj) = oplume(1,iobj)* crfrat
              fqf(nobj) = heatlp(iroom) + heatup(iroom)
              fqfc(nobj) = qfc(1,iroom)
              fqlow(nobj) = heatlp(iroom)
              fqupr(nobj) = heatup(iroom)
              farea(nobj) = oareat
              do j = 1,3
                  fopos (j,nobj) = objpos(j,iobj)
              end do

          end if
      end do

      return
      end

      subroutine dofire(ifire,iroom,xemp,xhr,xbr,xdr,hcombt,
     . y_soot,y_co,y_trace,n_C,n_H,n_O,n_N,n_Cl,mol_mass,stmass,
     .xfx,xfy,xfz,objectsize,
     .xeme,xems,xqpyrl,xntms,xqf,xqfc,xqfr,xqlp,xqup)


!     routine: dofire
!     purpose: do heat release from a fire for both main fire and objects. pyrolysis 
!         and kinetics are separate operations.  pyrolysis: tuhc, hcl, hcn, ct and ts - source 
!         is from pyrols ; plume to ul is done below. combustion kinetics applies to o2, co2, co, od - chemie
!     revision: $revision: 352 $
!     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
!     arguments:  ifire: fire number (ifire=0 is the main fire)
!                 iroom: room containing the fire
!                 xemp: pyrolysis rate of the fire (kg/s)
!                 xhr: height of the room (m)
!                 xbr: breadth of the room (m)
!                 xdr: Depth of the room (m)
!                 hcombt  - current heat of combustion (j/kg)
!                 y_soot, y_co, y_trace: species yields for soot, CO, and trace species; others are calculated from the molecular formula of the fuel (kg species produced/kg fuel pyrolyzed)
!                 n_C, n_H, n_O, n_N, n_Cl: molecular formula for the fuel; these can be fractional; yields of O2, HCl, and HCN are determined from this
!                 molar_mass: molar mass of the fuel (kg/mol)
!                 stmass: mass of a species in a layer in the room (kg)
!                 xfx: position of the fire in x direction
!                 xfy: position of the fire in y direction
!                 xfz: position of the fire in z direction
!                 objectsize: characteristic object diameter for plume models
!                 xeme (output): plume entrainment rate (kg/s)
!                 xems (output): plume flow rate into the upper layer (kg/s)
!                 xqpyrl (output): actual heat release rate of the fire (w)
!                 xntms (output): net change in mass of a species in a layer
!                 xqf (output): net heat generation rate into upper layer (w)
!                 xqfc (output): net convection into layers (w)
!                 xqfr (output): net radiation from fire (w)
!                 xqlp (output): heat release in the lower plume (w)
!                 xqup (output): heat release rate in the upper plume (w)

      use interfaces
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "fireptrs.fi"

      dimension xntms(2,ns), xqfc(2), stmass(2,ns), xmass(ns)
      double precision n_C,n_H,n_O,n_N,n_Cl,mol_mass

      x1 = 1.0d0
      x0 = 0.0d0

      ! note: added upper/lower parameters to following three statements.
      ! xtu was incorrectly set to lower layer temp, fixed it
      xz = zzhlay(iroom,upper)
      xtl = zztemp(iroom,lower)
      xtu = zztemp(iroom,upper)
      xqfc(lower) = 0.0d0
      xqfc(upper) = 0.0d0
      xqlp = 0.0d0
      xeme = 0.0d0

      ! these are the lengths ("heights") in the upper and lower layers respectively
      ! if it is negative, then the fire is not in that layer
      xxfirel = xhr - xz - xfz
      xxfireu = xhr - xfz
      xntfl = x0
      qheatl = x0
      qheatu = x0
      xqfr = x0
      xems = x0

      do lsp = 1, ns
          xntms(upper,lsp) = x0
          xntms(lower,lsp) = x0
          xmass(lsp) = x0
      end do
      
      ! the trace species is assumed to be released by the pyrolysis of the burning object regardless of whether the fuel actually combusts here.
      ! this is consistent with the earlier chemistry routine. release it here and deposit it in the upper layer
      xntms(upper,11) = xemp*y_trace

      ! now do the kinetics scheme

      ! divvy up the plume output into radiation and convective energy.
      ! convection drives the plume entrainment

      chirad = max(min(radconsplit(ifire),x1),x0)
      qheatl = max((xqpyrl+cp*(te-xtl)*xemp)*(x1-chirad),x0)

      if (lfbt==free) then
          ! we have eliminated unconstrained fires, if we reach this point, the input parser has failed!
          stop 101
      else

          ! note that the combination of firplm and chemie can be called twice
          ! in a single iteration to make sure that the plume entrainment is
          ! consistent with the actual fire size for oxygen limited fires
          ! this is done by "re-passing" the actual fire size to firplm in the
          ! second pass
          ipass = 1
          do while (ipass<=2)

              ! calculate the entrainment rate but constrain the actual amount
              ! of air entrained to that required to produce stable stratification
              call firplm(fplume(ifire), ifire, objectsize, qheatl,
     .        xxfirel,xemp,xems,xeme,min(xfx,xbr-xfx),min(xfy,xdr-xfy))

              ! check for an upper only layer fire
              if (xxfirel<=x0) go to 90
              xeme = min(xeme,qheatl/(max((xtu-xtl),x1)*cp))
              xems = xemp + xeme
              
              source_o2 = zzcspec(iroom,lower,2)
              if (iquench(iroom)>0) then
                  activated_time = xdtect(iquench(iroom),dtact)
                  activated_rate = xdtect(iquench(source),drate)
              else
                  activated_time = 0
                  activated_rate = 0.0
              end if
              call chemie(xemp,mol_mass,xeme,iroom,hcombt,y_soot,y_co,
     .        n_C,n_H,n_O,n_N,n_Cl,source_o2,limo2,idset,
     .        iquench(iroom),activated_time,
     .        activated_rate,stime,qspray(ifire,lower),
     .        xqpyrl,xntfl,xmass) 
!              if (stime>=100.0d0) then
!                  write (*,'(a,f10.6)') 'O2 = ',xmass(2)*1000.0d0
!                  write (*,'(a,f10.6)') 'CO2 = ',xmass(3)*1000.0d0
!                  write (*,'(a,f10.6)') 'CO = ',xmass(4)*1000.0d0
!                  write (*,'(a,f10.6)') 'HCN = ',xmass(5)*1000.0d0
!                  write (*,'(a,f10.6)') 'HCl = ',xmass(6)*1000.0d0
!                  write (*,'(a,f10.6)') 'fuel = ',xmass(7)*1000.0d0
!                  write (*,'(a,f10.6)') 'H2O = ',xmass(8)*1000.0d0
!                  write (*,'(a,f10.6)') 'soot = ',xmass(9)*1000.0d0
!                  stop
!              end if

              ! limit the amount entrained to that actually entrained by the
              ! fuel burned
              xqpyrl = max(x0, (xqpyrl+cp*(te-xtl)*xemp)*(x1-chirad))

              if (xqpyrl<qheatl) then
                  xeme = xeme * (xqpyrl/qheatl)
                  qheatl = xqpyrl
                  ipass = ipass + 1
                  cycle
              end if
              exit
          end do
          xqpyrl = xqpyrl/(x1-chirad)
          xems = xemp + xeme

          do  i = 1, ns
              xntms(upper,i) = xmass(i) + xntms(upper,i)
          end do

          ! add the species flow entrained by the plume to normalize the yields to unity
          xtemp = x0
          do lsp = 1, 9
              xtemp = xtemp + stmass(lower,lsp)
          end do
          ! including the trace species
          xtemp = xtemp + stmass(lower,11)
          if(xtemp==0.0d0) xtemp = 1.0d0
          do lsp = 1, ns
              if (activs(lsp)) then
                  xnet = xeme * stmass(lower,lsp) / xtemp
                  xntms(upper,lsp) = xntms(upper,lsp) + xnet
                  xntms(lower,lsp) = xntms(lower,lsp) - xnet
              end if
          end do
          xqfr = xqpyrl * chirad
          xqfc(upper) = xqpyrl * (x1-chirad)
          xqlp = xqpyrl
          xqf = xqpyrl

          ! add burning in the upper layer to the fire. the heat which
          ! drives entrainment in the upper layer is the sum of the
          ! heat released in the lower layer and what can be released
          ! in the upper layer.

          ! start with the fuel removed by lower layer burning, xntfl
          ! umplm{ep},{es},and {ee} are equivalent to emp, ems and eme
   90     xqup = 0.0d0
          uplmep = max(x0,xemp-xntfl)

          if (uplmep>x0) then
              qheatu = hcombt * uplmep + qheatl
              height = max (x0, min(xz,xxfireu))

              call firplm(fplume(ifire), ifire, objectsize,
     .        qheatu,height,uplmep,uplmes,uplmee,
     .        min(xfx,xbr-xfx),min(xfy,xdr-xfy))
              
              source_o2 = zzcspec(iroom,upper,2)
              call chemie(uplmep,mol_mass,uplmee,iroom,hcombt,y_soot,
     .        y_co,n_C,n_H,n_O,n_N,n_Cl,source_o2,limo2,idset,
     .        iquench(iroom),activated_time,
     .        activated_rate,stime,qspray(ifire,upper),
     .        xqpyrl,xntfl,xmass)

              !call chemie(qspray(ifire,upper),uplmep,uplmee,iroom,upper,
     .        !hcombt,cco2t,coco2t,hcratt,ocratt,clfrat,cnfrat,crfrat,
     .        !xqpyrl,xntfl,xmass)
              xqfr = xqpyrl * chirad + xqfr
              xqfc(upper) = xqpyrl * (x1-chirad) + xqfc(upper)
              xqup = xqpyrl
              xqf = xqpyrl + xqf
              do i = 1, ns
                  xntms(upper,i) = xmass(i) + xntms(upper,i)
              end do
          end if

      end if
      return
      end

      subroutine pyrols (objn,time,iroom,omasst,oareat,ohight,
     +        oqdott,objhct,n_C,n_H,n_O,n_N,n_Cl,
     +        y_soot,y_co,y_trace)

      
!     routine: chemie
!     purpose: returns yields for object fires interpolated from user input  
!     revision: $revision: 352 $
!     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
!     arguments:  objn: the object pointer number, 
!                 time: current simulation time (s)
!                 iroom: room contining the object
!                 omasst (output): pyrolysis rate of object (returned)
!                 oareat (output): area of pyrolysis of object (returned)
!                 ohight (output): height of fire (returned)
!                 oqdott (output): heat release rate of object
!                 objhct (output): object heat of combustion
!                 n_C, n_H, n_O, n_N, n_Cl (output): molecular formula for the fuel; these can be fractional; yields of O2, HCl, and HCN are determined from this
!                 y_soot, y_co, y_trace (output): species yields for soot, CO, and trace species; others are calculated from the molecular formula of the fuel (kg species produced/kg fuel pyrolyzed)

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "fireptrs.fi"
      include "fltarget.fi"
      include "objects1.fi"
      include "objects2.fi"

      integer objn
      double precision n_C,n_H,n_O,n_N,n_Cl,y_soot,y_co,y_trace

      if (.not.objon(objn).or.objset(objn).gt.0) then
          xx0 = 0.0d0
          omasst = xx0
          oareat = xx0
          ohight = xx0
          oqdott = xx0
          n_C = 1.0d0
          n_H = 4.0d0
          n_O = 0.0d0
          n_N = 0.0d0
          n_Cl = 0.0d0
          objhct = 5.0d7
          y_soot = 0.0d0
          y_co = 0.0d0
          y_trace = 0.0d0
          return
      endif

      lobjlfm = objlfm(objn)
      xxtime = time - objcri(1,objn)

      id = iquench(iroom)

      if(id==0)then

          ! if a sprinkler is not active then interpolate at current time
          ifact = 0
      else

          ! if a sprinkler is active then interpolate at current time
          ! and when sprinkler first activated.  make sure that specified
          ! heat release rate is the smaller of rate at current time
          ! and rate at sprinkler activation time * exp( ...) 
          tdrate = xdtect(id,drate)
          xxtimef = xdtect(id,dtact) - objcri(1,objn)
          call interp(otime(1,objn),oqdot(1,objn),lobjlfm,xxtime,1,qt)
          call interp(otime(1,objn),oqdot(1,objn),lobjlfm,xxtimef,1,qtf)
          ifact = 1
          tfact = exp(-(xxtime-xxtimef)/tdrate)
          if(qt<tfact*qtf)then

              ! current time heat release rate is smaller than sprinklerd value
              ! so use current time and reset ifact to 0 so rates are not 
              ! decreased
              ifact = 0
          else
              xxtime = xxtimef
          endif
      endif

      call interp(otime(1,objn),omass(1,objn),lobjlfm,xxtime,1,omasst)
      call interp(otime(1,objn),oqdot(1,objn),lobjlfm,xxtime,1,oqdott)
      call interp(otime(1,objn),objhc(1,objn),lobjlfm,xxtime,1,objhct)
      call interp(otime(1,objn),ood(1,objn),lobjlfm,xxtime,1,y_soot)
      call interp(otime(1,objn),oco(1,objn),lobjlfm,xxtime,1,y_co)
      call interp(otime(1,objn),omprodr(1,11,objn),lobjlfm,xxtime,1,
     .y_trace)
      call interp(otime(1,objn),oarea(1,objn),lobjlfm,xxtime,1,oareat)
      call interp(otime(1,objn),ohigh(1,objn),lobjlfm,xxtime,1,ohight)
      
      n_C = obj_C(objn)
      n_H = obj_H(objn)
      n_O = obj_O(objn)
      n_N = obj_N(objn)
      n_Cl = obj_Cl(objn)
      
    ! attenuate mass and energy release rates if there is an active sprinkler in this room
      if(id/=0.and.ifact==1)then
          omasst = omasst*tfact
          oqdott = oqdott*tfact
      endif

      return
      end subroutine pyrols

      subroutine firplm (plumetype, objectnumber, objectsize, 
     .qjl,zz,xemp,xems,xeme,xfx,xfy)

!     routine: fireplm
!     revision: $revision: 352 $
!     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
!     purpose: physical interface between dofire and the plume models

      include "precis.fi"
      include "cfast.fi"
      integer plumetype, objectnumber

      select case (plumetype)
      case (1) !    mccaffrey
          call mccaffrey(qjl,zz,xemp,xems,xeme,xfx,xfy,objectsize)
          return        
      case (2) !    heskestad
          call heskestad (qjl,zz,xemp,xems,xeme,xfx,xfy,objectsize)
          return        
      end select
      stop 'bad case in firplm'
      end subroutine firplm

      subroutine mccaffrey (qjl,zz,xemp,xems,xeme,xfx,xfy,od)

!     routine: mccaffrey
!     revision: $revision: 352 $
!     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
!     purpose: calculates plume entrainment for a fire from mccaffrey's correlation
!     inputs:    qjl    fire size (w)
!                zz      plume height (m)
!                xemp  mass loss rate of the fire (kg/s)
!                xfx   position of the fire in x direction (m)
!                xfy   position of the fire in y direction (m)
!                od is the object diameter
!     outputs:   xems  total mass transfer rate at height z (kg/s)
!                xeme  net entrainment rate at height z (kg/s)
!     algorithm: "momentum implications for buoyant diffusion flames", combustion and flame 52, 149 (1983)

      include "precis.fi"
      logical first
      save first, a1, a2, a3, t1, t2
      data first /.true./

      ! define assignment statement subroutines to compute three parts of correlation
      fm1(zq) = zq ** .566d0
      fm2(zq) = zq ** .909d0
      fm3(zq) = zq ** 1.895d0

      ! first time in firplm calculate coeff's to insure that mccaffrey correlation is continuous.  
      ! that is, for a1 = .011, compute a2, a3 such that
      ! a1*zq**.566 = a2*zq**.909  for zq = .08
      ! a2*zq**.909 = a3*zq**1.895 for zq = .2
      if (first) then
          first = .false.
          t1 = .08d0
          t2 = .20d0
          a1 = .011d0
          a2 = a1 * fm1(t1) / fm2(t1)
          a3 = a2 * fm2(t2) / fm3(t2)
      end if
      x0 = 0.0d0

      ! determine which entrainment to use by fire position.  if we're on the wall or in the corner, entrainment is modified.
      xf = 1.0d0
      if (xfx==x0.or.xfy==x0) xf = 2.0d0
      if (xfx==x0.and.xfy==x0) xf = 4.0d0
      qj = 0.001d0 * qjl
      if (zz>0.d0.and.qj>0.0d0) then
          zdq = zz / (xf*qj) ** 0.4d0
          if (zdq>t2) then
              xems = a3 * fm3(zdq) * qj
          else if (zdq>t1) then
              xems = a2 * fm2(zdq) * qj
          else
              xems = a1 * fm1(zdq) * qj
          end if
          xems = max(xemp,xems/xf)
          xeme = max(xems-xemp,x0)
      else
          xems = xemp
          xeme = 0.0d0
      end if
      return
      end subroutine mccaffrey

      subroutine heskestad (q, z, emp, ems, eme, x, y, od)

!     routine: mccaffrey
!     revision: $revision: 352 $
!     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
!     purpose: calculates plume entrainment for a fire from heskestad's variant of zukoski's correlation
!     inputs:    q    fire size (w)
!                z      plume height (m)
!                emp  mass loss rate of the fire (kg/s)
!                xfx   position of the fire in x direction (m)
!                xfy   position of the fire in y direction (m)
!                od is the characteristic size of the object (diameter)
!     outputs:   ems  total mass transfer rate at height z (kg/s)
!                eme  net entrainment rate at height z (kg/s)

      include "precis.fi"

      double precision q, qj, z, z0, emp, eme, ems, x, y, od, deltaz

      qj = 0.001d0 * q
      z0 = -1.02d0 * od + 0.083d0 * qj**0.4
      deltaz = max(0.0001d0, z-z0)
      eme = 0.071 * qj**0.333 * deltaz**1.67 * (1.0d0+0.026d0*qj**0.67
     .* deltaz**(-1.67))
      ems = emp + eme    

      end subroutine heskestad
      
      subroutine integrate_mass (time, deltt)

!     routine:  integrate_mass
!     description: Routine to integrate the pyrolosate of objects
!         we also integrate the trace species release and total for all fires
!     revision: $revision: 352 $
!     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
!     Arguments:  time    current simulation time
!                 deltt   current time step

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"    

      integer i, j
      double precision xx0,xx1,filter,qcifraction,time,deltt
      data xx0/0.0d0/, xx1/1.0d0/

      do i = 0, numobjl
          objmaspy(i) = objmaspy(i) + femp(i)*deltt
          radio(i) = radio(i) + femr(i)*deltt
      end do

      ! sum the trace release from all of the fires
      tradio = xx0
      do i = 0, numobjl
          tradio = tradio + radio(i)
      end do

      ! sum the hvac flow
      ! tracet is the trace species which gets through the vent, traces is the mass stopped. Has to be calculated here since
      ! there is no equivalent to 1-... 
      do irm = 1, n
          DO II = 1, NEXT
              I = HVNODE(1,II)
              J = HVNODE(2,II)
              ISYS = IZHVSYS(J)       
              filter = (xx1-qcifraction(qcvf,isys,time)) 
              if (irm==i) then
                  hveflot(upper,ii) = hveflot(upper,ii)+hveflo(upper,ii)
     .                *deltt
                  hveflot(lower,ii) = hveflot(lower,ii)+hveflo(lower,ii)
     .                *deltt 
                  tracet(upper,ii)  = tracet(upper,ii) + 
     .            hveflo(upper,ii)*hvexcn(ii,11,upper)*filter*deltt
                  tracet(lower,ii)  = tracet(lower,ii) + 
     .            hveflo(lower,ii)*hvexcn(ii,11,lower)*filter*deltt
                  traces(upper,ii)  = traces(upper,ii) + 
     .            hveflo(upper,ii)*hvexcn(ii,11,upper)*(xx1-filter)
     .                *deltt
                  traces(lower,ii)  = traces(lower,ii) + 
     .            hveflo(lower,ii)*hvexcn(ii,11,lower)*(xx1-filter)
     .                *deltt
              endif 
          end do
      end do

      return
      end subroutine integrate_mass

      subroutine djet (flwdjf,djetflg)

!     routine:  integrate_mass
!     description: physical interface routine to calculate the current
!                  rates of mass and energy flows into the layers from
!                  all door jet fires in the building.

!                  note that we presume that this calculation is performed
!                  after the normal fires and flow through vents so we
!                  have a heat of combustion to use for the burning fuel.
!                  at present, this heat of combustion is presumed to be
!                  that of the main fire.
!
!     inputs:   nfire   total number of normal fires
!     outputs:  flwdjf  mass and energy flows into layers due to fires.
!                       standard source routine data structure.
!     commons:
!      passed:  vsas     zzgspec  zztemp
!        used:  izvent   n        nvents

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"
      include "flwptrs.fi"
      include "opt.fi"
      include "vents.fi"

      dimension flwdjf(nr,ns+2,2), xntms1(2,ns), xntms2(2,ns)
      dimension flwdjf0(nr,ns+2,2)
      save flwdjf0

      logical djetflg,dj1flag,dj2flag
      logical ventflg(mxvent), roomflg(nr), anyvents

      ! initialize summations and local data
      djetflg = .false.
      xx0 = 0.0d0
      if (option(fdfire)/=on.or.nfire.le.0) return


      ! if no vents have a door jet fire then exit
      do i = 1, nvents

          ! is there a door jet fire into room iroom1
          iroom1 = izvent(i,1)
          if(zztemp(iroom1,upper).ge.tgignt)then
              flw1to2 = vss(1,i)+vsa(1,i)
              if(vsas(2,i).gt.xx0.and.flw1to2.gt.xx0)then
                  djetflg = .true.
                  exit
              endif
          endif

          !is there a door jet fire into room iroom2
          iroom2 = izvent(i,2)
          if(zztemp(iroom2,upper).ge.tgignt)then
              flw2to1 = vss(2,i)+vsa(2,i)
              if(vsas(1,i).gt.xx0.and.flw2to1.gt.xx0)then
                  djetflg = .true.
                  exit
              endif
          endif
      end do

      if(.not.djetflg)return
      do ifrom = 1, n
          do lsp = 1, ns + 2
              flwdjf(ifrom,lsp,lower) = xx0
              flwdjf(ifrom,lsp,upper) = xx0
          end do
      end do

      do i = 1, n
          fqdj(i) = xx0
      end do
      
      hcombt = xfire(1,12)

      ! calculate the heat for each of the door jet fires
      call ventflag(ventflg,roomflg,anyvents)
      if(anyvents)then
          do i = 1, nvents
              if(ventflg(i))then
                  iroom1 = izvent(i,1)
                  iroom2 = izvent(i,2)
                  flw1to2 = zzcspec(iroom1,upper,7)*(vss(1,i)+vsa(1,i))
                  flw2to1 = zzcspec(iroom2,upper,7)*(vss(2,i)+vsa(2,i))
                  call djfire(iroom2,zztemp(iroom1,upper),
     .            flw1to2,vsas(2,i),hcombt,qpyrol2,xntms2,dj2flag)
                  call djfire(iroom1,zztemp(iroom2,upper),
     .            flw2to1,vsas(1,i),hcombt,qpyrol1,xntms1,dj1flag)

                  ! sum the flows for return to the source routine
                  if(dj1flag)then
                      flwdjf(iroom1,q,upper) = 
     .                flwdjf(iroom1,q,upper) + qpyrol1
                      do lsp = 1, ns
                          flwdjf(iroom1,lsp+2,upper) = 
     .                    flwdjf(iroom1,lsp+2,upper) + xntms1(upper,lsp)
                      end do
                  endif
                  if(dj2flag)then
                      flwdjf(iroom2,q,upper) = 
     .                flwdjf(iroom2,q,upper) + qpyrol2
                      do lsp = 1, ns
                          flwdjf(iroom2,lsp+2,upper) = 
     .                    flwdjf(iroom2,lsp+2,upper) + xntms2(upper,lsp)
                      end do
                  endif
              end if
          end do
      endif

      if(option(fmodjac)==on)then
          if(jaccol==0)then

              ! we need to save the solution for later jacobian calculations
              do iroom = 1, nm1
                  do lsp = 1, ns + 2
                      flwdjf0(iroom,lsp,lower) = flwdjf(iroom,lsp,lower)
                      flwdjf0(iroom,lsp,upper) = flwdjf(iroom,lsp,upper)
                  end do
              end do
          else if(jaccol>0)then

              ! we are computing a jacobian, so get previously saved solution for rooms
              ! that are not affected by the perturbed solution variable
              do iroom = 1, nm1
                  if(.not.roomflg(iroom))then
                      do lsp = 1, ns+2
                          flwdjf(iroom,lsp,lower) = 
     .                    flwdjf0(iroom,lsp,lower)
                          flwdjf(iroom,lsp,upper) = 
     .                    flwdjf0(iroom,lsp,upper)
                      end do
                  endif
              end do
          endif
      endif

      do i = 1, n
          fqdj(i) = flwdjf(i,q,upper) + flwdjf(i,q,lower)
          heatvf(i) = flwdjf(i,q,upper)
      end do
      return
      end subroutine djet

      subroutine djfire(ito,tjet,xxnetfl,sas,hcombt,qpyrol,xntms,
     .djflowflg)

!     routine: djfire
!     purpose: calculate heat and combustion chemistry for a door jet fire  
!     revision: $revision: 352 $
!     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
!     arguments:  ito: room number door jet is flowing into
!                 tjet: temperature of the door jet gas
!                 xxnetfl: net fuel available to be burned
!                 sas: mass flow rate of entrained air in door jet
!                 hcombt: heat of combustion of unburned fuel
!                 qpyrol (output): total heat released by door jet fire
!                 xntms (output): net change in mass of species in door jet

      use interfaces
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"

      dimension xntms(2,ns), xmass(ns)
      logical djflowflg

      x0 = 0.0d0
      qpyrol = x0
      djflowflg = .false.
      
      ! we only wnat to do the door jet calculation if there is fuel, oxygen, and sufficient temperature in the door jet
      if (xxnetfl>x0.and.sas>x0.and.tjet>=tgignt) then

          ! do combustion chemistry assuming complete comversion to co2 & h2o.
          ! although the real chemistry is more complex, for now we don't know
          ! how to handle it.
          dummy = -1.0d0
          djflowflg = .true.
          do i = 1, ns
              xmass(i) = x0
          end do
          source_o2 = zzcspec(ito,lower,2)
          mol_mass = 0.01201d0 ! we assume it's just complete combustion of methane
          qspray = 0.0d0
          call chemie(xxnetfl,xxmol_mass,sas,ito,hcombt,0.0d0,0.0d0,
     .    1.0d0,4.0d0,0.0d0,0.0d0,0.0d0,source_o2,limo2,0,
     .    0,0.0d0,0.0d0,stime,xxqspray,
     .    xqpyrl,xntfl,xmass)
          !call chemie(dummy,xxnetfl,sas,ito,lower,hcombt,x0,x0,x0,x0,x0,
     +    !x0,x0,qpyrol,xxnetfue,xmass)
          do i = 1, ns
              xntms(upper,i) = xmass(i)
              xntms(lower,i) = x0
          end do
      end if
      return
      end

      subroutine flamhgt (qdot, area, fheight)

!     Description:  Calculates flame height for a given fire size and area
!
!     Arguments: qdot    Fire Size (W)
!                area    Area of the base of the fire (m^2)
!                fheight Calculated flame height (m)
!
!     Source: SFPE handbook, Section 2, Chapter 1

      implicit none
      character str*10
      real*8, parameter :: zero = 0.0d0, four = 4.0d0, pi = 3.14159d0
      real*8 qdot, area
      real*8 fheight
      real*8 d
      if (area<=0d0) THEN
          d = 0.09d0
      else
          d = SQRT(four*area/pi)
      end if
      fheight = -1.02*d + 0.235*(qdot/1.0d3)**0.4d0
      fheight = max (zero, fheight)
      return
      end subroutine flamhgt

      subroutine PlumeTemp (qdot, xrad, dfire, tu, tl, zfire, zlayer,
     *zin, tplume)

! Calculates plume centerline temperature at a specified height above
! the fire.
!
! Uses McCaffrey's or Heskestad's correlation to calculate plume centerline temperature

! Uses Evan's method to determine virtual fire size and fire origin when fire
! is in the lower layer and position is in the upper layer

! Inputs:
!
! qdot    total heat release rate of the fire (W)
! xrad    fraction of fire HRR released as radiation
! dfire   fire diamater (m)
! tu      upper layer gas temperature (K)
! tl      lower layer gas temperature (K)
! zfire   height of the base of the fire (m)
! zlayer  height of the hot/cold gas layer interface (m)
! z       position to calculate plume centerline temperature (m)

! Output:
!
! tplume  plume centerline temperature

      implicit none
      real*8 qdot, xrad, dfire, tu, tl, zfire, zlayer, zin
      real*8 tplume
      real*8, parameter :: g = 9.81d0, C_T = 9.115d0, Beta = 0.955d0
      real*8 cp,  rhoamb, z0, qdot_c, z_i1, q_i1star, xi, fheight, z,
     *q_i2star, z_i2, z_eff, q_eff, dt

!     for the algorithm to work, there has to be a fire, two layers, and a target point about the fire      
      z = zin - zfire
      if (qdot>0.0d0.and.tu>=tl.and.z>=0.0d0) then

!       fire and target are both in the lower layer
          if (z<=zlayer) then
              !call PlumeTemp_H (qdot, xrad, dfire, tl, z, tplume)
              call PlumeTemp_M (qdot, tl, z, tplume)

!       fire and target are both in the upper layer
          else if (zfire>=zlayer) then
              !call PlumeTemp_H (qdot, xrad, dfire, tu, z, tplume)
              call PlumeTemp_M (qdot, tu, z, tplume)

!       fire is in lower layer and target is in upper layer
          else
              qdot_c = qdot*(1.0d0 - xrad)/1000.d0
              rhoamb = 352.981915d0/tl
              cp = 3.019d-7*tl**2 - 1.217d-4*tl + 1.014d0
              z_i1 = zlayer - zfire
              q_i1star = qdot_c/(rhoamb*cp*tl*sqrt(g)*z_i1**(5.d0/2.d0))
              xi = tu/tl
!           the effective fire source (qi2star) must be a positive number
              if (1.d0+C_T*q_i1star**(2.d0/3.d0)>xi) then
                  q_i2star = ((1.d0+C_T*q_i1star**(2.d0/3.d0))/(xi*C_T)
     *            -1.d0/C_T)**(3.d0/2.d0)
                  z_i2 = (xi*q_i1star*C_T/
     *            (q_i2star**(1.d0/3.d0)*((xi-1.d0)*(Beta+1.d0)+
     *            xi*C_T*q_i2star**(2.d0/3.d0))))**(2.d0/5.d0)*z_i1
                  rhoamb = 352.981915d0/tu
                  cp = 3.019d-7*tu**2 - 1.217d-4*tu + 1.014d0
                  q_eff = q_i2star*rhoamb*cp*tu*sqrt(g)*
     *            z_i2**(5.d0/2.d0)/(1.0d0-xrad)*1000.d0
                  z_eff = z-z_i1+z_i2
                  call PlumeTemp_M (q_eff, tu, z_eff, tplume)
              else
                  tplume = tu
              end if
          end if
      else
          if (zin<=zlayer) then
              tplume = tl
          else
              tplume = tu
          end if
      end if  
      return
      end subroutine PlumeTemp

      subroutine PlumeTemp_H (qdot, xrad, dfire, tgas, z, tplume)

! Calculates plume centerline temperature at a specified height above
! the fire using Heskestad's correlation

! Inputs:
!
! qdot    total heat release rate of the fire (W)
! xrad    fraction of fire HRR released as radiation
! dfire   fire diamater (m)
! tgas    surrounding gas temperature (K)
! z       distance from fire to position to calculate plume centerline temperature (m)

! Output:
!
! tplume  plume centerline temperature

      implicit none
      real*8 qdot, xrad, dfire, tgas, tl, z
      real*8 tplume
      real*8, parameter :: g = 9.81d0, piov4 = (3.14159d0/4.0d0)
      real*8 cp, fheight, rhoamb, z0, qdot_c, dt
      real*8 dstar, zp1, zp2, tp1, tp2, a, b

!     plume temperature correlation is only valid above the mean flame height      
      call flamhgt (qdot,piov4*dfire**2,fheight)

!     z0 = virtual origin, qdot_c = convective HRR
      if (dfire>0.d0) then
          z0 = -1.02d0*dfire + 0.083d0*(qdot/1000.d0)**0.4d0
      else
          z0 = 0.d0
      end if
      qdot_c = qdot*(1.0d0 - xrad)/1000.d0

      rhoamb = 352.981915d0/tgas
      cp = 3.019d-7*tgas**2 - 1.217d-4*tgas + 1.014d0
      dstar = (qdot/1000.d0/(rhoamb*cp*tgas*sqrt(g)))**0.4d0

      if ((z-z0)/dstar<1.32) then
          dt = 2.91d0*tgas
      else if ((z-z0)<fheight) then
          zp1 = 1.32*dstar
          tp1 = 2.91*tgas
          zp2 = fheight
          tp2 = 9.1d0*(tgas/(g*cp**2*rhoamb**2))**(1.d0/3.d0)*
     *    qdot_c**(2.d0/3.d0)*(zp2)**(-5.d0/3.d0)
          a = ((tp2-tp1)*zp2*zp1)/(zp1-zp2)
          b = tp1-a/zp1
          dt = a/(z-z0) + b
      else
          dt = 9.1d0*(tgas/(g*cp**2*rhoamb**2))**(1.d0/3.d0)*
     *    qdot_c**(2.d0/3.d0)*(z-z0)**(-5.d0/3.d0)
      end if
      tplume = tgas + dt

      end subroutine PlumeTemp_H

      subroutine PlumeTemp_M (qdot, tgas, z, tplume)

! Calculates plume centerline temperature at a specified height above
! the fire using McCaffrey's correlation

! Inputs:
!
! qdot    total heat release rate of the fire (W)
! tgas    surrounding gas temperature (K)
! z       distance from fire to position to calculate plume centerline temperature (m)

! Output:
!
! tplume  plume centerline temperature

      implicit none
      real*8 qdot, dfire, tgas, z
      real*8 tplume
      real*8, parameter :: g = 9.81d0

      real*8 cp, rhoamb, dstar, zstar, dt, n, B, theta

      rhoamb = 352.981915d0/tgas
      cp = 3.019d-7*tgas**2 - 1.217d-4*tgas + 1.014d0
      dstar = (qdot/1000.d0/(rhoamb*cp*tgas*sqrt(g)))**(0.4d0)
      zstar = z/dstar
      if (zstar>=0.d0 .and. zstar<1.32d0) then
          n = 0.5d0
          b = 2.91d0
      else if (zstar>=1.32d0 .and. zstar<3.30d0) then
          n = 0.d0
          b = 3.81d0
      elseif (zstar>=3.30d0) then
          n = -1.d0/3.d0
          b  = 8.41d0
      endif

      theta = b*zstar**(2.*n-1.)
      tplume = tgas*(1.+theta)
      return
      end subroutine PlumeTemp_M

      SUBROUTINE TOXIC(DELTT)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     TOXIC
C
C     Source File: TOXIC.SOR
C
C     Functional Class:  CFAST
C
C     Description:  This routine is used tto calculate species concentrations
C                   (ppm), mass density (kg/m^3), opacity (1/m), 
C                   CT (g-min/m^3), heat flux to target on floor (W)
C
C     Arguments: DELTT  length of the latest time step (s)
C
C     Revision History:
C        5/26/1987 by WWJ, change ct calculation to g-m/m^3
C        11/20/1987 by WWJ, use sum of species to determine total mass used
C                           to calculate mass and volume fractions
C        3/30/1989 by WWJ, fix "ontarget" s*(t-ta)**4->s*(t**4-ta**4)
C        9/12/1989 by WWJ, set ontarget to 0 if < 1
C        11/20/92 by RDP, eliminated MINMAS as the check for minimum molar
C                 count used to calculate molar fraction.  Changed to
C                 1/Avagadro' number so you can't have less than 1 molecule
C                 of gas in a layer.
C	    02/15/02 by WWJ The smoke conversion factor has been changed from 3500 to 3817 
C                  to reflect the new value as reported by Mulholland in Fire and Materials, 24, 227(2000)

C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"
C
      DIMENSION AWEIGH(NS), AIR(2), V(2)
      LOGICAL PPMCAL(NS)
C
C     AWEIGH'S ARE MOLAR WEIGHTS OF THE SPECIES, AVAGAD IS THE RECIPROCAL
C     OF AVAGADRO'S NUMBER (SO YOU CAN'T HAVE LESS THAN AN ATOM OF A SPECIES
C
      DATA AWEIGH, AWEIGH7 /28.D0, 32.D0, 44.D0, 28.D0, 27.D0, 37.D0, 
     +12.D0, 18.D0, 12.D0, 0.D0, 0.0d0, 12.D0/
      DATA AVAGAD /1.66D-24/
      DATA PPMCAL /3 * .FALSE., 3 * .TRUE., 5 * .FALSE./
      AWEIGH(7) = AWEIGH7 * (1.0D0+HCRATT)

      DO 90 I = 1, NM1
C
          V(UPPER) = ZZVOL(I,UPPER)
          V(LOWER) = ZZVOL(I,LOWER)
          DO 20 K = UPPER, LOWER
              AIR(K) = 0.0D0
              DO 10 LSP = 1, 9
                  AIR(K) = AIR(K) + ZZGSPEC(I,K,LSP) / AWEIGH(LSP)
   10         CONTINUE
              AIR(K) = MAX(AVAGAD,AIR(K))
   20     CONTINUE
C
C     CALCLUATE THE MASS DENSITY IN KG/M^3
C
          DO 40 LSP = 1, NS
              IF (ACTIVS(LSP)) THEN
                  DO 30 K = UPPER, LOWER
                      PPMDV(K,I,LSP) = ZZGSPEC(I,K,LSP) / V(K)
   30             CONTINUE
              END IF
   40     CONTINUE
C
C     NOW CALCULATE THE MOLAR DENSITY
C
          DO 60 LSP = 1, 8
              IF (ACTIVS(LSP)) THEN
                  DO 50 K = UPPER, LOWER
                      IF (PPMCAL(LSP)) THEN
                          TOXICT(I,K,LSP) = 1.D+6 * ZZGSPEC(I,K,LSP) /
     +                    (AIR(K)*AWEIGH(LSP))
                      ELSE
                          TOXICT(I,K,LSP) = 100.D0 * ZZGSPEC(I,K,LSP) / 
     +                    (AIR(K)*AWEIGH(LSP))
                      END IF
   50             CONTINUE
              END IF
   60     CONTINUE
C
C     OPACITY IS CALCULATED FROM SEDER'S WORK
C	Note: this value was change 2/15/2 from 3500 to 3778 to reflect the new value as reported by
C     Mulholland in Fire and Materials, 24, 227(2000) with recommended value of extinction coefficient
C     of 8700 m^2/g or 8700/ln(1)=3778 converted to optical density
C
          LSP = 9
          IF (ACTIVS(LSP)) THEN
              DO 70 K = UPPER, LOWER
                  TOXICT(I,K,LSP) = PPMDV(K,I,LSP) * 3778.0D0
   70         CONTINUE
          END IF

!     CT is the integration of the total "junk" being transported

          LSP = 10
          IF (ACTIVS(LSP)) THEN
              DO 80 K = UPPER, LOWER
                  TOXICT(I,K,LSP) = TOXICT(I,K,LSP) + PPMDV(K,I,LSP) * 
     +            1000.0D0 * DELTT / 60.0D0
   80         CONTINUE
          END IF

!     TS (trace species) is the filtered concentration - this is the total mass. 
!     It is converted to fraction of the total generated by all fires.
!     This step being correct depends on the INTEGRATEMASS routine

          LSP = 11
          IF (ACTIVS(LSP)) THEN
              DO 81 K = UPPER, LOWER
                  TOXICT(I,K,LSP) = zzgspec(i,k,lsp) ! / (tradio+1.0d-10)
   81         CONTINUE
          END IF

   90 continue

!     ONTARGET IS THE RADIATION RECEIVED ON A TARGET ON THE FLOOR

      DO 100 I = 1, NM1
          ONTARGET(I) = SIGM * (ZZTEMP(I,UPPER)**4-TAMB(I)**4)
          IF (ONTARGET(I)<1.0D0) ONTARGET(I) = 0.0D0
  100 CONTINUE
      RETURN
      END
      SUBROUTINE REMAPFIRES (NFIRES, FLOCAL, FXLOCAL, FYLOCAL, 
     .FZLOCAL, FQLOCAL, FHLOCAL)

C	This routine is to combine the main fire (in lfbo) and any objects into a single list
C	There does not have to be a main fire nor any objects, so NFIRES may be zero

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "smkview.fi"
      include "objects1.fi"
      include "objects2.fi"

C	First, the mainfire if there is one

      IF (LFBO>0) THEN
          nfires = 1
          FLOCAL(1) = FROOM(0)
          FXLOCAL(1) = fopos(1,0)
          FYLOCAL(1) = fopos(2,0)
          FZLOCAL(1) = fopos(3,0)
          CALL FLAMHGT (FQF(0),FAREA(0),FHEIGHT)
          FQLOCAL(1) = FQF(0)
          FHLOCAL(1) = FHEIGHT
      ELSE
          NFIRES = 0
      ENDIF

C	Now the other objects

      DO I = 1, NUMOBJL
          NFIRES = NFIRES + 1
          FXLOCAL(NFIRES) = fopos(1,i)
          FYLOCAL(NFIRES) = fopos(2,i)
          FZLOCAL(NFIRES) = fopos(3,i)
          CALL FLAMHGT (fqf(i),FAREA(I),FHEIGHT)
          FQLOCAL(NFIRES) = fqf(i)
          FHLOCAL(NFIRES) = FHEIGHT
          flocal(nfires) = froom(i)
      END DO
      RETURN
      END

      subroutine sethoc (maxint, mdot, qdot, hdot, hinitial)

!	Routine to implement the algorithm to set the heat of combustion for all fires

      include "precis.fi"

      double precision mdot(maxint), qdot(maxint), hdot(maxint)

      data hcmax /1.0D8/, hcmin /1.0D+6/

      do i = 1, maxint
          if(i>1) then
              if (mdot(i)*qdot(i)<=0.d0) then
                  hdot(i) = hinitial
              else					
                  Hdot(I) = min(hcmax,max(Qdot(I)/mdot(I),hcmin))
                  mdot(I) = Qdot(I)/Hdot(I)
              endif
          else
              hdot(1) = hinitial
          endif
      end do

      return
      end subroutine sethoc

      SUBROUTINE UPDOBJ(IFLAG, TOLD, DT, IFOBJ, TOBJ, IERROR)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     UPDOBJ
C
C     Source File: UPDOBJ.f
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: IFLAG   Flags if check, set, or update variables
C                TOLD    Time previous to this time step
C                DT      Length of last time step
C                IFOBJ   Object number that ignites (return)
C                TOBJ    Time object ignites
C                IERROR  Returns error codes
C
C     Revision History:
C        Created:  8/15/1995 at 13:08 by PAR
C        Modified: 9/5/1995 at 10:29 by PAR:
C                  Added support for IERROR and returns of stops to main
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "objects1.fi"
      include "objects2.fi"
      include "fltarget.fi"
      include "opt.fi"

      DIMENSION TMPOB(2,MXOIN)

      IFOBJ = 0
      TOBJ = TOLD + 2.D0*DT
      TNOBJ = TOLD + DT

!!!!! Note that ignition type 1 is time, type 2 is temperature and 3 is flux !!!
!!!!! The critiria for temperature and flux are stored backupwards - this historical
!!!!! See corresponding code in keywordcases
      DO 10 IOBJ = 1, NUMOBJL
          IF (OBJON(IOBJ)) GOTO 10
          IGNFLG = OBJIGN(IOBJ)
          IOBTARG = OBTARG(IOBJ)
          IF (IGNFLG==1) THEN
              IF (OBJCRI(1,IOBJ)<=TNOBJ) THEN
                  TOBJ = MIN(OBJCRI(1,IOBJ),TOBJ)
                  IFOBJ = IOBJ
                  TMPOB(1,IOBJ) = 1.D0
                  TMPOB(2,IOBJ) = OBJCRI(1,IOBJ)
              ELSE
                  TMPOB(1,IOBJ) = 0.0D0
                  TMPOB(2,IOBJ) = TNOBJ + DT
              END IF
          ELSE IF (IGNFLG==2) THEN
              CALL DO_OBJCK(IFLAG, TOLD, DT, XXTARG(TRGTEMPF,IOBTARG), 
     .        OBJCRI(3,IOBJ), OBCOND(OBOTEMP,IOBJ), IOBJ, IFOBJ, TOBJ,
     .        TMPOB(1,IOBJ))
          ELSE IF (IGNFLG==3) THEN
              CALL DO_OBJCK(IFLAG, TOLD, DT, XXTARG(TRGTFLUXF,IOBTARG), 
     .        OBJCRI(2,IOBJ), OBCOND(OBOFLUX,IOBJ), IOBJ, IFOBJ, TOBJ,
     .        TMPOB(1,IOBJ))
          ELSE
              CALL XERROR('UPDOBJ - Incorrectly defined object type',
     .            0,1,1)
              IERROR = 20
              RETURN
          ENDIF
   10 CONTINUE

      IF (IFLAG/=MDCHK) THEN
          DO 20 IOBJ = 1, NUMOBJL
              IF (.NOT.OBJON(IOBJ)) THEN
                  IOBTARG = OBTARG(IOBJ)
                  OBCOND(OBOTEMP,IOBJ) = XXTARG(TRGTEMPF,IOBTARG)
                  OBCOND(OBOFLUX,IOBJ) = XXTARG(TRGTFLUXF,IOBTARG)
                  IF (IFLAG==MDSET.AND.TMPOB(1,IOBJ)>0.0D0) THEN
                      IF (TMPOB(2,IOBJ)<=TOBJ) THEN
                          OBJON(IOBJ) = .TRUE.
                          IF (OPTION(FBTOBJ)==ON) THEN
                              OBJSET(IOBJ) = 1
                          ELSE
                              OBJSET(IOBJ) = 0
                          END IF
                          OBJCRI(1,IOBJ) = TMPOB(2,IOBJ)
                      END IF
                  END IF
              END IF
   20     CONTINUE
      END IF

      RETURN
      END

      SUBROUTINE DO_OBJCK(IFLAG,TOLD, DT, COND, TRIP, OLDCOND, IOBJ,
     .IFOBJ, TOBJ, TMPOB)

      include "precis.fi"

      DIMENSION TMPOB(2)

      IF (COND>TRIP) THEN
          DELTA = (TRIP-OLDCOND)/(COND-OLDCOND)
          TMPOB(1) = 1.0D0
          TMPOB(2) = TOLD + DT*DELTA
          TOBJ = MIN(TOBJ,TMPOB(2))
          IFOBJ = IOBJ
      ELSE
          TMPOB(1) = 0.0D0
          TMPOB(2) = TOLD + 2.D0*DT
      END IF

      RETURN
      END

      SUBROUTINE HCL (FLWHCL, FLXHCL,IERROR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     HCL
C
C     Source File: HCL.SOR
C
C     Functional Class:  Physical interface routine
C
C     Description:       Physical Interface routine to do HCl deposition
C                        on wall surfaces.
C
C     Arguments: FLWHCL  Mass and energy flows into layers due to HCl
C                        deposition.  Standard source routine data 
C                        structure.
C                FLXHCL  HCl surface concentration flux.
C                IERROR  Returns error codes
C
C     Commons:
C        USED:  Activs   Ar       Br       Dr       Hr       Hwj     
C               Hwjdot   Mass     N        Nm1      Qscnv    Switch  
C               Twj      Zzhlay   Zzrho    Zztemp   Zzvol   
C
C     Revision History:
C        Created:  9/5/1995 at 9:32 by PAR
C        Modified: 9/5/1995 at 9:35 by PAR:
C                  Added support for IERROR and returning stops to main
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"
      include "opt.fi"
      DIMENSION FLWHCL(NR,NS+2,2), FLXHCL(NR,4)

C     INITIALIZE SUMMATIONS AND LOCAL DATA

      X0 = 0.0D0

C*** only zero out mass (lsp=1) and hcl (lsp=2+6) entries of flwhcl

      DO 10 IROOM = 1, N
          DO 11 J = 1, NS+2
              FLWHCL(IROOM,J,UPPER) = X0
   11         FLWHCL(IROOM,J,LOWER) = X0
              FLXHCL(IROOM,1) = X0
              FLXHCL(IROOM,2) = X0
              FLXHCL(IROOM,3) = X0
              FLXHCL(IROOM,4) = X0
   10 CONTINUE
      IF (OPTION(FHCL)==OFF) RETURN

C     CALCULATE THE HCL "ADDED" TO THE LAYERS FROM EACH SURFACE

      IF (ACTIVS(6)) THEN
          DO 30 IROOM = 1, NM1
              DO 20 IWALL = 1, 4
                  IF (SWITCH(IWALL,IROOM)) THEN
                      IF (IWALL==1) THEN
                          ARW = AR(IROOM)
                          LAYER = UPPER
                      ELSE IF (IWALL==2) THEN
                          ARW = AR(IROOM)
                          LAYER = LOWER
                      ELSE IF (IWALL==3) THEN
                          ARW = (BR(IROOM)+DR(IROOM)) * 
     +                    ZZHLAY(IROOM,UPPER) * 2.0D0
                          LAYER = UPPER
                      ELSE IF (IWALL==4) THEN
                          ARW = (BR(IROOM)+DR(IROOM)) * (HR(IROOM)-
     +                    ZZHLAY(IROOM,UPPER)) * 2.0D0
                          ARW = MAX(X0,ARW)
                          LAYER = LOWER
                      END IF
C              Hclg = Mass(Layer,iroom,6) / Zzvol(iroom,Layer)
C              H2o = Mass(Layer,iroom,8) / Zzvol(iroom,Layer)

C*** use environment variables

                      HCLG = ZZCSPEC(IROOM,LAYER,6)
                      H2O = ZZCSPEC(IROOM,LAYER,8)
                      RHO = ZZRHO(IROOM,LAYER)
                      TG = ZZTEMP(IROOM,LAYER)
                      HCLW = ZZWSPEC(IROOM,IWALL)
                      FLUX = QSCNV(IWALL,IROOM)
                      TW = TWJ(1,IROOM,IWALL)
                      CALL HCLTRAN(IROOM,IWALL,ARW,HCLG,H2O,RHO,TG,HCLW,
     +                    FLUX,TW,HWDOT,HNET,IERROR)
                      IF (IERROR/=0) RETURN

C             SUM UP THE FLOWS AND FLUXES FOR THE SOURCE ROUTINE

                      FLWHCL(IROOM,1,LAYER) = FLWHCL(IROOM,1,LAYER)+HNET
                      FLWHCL(IROOM,2+6,LAYER) = FLWHCL(IROOM,2+6,LAYER) 
     .                    + HNET
                      FLXHCL(IROOM,IWALL) = HWDOT

                  END IF
   20         CONTINUE
   30     CONTINUE
      END IF
      RETURN
      END

      SUBROUTINE HCLTRAN(ICOMP,IWALL,ARW,HCLG,H2O,RHO,TG,HCLW,FLUX,TW,
     +HWDOT,HNET,IERROR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     HCLTRAN
C
C     Source File: HCLTRAN.SOR
C
C     Functional Class:  
C
C     Description: routine to calculate the hydrogen chloride balance 
C                  in the gas and on the wall surface.
C
C     Arguments: ICOMP   Compartment number (input)
C                IWALL   Wall surface number (input)
C                ARW     Area of the wall surface (m^2) (input)
C                HCLG    Current HCL gas concentration (kg/m^3) (input)
C                H2O     Current H2O gas concentration (kg/m^3) (input)
C                RHO     Current gas density (kg/m^3) (input)
C                TG      Gas layer temperature (K) (input)
C                HCLW    Current HCL wall density (kg/m^2) (input)
C                FLUX    Current convective heat flux on wall (W/m^2) (input)
C                TW      Corresponding wall temperature (K) (input)
C                HWDOT   Time derivative of the HCl wall concentration (output)
C                HNET    Time derivative of the HCL gas concentration (output)
C                IERROR  Returns error codes (output)
C
C     Commons:
C        USED:  Cp       Hclbf   
C
C     Revision History:
C        Created:  10/29/1989 at 9:36 by WWJ:
C        Modified: 10/29/1989 at 9:46 by WWJ:
C                  fix the coefficient for deposition, 
C                  optimize the numerics
C        Modified: 2/27/1990 at 9:46 by WWJ:
C                  fix the constants, and add wall absorption 
C                  coefficents to the thermal database
C        Modified: 9/5/1995 at 9:46 by PAR:
C                  Added support for IERROR and returning stops to main
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"

      XX0 = 0.0D0
      HWDOT = XX0
      HNET = XX0
      IF ((HCLG==0.).AND.(HCLW==0.)) RETURN
C
C     NOTE THAT WE CALCULATE DENSITY ON THE FLY, SINCE PPMDV IS NOT UPDATED
C     OFTEN ENOUGH
C
      XHCLF = HCLG * TG * 2.25D-3
      HCLP = XHCLF * 1.0D6
      TWC = TW - 273.0D0
C
C     SPECIFIC VALUES FOR PAINTED GYPSUM - B1 AND B2 ARE FOR GAS PHASE
C     REACTIONS, AND B3 AND B4 ARE FOR THE WALL ITSELF
C
      B1 = HCLBF(1,IWALL,ICOMP)
      B2 = HCLBF(2,IWALL,ICOMP)
      B3 = HCLBF(3,IWALL,ICOMP)
      B4 = HCLBF(4,IWALL,ICOMP)
      B5 = HCLBF(5,IWALL,ICOMP)
      B6 = HCLBF(6,IWALL,ICOMP)
      B7 = HCLBF(7,IWALL,ICOMP)

      IF (B1<=0) RETURN

C     CALCULATE HCL GAS-SURFACE PARTITION COEFFICIENT
C     H2OS IS THE SATURATION CONCENTRATION OF WATER.

      IF (TWC<=40.D0) THEN
          IF (HCLP>10.D0) THEN
              H2OS = (1.8204D0-0.18890D0*LOG(HCLP)+0.06466D0*TWC+
     +        1.650D-3*TWC**2+7.408D-5*TWC**3) / TW
          ELSE
              XTEMP = 17.64262D0 - 5164.1D0 / TW
              EXPTW = EXP(XTEMP)
              BCOEF = (7.696D-5+3.5920D-6*TWC+9.166D-8*TWC**2+4.116D-
     +        9*TWC**3) / TW - 1.D-7 * EXPTW
              H2OS = 0.018D0 * EXPTW + 1.8D4 * BCOEF * HCLP
          END IF
      ELSE IF ((TWC>40.0D0).AND.(TWC<=60.0D0)) THEN
          H2OS = (7.044D0-2.2416D3*XHCLF-3.874D-3*TWC**2+2.328D-4*TWC**3
     +    +2.376D6*XHCLF**2-5.527D8*XHCLF**3+4.918D10*XHCLF**4-
     +    1.359D12*XHCLF**5-1.4033D2*TWC*XHCLF+2.431D4*TWC*XHCLF**2-
     +    1.6023D6*TWC*XHCLF**3) / TW
      ELSE IF ((TWC>60.0D0).AND.(TWC<=80.0D0)) THEN
          H2OS = (107.46D0-4.129D0*TWC+5.096D-2*TWC**2-3.1915D8*XHCLF**3
     +    +1.0408D10*XHCLF**4-2.2793D11*XHCLF**5-5.8194D0*TWC**2*
     +    XHCLF+7.6883D4*TWC*XHCLF**2-7.4363D2*TWC**2*XHCLF**2+
     +    .059067D0*TWC**3*XHCLF+1.8132D6*TWC*XHCLF**3) / TW
      ELSE IF ((TWC>80.0D0).AND.(TWC<=95.0D0)) THEN
          H2OS = (2.583D2-8.0386D0*TWC+1.739D5*XHCLF+7.608D-2*TWC**2-
     +    1.5492D7*XHCLF**2+3.956D9*XHCLF**3-2.065D11*XHCLF**4+
     +    1.3747D13*XHCLF**5-4.086D3*TWC*XHCLF+24.06D0*TWC**2*XHCLF+
     +    1.3558D5*TWC*XHCLF**2-3.076D7*TWC*XHCLF**3) / TW
      ELSE IF ((TWC>95.0D0).AND.(TWC<=110.0D0)) THEN
          H2OS = (6.431D2-16.374D0*TWC+2.822D5*XHCLF+0.12117D0*TWC**2-
     +    8.224D7*XHCLF**2-7.387D6*XHCLF**3-5.247D3*TWC*XHCLF+
     +    24.30D0*TWC**2*XHCLF+1.5465D6*TWC*XHCLF**2-7.250D3*TWC**2*
     +    XHCLF**2) / TW
      ELSE IF (TWC>110.0D0) THEN
          XTEMP = 18.3036D0 - 3816.44D0 / (TW-46.13D0)
          H2OS = 0.2885D0 * EXP(XTEMP) / TW
      ELSE
C        STOP 'Error in hcltran - H2O out of range'
          CALL XERROR('HCLTRAN - H2O out of range',0,1,1)
          IERROR = 12
          RETURN
      END IF
C
C     CALCULATE THE COEFFICIENTS
C
C     RK IS THE CONSTANT "kc" WHICH IS THE DEPOSITION COEFFICIENT (M/S)
C     RKE IS THE EQUILIBRIUM COEFFIENT BETWEEN THE GAS AND SOLID PHASE
C
      IF (TW>=TG) THEN
          RK = 8.33D-3
      ELSE
          X001 = .001D0
          RK = ABS(FLUX/(MAX(X001,TG-TW)*RHO*CP))
      END IF
      IF (H2OS>H2O) THEN
          XTEMP = 1500.0D0 / TW
          EXPTW = EXP(XTEMP)
          RKE = B1 * EXPTW / (1.0D0+B2*EXPTW*HCLG) * (1.0D0+B5*H2O**B6/(
     +    (H2OS-H2O)**B7))
      ELSE
          RKE = 1.0D4
      END IF
C
C     CALCULATE THE DERIVATIVES
C
      HCLCOF = RK * (HCLG-HCLW/(RKE+1.0D-20))
      HNET = -HCLCOF * ARW
      XTEMP = -B4 / (8.31D0*TW)
      HWDOT = HCLCOF - B3 * EXP(XTEMP) * HCLW
      RETURN
      END
      integer function rev_fire

      INTEGER :: MODULE_REV
      CHARACTER(255) :: MODULE_DATE 
      CHARACTER(255), PARAMETER :: 
     *mainrev='$Revision$'
      CHARACTER(255), PARAMETER :: 
     *maindate='$Date$'

      WRITE(module_date,'(A)') 
     *mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
      READ (MODULE_DATE,'(I5)') MODULE_REV
      rev_fire = module_rev
      WRITE(MODULE_DATE,'(A)') maindate
      return
      end function rev_fire
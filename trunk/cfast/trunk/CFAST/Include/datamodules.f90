module  iofiles

    implicit none
    
!File descriptors for cfast

    character(6), parameter :: heading="VERSN"
	character(64) ::  project
	character(256) :: datapath, exepath, inputfile, outputfile, smvhead, smvdata, smvcsv, &
	      ssflow, ssnormal, ssspecies, sswall, errorlogging, stopfile, solverini, &
	      historyfile, queryfile, statusfile, kernelisrunning

! Work arrays for the csv input routines

    integer, parameter :: nrow=200, ncol=200
    real*8 rarray(nrow,ncol) 
    character(128) :: carray(nrow,ncol)

end module iofiles

module  interfaces
    interface
    subroutine chemie (pyrolysis_rate, molar_mass, entrainment_rate, source_room, h_c, y_soot, y_co, & 
    n_C, n_H, n_O, n_N, n_Cl, source_o2, lower_o2_limit, activated_room, activated_sprinkler, activated_time, &
    activated_rate, model_time, hrr_at_activation, hrr_constrained, pyrolysis_rate_constrained, species_rates)

    integer, intent(in) :: source_room, activated_room, activated_sprinkler
    real*8, intent(in) :: pyrolysis_rate, molar_mass, entrainment_rate, h_c, y_soot, y_co, &
    n_C, n_H, n_O, n_N, n_Cl, source_o2, lower_o2_limit, activated_time, activated_rate, model_time
    real*8, intent(out) :: hrr_constrained, hrr_at_activation, pyrolysis_rate_constrained, species_rates(:)
    end subroutine chemie
end interface
end module interfaces

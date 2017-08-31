module namelist_data

    use precision_parameters
    use cparams

    implicit none
    save

    ! For namelist input
    integer :: input_file_line_number
    
    ! For conversion routines
    integer :: c1,c2,c3,c4,c5
    
    ! For namelist input and conversion routines
    integer :: n_chemi,hheatnto,nfurncount,nmlcount
    
    real(eb), dimension(mxrooms) :: hheat_comp
    
    ! Diagnositic output
    character(256) :: nmlconfile
    
    ! For type of input files
    logical :: nmlflag=.false.,dotinflag=.false.
    
    logical :: versnflag=.false.,stpmaflag=.false.,matrlflag=.false., &
               compaflag=.false.,targeflag=.false.,cfireflag=.false., &
               chemiflag=.false.,ctimeflag=.false.,tambiflag=.false., &
               eambiflag=.false.,limo2flag=.false.,hventflag=.false., &
               deadrflag=.false.,eventflag=.false.,crampflag=.false., &
               vventflag=.false.,mventflag=.false.,detecflag=.false., &
               conezflag=.false.,challflag=.false.,roomaflag=.false., &
               roomhflag=.false.,hheatflag=.false.,dtcheflag=.false., &
               furncflag=.false.,adiabflag=.false.,cslcfflag=.false., &
               cisofflag=.false.,vheatflag=.false.
    
    logical :: event_hflag=.false.,event_vflag=.false.,event_mflag=.false., &
               event_fflag=.false.
    
    logical, dimension(50,50,10,mxhvents) :: &
               hflag=.false.,vflag=.false.,mflag=.false.
    
    logical, dimension(50,50,mxhvents) :: fflag=.false.
    
    ! For diagnosis
    logical :: diagnflag=.false.,diradflag=.false.

end module namelist_data


! --------------------------- interfaces -------------------------------------------

module  interfaces
    
    interface
    subroutine flogo(dirs12,yslab,xmslab,tslab,nslab,tu,tl,zlay,qslab,pslab,nprod,mxfslab,mflows,uflw2)

    use precision_parameters
    use cparams, only: ns

    integer, intent(in) :: dirs12(*)
    integer, intent(in) :: nprod, nslab, mxfslab
    real(eb), intent(in) :: yslab(*), xmslab(*), tslab(*), qslab(*), zlay(*), pslab(mxfslab,*), tu(*), tl(*)
    real(eb), intent(out) :: mflows(2,2,2), uflw2(2,ns+2,2)

    end subroutine flogo
    end interface
    
end module interfaces
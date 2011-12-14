subroutine getvals(rm_pres,layer_ht,llayer_temp,ulayer_temp)
  use precision
  use zonedata
  implicit none

  real(kind=dd),dimension(*) :: rm_pres,layer_ht,llayer_temp,ulayer_temp
  
  type(room_data), pointer :: r
  integer :: iroom

  do iroom = 1, nrooms
    r => rooms(iroom)
    rm_pres(iroom) =  r%rel_pressure
    layer_ht(iroom) = r%rel_layer_height
    llayer_temp(iroom) = r%layer(lower)%temperature
    ulayer_temp(iroom) = r%layer(upper)%temperature
  end do
end subroutine getvals


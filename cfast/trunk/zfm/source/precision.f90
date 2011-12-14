module precision
  implicit none
  save

 ! 15 digits of precision used

   integer, parameter :: dd = selected_real_kind(15)

end module precision


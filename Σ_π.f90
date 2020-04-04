program sum_pi
  use iso_fortran_env
  implicit none
  integer, parameter :: qp = selected_real_kind(16)
  integer, parameter :: ip = selected_int_kind(8)
  real(kind=qp) :: approx_pi [*]
  integer(ip) :: i, j, n_max = 2000000

  do j=1, 200
    approx_pi = 0
    do i= this_image(), n_max, num_images()
      approx_pi = approx_pi + (-1)**(1+i) / real( 2*i -1, kind=qp)
    end do
    sync all
    if (this_image() == 1) then
      do i=1, num_images()
        approx_pi = approx_pi + approx_pi[i]
      end do
    end if
  end do
end program sum_pi

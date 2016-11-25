module cow
  use random
  implicit none
  contains
    function rbi(n, mu1, mu2, sigma1, sigma2, rho)
      INTEGER, intent(in) :: n
      REAL, intent(in)    :: mu1, mu2, sigma1, sigma2, rho
      real                :: rbi(n, 2), x, y
      integer             :: i
      x = random_normal()*sigma1 + mu1
      y = random_normal()*sigma2 + mu2
      do i=1, n
         x = random_normal()*sqrt(1 - rho**2)*sigma1 + mu1 + sigma1/sigma2 * rho * (y - mu2)
         y = random_normal()*sqrt(1 - rho**2)*sigma2 + mu2 + sigma2/sigma1 * rho * (x - mu1)
         rbi(i,:)=(/x,y/)
      end do
      return
    end function rbi
end module cow

program test_rng
  use cow
  implicit none
  integer, parameter :: it=500000
  INTEGER, DIMENSION(1) :: seed = (/123/)
  real    :: sol(it, 2)
  CALL RANDOM_SEED(PUT=seed)
  sol=rbi(it, 0.0, 1.0, 2.0, 3.0, 0.7)
  print*, sum(sol, 1)/it
  print*, size(sol, 1)
end program

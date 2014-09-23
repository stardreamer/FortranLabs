function f(x) result(y)
    real, intent(in) :: x 
    real :: y
    
    if (x < 0.1) then
        y = 0.
    else if (x < 0.3) then
        y = 5. * x - 0.5
    else if (x < 0.7) then
        y = 1.
    else if (x < 0.9) then
        y = -5. * x + 4.5
    else
        y = 0.
    end if

end function f
    
program hello
use iso_fortran_env
use internal_data_types
use simple_algorithm
use logger
use errors
use fileworker

real, external :: f

type(configuration) :: conf
type(resultdata), pointer :: pAnswer => NULL()

conf = read_configuration('/home/doctor/Labs/FortranLabs/Lab2/bin/default.conf')

call calculate_solution(conf, pAnswer, f)
    
end program hello

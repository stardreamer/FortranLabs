module userfunc

    contains
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
end module userfunc

program hello
use iso_fortran_env
use internal_data_types
use logger
use errors
use fileworker
use simple_algorithm
use userfunc

integer :: i=1
type(configuration) :: conf
type(resultdata) :: answer

conf = read_configuration('/home/doctor/Labs/FortranLabs/Lab2/bin/default.conf')

call calculate_solution(conf, answer, f)
do while (i<conf % numslice)
    print *, answer % calc_result(1) % current_slice % x(i), answer % calc_result(1) % current_slice % values(i)
    i=i+1
end do 
call free_result(answer)   
end program hello

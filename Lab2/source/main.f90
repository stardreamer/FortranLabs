module userfunc
    REAL, PARAMETER :: pi = 3.1415927
    contains
    function f(x) result(y)
        real, intent(in) :: x 
        real :: y
        if (x <= 0.1) then
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
    
    function fint(n) result(y)
        integer, intent(in) :: n
        real :: y
        
        y=20. * ( ( sin(pi*real(n)/10) ) ** 3) * ( 6.*cos(pi*n/5.) + 4.*cos(2.*pi*n/5.) +&
        & 2. * cos(3.*pi*n/5.)+3.)/((pi*real(n))**2)
        

        y = 2. * y

    end function fint
end module userfunc

program hello
use iso_fortran_env
use internal_data_types
use logger
use errors
use fileworker
use simple_algorithm
use userfunc

type(configuration) :: conf
type(resultdata) :: answer

conf = read_configuration('/home/doctor/Labs/FortranLabs/Lab2/config/default.conf')

call calculate_solution(conf, answer, f, fint)
call printStr(getErrorString(), Output_Unit)
call printToGnuplotAnim(conf, answer)
call printToGnuplot(conf, answer)
call printReport(conf,answer)
call printResult(conf,answer)

call free_result(answer)   
end program hello


program hello
use iso_fortran_env
use internal_data_types
use logger
use errors
use fileworker
use calculations

type(configuration) :: conf
type(mol_system) :: cur_system
real :: start, finish
integer :: j = 1

conf = read_configuration('/home/doctor/Labs/FortranLabs/Lab3/config/default.conf')

call initialize_mol_sys(cur_system, conf)

call cpu_time(start)
call simulate_mol_system(cur_system)
call cpu_time(finish)

call printToGnuplot(cur_system)
print '("Time = ",f6.3," seconds.")', finish - start
!~ do while (j <=  size(cur_system % sys))
!~     print *, cur_system % sys(j) % F
!~     j = j + 1
!~ end do

end program hello

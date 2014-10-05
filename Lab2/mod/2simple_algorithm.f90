module simple_algorithm
    use errors
    use internal_data_types
    
    implicit none
    
    REAL, PARAMETER :: pi = 3.1415927
    
    contains
 
    function compare_sol(sol1,sol2) result(diff)
        type(timeslice) :: sol1, sol2
        real :: diff
        
        diff = maxval(abs(sol1 % current_slice % values - sol2 % current_slice % values))
    end function compare_sol
     
    !calculates analitical solution
    subroutine get_analitical_solution(conf, fint, answer, last_timeslice)
        type(resultdata) :: answer
        type(timeslice)  :: last_timeslice
        type(configuration), intent(in) :: conf
        real, external :: fint
        integer :: n = 3, i = 1, ntime = 2
        real :: newelem, oldelem, totalsum

        
 
        do 
            last_timeslice % time = last_timeslice % time + conf % timestep
            i = 1
            do while (i <= size(last_timeslice % current_slice % x))
                n = 3
                oldelem = fint(1) * & 
                                    & exp(- conf % alpha * last_timeslice % time * (pi ** 2)) * &
                                    & sin(pi * last_timeslice % current_slice % x(i))
                newelem = fint(2) * &
                                    & exp(- conf % alpha * last_timeslice % time * ((2*pi) ** 2)) * &
                                    & sin(2. * pi * last_timeslice % current_slice % x(i))
                totalsum = oldelem + newelem 
                
                
                do while (abs(newelem - oldelem) >= conf % eps)
                    oldelem = newelem
                    
                    newelem = fint(n) * &
                                        & exp(- conf %  alpha * last_timeslice % time * ((n*pi) ** 2)) * &
                                        & sin(n * pi * last_timeslice % current_slice % x(i))
                    
                    totalsum = totalsum + newelem
                    n = n + 1
                end do
                last_timeslice % current_slice % values(i) = totalsum
                i = i + 1
                
            end do
            
            if (ntime > size(answer % calc_result)) then
                call extend_result(answer, 2)
            end if
            
            answer % calc_result(ntime) = last_timeslice
            
            if (compare_sol(last_timeslice , answer % calc_result(ntime -1)) < conf % eps) then
                exit
            end if
            ntime = ntime + 1
            
        end do
        print *, ntime
    end subroutine get_analitical_solution
  
    subroutine calculate_solution(conf, answer, f, fint)
          real, external :: f
          real, external :: fint
          type(configuration), intent(inout) :: conf
          type(resultdata) :: answer
          real, pointer :: xgrid(:) => NULL()
          
          type(timeslice) :: local_timeslice
          
          real :: step 
          
          integer :: a = 1 
          
          !check conf
          if (abs(conf % alpha ) < 1e-4) then
            errorcode = SMT_BAD
            return
          end if
          !calculating step
          conf % step = 1. / (conf % numslice -1.)
          
          if ( conf % timestep < 1e-4 ) then
            conf % timestep = (conf % step**2)/(4. * conf % alpha)
          end if
          !preallocation
          allocate(answer % calc_result(1: conf % start_time_slice))
          allocate(xgrid(1 : conf % numslice))
          allocate(local_timeslice % current_slice % values(1 : conf % numslice))
          
                    
          !Setting up start values
          xgrid = (/(a * conf % step, a=1, conf % numslice)/)  
          local_timeslice % time = 0.
          local_timeslice % current_slice % x => xgrid  
          local_timeslice % current_slice % values = (/(f(local_timeslice % current_slice % x(a)), a=1, conf % numslice)/)

          !Saving initial data
          answer % calc_result(1) = local_timeslice
          
          !Calculating solution
          call get_analitical_solution(conf, fint, answer, local_timeslice)
          
    end subroutine calculate_solution 
    
end module simple_algorithm

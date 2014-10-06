module simple_algorithm
    use errors
    use internal_data_types
    
    implicit none
    

    
    contains
 
    function compare_sol(sol1,sol2) result(diff)
        type(timeslice) :: sol1, sol2
        real :: diff
        
        diff = maxval(abs(sol1 % current_slice % values - sol2 % current_slice % values))
    end function compare_sol
    
    !calculate implicit solution
    subroutine get_implicit_solution(conf, answer, last_timeslice)
        type(resultdata) :: answer
        type(timeslice)  :: last_timeslice
        integer :: i = 1, ntime = 2, maxi = 0
        type(configuration), intent(inout) :: conf
        real, dimension(2 : (size(last_timeslice % current_slice % x)-1)) :: A, B
        real :: s = 1.
        s = ((conf % step ** 2) / (conf % alpha * conf % timestep) )
        maxi = size(last_timeslice % current_slice % x) 
        A(2) = 1. / (2. + s) 
        
        
        last_timeslice % current_slice % values(1) = 0.
        last_timeslice % current_slice % values(maxi) = 0.
        
        do 
            last_timeslice % time = last_timeslice % time + conf % timestep 
            
            if (conf % stoptime > 0.) then
                if (last_timeslice % time > conf % stoptime) then
                    exit
                end if
            end if
             
            B(2) = s * (answer % calc_result(ntime - 1) % current_slice % values(2)) / (2. + s) 
            
            i = 3
            
            do while(i < maxi)
                A(i) = 1./(2. + s - A(i-1))
                B(i) = (s * answer % calc_result(ntime - 1) % current_slice % values(i) + B(i-1))/(2. + s - A(i-1))
                i = i + 1
            end do
            
            i = maxi - 1
            do while(i > 1)
                last_timeslice % current_slice % values(i) = A(i) * &
                & last_timeslice % current_slice % values(i + 1) + B(i)
                i = i - 1
            end do

            if (ntime > size(answer % calc_result)) then
                call extend_result(answer, 2)
            end if
            
            answer % calc_result(ntime) = last_timeslice
            
            if (compare_sol(last_timeslice , answer % calc_result(ntime - 1)) < conf % eps) then
                exit
            end if
            ntime = ntime + 1
        end do 
        
        if (conf % stoptime < 0.) then
            conf % stoptime = last_timeslice % time
        end if
        
    end subroutine get_implicit_solution
    
    !calculate explicit solution
    subroutine get_explicit_solution(conf, answer, last_timeslice)
        type(resultdata) :: answer
        type(timeslice)  :: last_timeslice
        type(configuration), intent(inout) :: conf
        integer :: i = 1, ntime = 2, maxi = 0
        real :: corr_parameter = 0
        
        corr_parameter = (conf % alpha * conf % timestep / (conf % step ** 2))
        
        maxi = size(last_timeslice % current_slice % x)
        last_timeslice % current_slice % values(1) = 0
        last_timeslice % current_slice % values(maxi) = 0
        
        do 
            last_timeslice % time = last_timeslice % time + conf % timestep            
            
            if (conf % stoptime > 0.) then
                if (last_timeslice % time > conf % stoptime) then
                    exit
                end if
            end if
            
            i = 2
            
            do while (i < maxi)
                last_timeslice % current_slice % values(i) = answer % calc_result(ntime -1) % current_slice % values(i) + &
                & corr_parameter * ( &
                & answer % calc_result(ntime -1) % current_slice % values(i + 1) - &
                & 2. * answer % calc_result(ntime -1) % current_slice % values(i) +&
                & answer % calc_result(ntime -1) % current_slice % values(i - 1)&
                & )
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
        
        if (conf % stoptime < 0.) then
            conf % stoptime = last_timeslice % time
        end if
        
    end subroutine get_explicit_solution
    !calculates analitical solution
    subroutine get_analitical_solution(conf, fint, answer, last_timeslice)
        type(resultdata) :: answer
        type(timeslice)  :: last_timeslice
        type(configuration), intent(inout) :: conf
        real, external :: fint
        integer :: n = 3, i = 1, ntime = 2
        real :: newelem, oldelem, totalsum

        
 
        do 
            last_timeslice % time = last_timeslice % time + conf % timestep
            
            if (conf % stoptime > 0.) then
                if (last_timeslice % time > conf % stoptime) then
                    exit
                end if
            end if
            
            i = 1
            do while (i <= size(last_timeslice % current_slice % x))
                
                oldelem = fint(1) * & 
                                    & exp(- conf % alpha * last_timeslice % time * (pi ** 2)) * &
                                    & sin(pi * last_timeslice % current_slice % x(i))
                newelem = fint(2) * &
                                    & exp(- conf % alpha * last_timeslice % time * ((2.*pi) ** 2)) * &
                                    & sin(2. * pi * last_timeslice % current_slice % x(i))
                totalsum = oldelem + newelem 
                
                n = 3
                do while (abs(newelem - oldelem) > conf % eps)
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
        
        if (conf % stoptime < 0.) then
            conf % stoptime = last_timeslice % time
        end if
        
    end subroutine get_analitical_solution
  
    subroutine calculate_solution(conf, answer, f, fint)
          real, external :: f
          real, external :: fint
          type(configuration), intent(inout) :: conf
          type(resultdata) :: answer
          real, pointer :: xgrid(:) => NULL()
          
          type(timeslice) :: local_timeslice
          
          integer :: a = 1 
          
          !check conf
          if (abs(conf % alpha ) < 1e-5) then
            errorcode = BAD_ALPHA
            return
          end if
          !calculating step
          conf % step = 1. / (conf % numslice -1.)
          
          if ( conf % timestep < 1e-4 ) then
            conf % timestep = (conf % step**2)/(10. * conf % alpha)
          end if
          !preallocation
          allocate(answer % calc_result(1: conf % start_time_slice))
          allocate(xgrid(1 : conf % numslice))
          allocate(local_timeslice % current_slice % values(1 : conf % numslice))
          
                    
          !Setting up start values
          xgrid = (/((a-1) * conf % step, a=1, conf % numslice)/)  
          local_timeslice % time = 0.
          local_timeslice % current_slice % x => xgrid  
          local_timeslice % current_slice % values = (/(f(local_timeslice % current_slice % x(a)), a=1, conf % numslice)/)

          !Saving initial data
          answer % calc_result(1) = local_timeslice
          
          !Calculating solution
          select case (conf % mode)
          case (0)
            call get_analitical_solution(conf, fint, answer, local_timeslice)
          case (1)
            call get_explicit_solution(conf, answer, local_timeslice)
          case (2)
            call get_implicit_solution(conf, answer, local_timeslice)
          case default
            errorcode = UNKNOWN_MODE
            return
          end select
          
    end subroutine calculate_solution 
    
end module simple_algorithm

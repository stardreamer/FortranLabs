module simple_algorithm
    use errors
    use internal_data_types
    implicit none
    
    contains
 
    !subroutine get_analitical_solution(alpha, pAnswer)
        
        
    !end subroutine get_analitical_solution
  
    subroutine calculate_solution(conf, answer, f)
          real, external :: f
          type(configuration), intent(in) :: conf
          type(resultdata) :: answer
          
          type(slice) :: local_slice
          
          real :: step 
          
          integer :: a = 1 
          
          !calculating step
          step = 1. / (conf % numslice -1.)
          
          
          !preallocation
          allocate(answer % calc_result(1:100))
          allocate(local_slice % x(1 : conf % numslice))
          allocate(local_slice % values(1 : conf % numslice))
          
          
          
          !Setting up start values
          local_slice % x = (/(a * step, a=1, conf % numslice)/)  
          local_slice % values = (/(f(local_slice % x(a)), a=1, conf % numslice)/)

          !Saving current result
          answer % calc_result(1) % time = 0.
          answer % calc_result(1) % current_slice = local_slice
          
          print *, size(answer % calc_result)
          call extend_result(answer, 5)
          print *, size(answer % calc_result)
          
    end subroutine calculate_solution 
    
end module simple_algorithm

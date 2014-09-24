module simple_algorithm
    use errors
    use internal_data_types
    implicit none
    
    contains
    
    subroutine calculate_solution(conf, pAnswer, f)
          real, external :: f
          type(configuration), intent(in) :: conf
          type(resultdata),pointer :: pAnswer(:)
          
          real, pointer :: zerovalues(:,:)
          
          real :: step 
          integer :: a = 1 
          
          step = 1./(conf % numslice -1.)
          
          
          allocate(pAnswer(1:100))
          allocate(zerovalues(1 : conf % numslice, 1 : 2))
          
          
          do while (a < conf % numslice)
                zerovalues(a, 1) = a*step
                zerovalues(a, 2) = f(zerovalues(a, 1))
                a = a + 1

          end do
          
          pAnswer(1) % pdataset => zerovalues
          
    end subroutine calculate_solution 
    
end module simple_algorithm

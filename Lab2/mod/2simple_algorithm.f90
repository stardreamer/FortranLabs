module simple_algorithm
    use errors
    use internal_data_types
    implicit none
    
    contains
    
    subroutine calculate_solution(conf, pAnswer, f)
          real, external :: f
          type(configuration), intent(in) :: conf
          type(resultdata), pointer, intent(inout) :: pAnswer

          allocate(pAnswer(1:10))
    end subroutine calculate_solution 
    
end module simple_algorithm

module internal_data_types
    type :: configuration
        integer :: mode = 0
        integer :: numslice = 100
        real :: timestep = 0
        real :: stoptime = 0
        real :: eps = 0.001
    end type configuration
    
    type :: slice
        real, pointer :: x(:) => NULL()
        real, pointer :: values(:) => NULL()
    end type slice
    
    type :: timeslice
        real :: time = -1.
        type(slice) :: current_slice
    end type timeslice
    
    type :: resultdata
        real :: worktime
        type(timeslice), pointer :: calc_result(:) => NULL()
    end type resultdata
    
    interface assignment (=)
        module procedure copy_slice, copy_timeslice, copy_resultdata
    end interface
    
    contains 
    
    subroutine free_slice(Uslice)
        type(slice), intent(inout) :: Uslice 
        deallocate(Uslice % x)
        deallocate(Uslice % values)
        
        Uslice % x => NULL()
        Uslice % values => NULL()
    
    end subroutine free_slice
    
    subroutine free_timeslice(Utslice)
        type(timeslice), intent(inout) :: Utslice
        if (Utslice % time > 0) then
            call free_slice(Utslice % current_slice)
        end if
    end subroutine free_timeslice
    
    subroutine free_result(Udata)
        type(resultdata), intent(inout) :: Udata
        integer :: a = 1
        
        do while (a <= size(Udata % calc_result))
            call free_timeslice(Udata % calc_result(a))
            a = a + 1
        end do
        
        deallocate(Udata % calc_result)
         
        Udata % calc_result => NULL() 
    end subroutine free_result
    
    !overload assign for slice
    subroutine copy_slice(slice2, slice1)
        type (slice), intent(in) :: slice1
        type (slice), intent(out) :: slice2
        if (.not. associated(slice2 % x)) then
            allocate(slice2 % x(1:size(slice1 % x)))
        end if
        if (.not. associated(slice2 % values)) then
            allocate(slice2 % values(1:size(slice1 % values)))
        end if
        
        slice2 % x = slice1 % x
        slice2 % values = slice1 % values
    end subroutine copy_slice
    
    !overload assign for timeslice 
    subroutine copy_timeslice(timeslice2, timeslice1)
        type (timeslice), intent(in) :: timeslice1
        type (timeslice), intent(out) :: timeslice2
        
        timeslice2 % time = timeslice1 % time
        timeslice2 % current_slice = timeslice1 % current_slice
    end subroutine copy_timeslice
    
    !overload assign for resultdata
    subroutine copy_resultdata(resultdata2, resultdata1)
        type (resultdata), intent(in) :: resultdata1
        type (resultdata), intent(out) :: resultdata2
        
        if (.not. associated(resultdata2 % calc_result)) then
            allocate(resultdata2 % calc_result(1:size(resultdata1 % calc_result)))
        end if
        
        resultdata2 % worktime = resultdata1 % worktime
        resultdata2 % calc_result = resultdata1 % calc_result
    end subroutine copy_resultdata
    
    subroutine extend_result(res, modif)
        type(resultdata), intent(inout) :: res
        integer, intent(in) :: modif
        type(timeslice), pointer :: local_calc_result(:) => NULL()
        
        if (modif > 0) then
            allocate(local_calc_result(1:modif * size(res % calc_result)))
        end if
        
        local_calc_result = res % calc_result
        
        deallocate(res % calc_result)
        res % calc_result => local_calc_result
    end subroutine extend_result
    
end module internal_data_types

module internal_data_types
    type :: configuration
        integer :: mode = 0
        integer :: numslice = 0
        real :: timestep = 0
        real :: stoptime = 0
        real :: eps = 0.001
    end type configuration
    
    type :: resultdata
        real :: time
        real, pointer :: pdataset(:,:) => NULL()
    end type resultdata
end module internal_data_types

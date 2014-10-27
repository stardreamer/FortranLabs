module internal_data_types

    real, parameter :: pi = 3.1415927
    real, parameter :: kb = 1.380648813 * 1e-23

    type :: configuration
        integer :: N = 100
        integer :: timesteps
        real :: sigma = 3.405
        real :: eps = 1.65 * 1e-21
        real :: m0 = 66.4 * 1e-21
        real :: l = 5.
    end type configuration
    
    type :: molecule
        real, dimension(3) :: coord
        real :: v
        real :: m
        real :: F
    end type molecule
    
    type :: mol_system
        type(molecule), pointer :: sys(:) => NULL()
        real, pointer :: Ekin(:) => NULL()
        real :: sigma = 3.405 * 1e-10
        real :: eps = 1.65 * 1e-21
        real :: m0 = 66.4 * 1e-21
        real :: l = 5.
        real :: max_dim_len = 1.
    end type mol_system
    
    contains
    
    subroutine set_powers(mol_sys)
            type(mol_system) :: mol_sys
            type(molecule), pointer :: MOI=>NULL(), NIM=>NULL()
            integer :: i = 1, j = 1, N = 1
            real :: dist
            real :: fij = 0.

            N = size(mol_sys % sys)
            i = 1
            j = 1
             
            do while (i <= N)
                mol_sys % sys(i) % F = 0.
                i = i + 1
            end do
            
            i = 1
            do while (i <= N)
                MOI => mol_sys % sys(i)
                
                j = i + 1
                do while (j <= N)
                        NIM => mol_sys % sys(j)
                        dist = sqrt(sum((MOI % coord - NIM % coord) ** 2))
                            
                        if ( dist < mol_sys % l * mol_sys % sigma) then
                            fij = (48./ (mol_sys % sigma ** 2)) * mol_sys % eps * &
                                                & ((mol_sys % sigma/dist)**14 - 0.5*(mol_sys % sigma/dist)**8) * &
                                                & dist
                            MOI % F = MOI % F + fij
                            NIM % F = NIM % F - fij
                        end if
                            

                    j = j + 1
                end do
                i = i + 1
            end do
            

!~ 
!~             do while (i <= N)
!~                 MOI => mol_sys % sys(i)
!~                 MOI % F = 0.
!~ 
!~                 do while (j <= N)
!~                     if (i /= j) then
!~                             NIM => mol_sys % sys(j)
!~                             dist = sqrt(sum((MOI % coord - NIM % coord) ** 2))
!~                             
!~                             if ( dist < mol_sys % l * mol_sys % sigma) then
!~                                 MOI % F = MOI % F + (48./ (mol_sys % sigma ** 2)) * mol_sys % eps * &
!~                                                     & ((mol_sys % sigma/dist)**14 - 0.5*(mol_sys % sigma/dist)**8) * &
!~                                                     & dist
!~                             end if
!~                             
!~ 
!~                     end if
!~                     j = j + 1
!~                 end do
!~                 j = 1
!~                 i = i + 1
!~             end do

    end subroutine set_powers
    
    subroutine initialize_mol_sys(mol_sys, conf)
        
        type(mol_system) :: mol_sys
        type(configuration) :: conf
        real ::  M = 0., S = 0., V = 0.,dx = 0., Vsum = 0.
        real, parameter :: min_outlet = 1e-2
        integer :: j = 1,i = 1, k = 1, l = 1,counter = 1, seed = 78
        
        
        if (.not. associated(mol_sys % sys)) then
            allocate(mol_sys % sys(1: conf % N))
        else if (size(mol_sys % sys) /= conf % N) then
            deallocate(mol_sys % sys)
            allocate(mol_sys % sys(1:conf % N))
        end if
        
        if (.not. associated(mol_sys % Ekin)) then
            allocate(mol_sys % Ekin(1: conf % timesteps))
        else if (size(mol_sys % Ekin) /= conf % timesteps) then
            deallocate(mol_sys % Ekin)
            allocate(mol_sys % Ekin(1:conf % timesteps))
        end if
        
        mol_sys % Ekin = 0.
        
        call random_seed(seed)

        
        mol_sys % sigma = conf % sigma
        mol_sys % eps = conf % eps
        mol_sys % m0 = conf % m0
        
        do while (j <=  conf % N)
!~             do while(X < min_outlet)
!~                 call random_number(X)
!~             end do
!~             
!~             do while(Y < min_outlet)
!~                 call random_number(Y)
!~             end do
!~             
!~             do while(Z < min_outlet)
!~                 call random_number(Z)
!~             end do
!~             
!~             do while(M < 7. *  min_outlet)
!~                 call random_number(M)
!~             end do
            
            do while(S < min_outlet)
                call random_number(S)
            end do    
            
            do while(V < min_outlet)
                call random_number(V)
                if (S > 0.5) then
                    V = -V
                    exit
                end if
            end do
            
            mol_sys % sys(j) % m = 1.
!~             mol_sys % sys(j) % coord = (/ 0.01*X, 0.01*Y, 0.01*Z /)
            mol_sys % sys(j) % v = V
            
     
            Vsum = Vsum + conf % N * V
            
!~             X = 0.            
!~             Y = 0.            
!~             Z = 0.
            M = 0.
            S = 0.
            V = 0.
            
            j = j + 1
            
        end do
        
        j = 1
        i = 1
        k = 1
        counter = 1
        l = int((conf % N) ** (1.0/3.0) )    
        dx = 1./l
        open(42, file="/home/doctor/Labs/FortranLabs/Lab3/results/init.txt",action="write",status="replace")
        do while (i <= l)
        
            do while (j <= l)
                do while (k <= l)
                    mol_sys % sys(counter) % coord = (/ (real(i)-0.5)*dx, (real(j)-0.5)*dx, (real(k)-0.5)*dx /)
                    write(42, *) mol_sys % sys(counter) % coord
                    counter = counter + 1
                    k = k + 1;                    
                end do
                k = 1
                j = j + 1;
            end do
            j = 1
            i = i + 1
        end do
        close(42)
        j = 1
!~ 
        do while (j <=  conf % N)
           mol_sys % sys(j) % v = conf % N * mol_sys % sys(j) % v - Vsum    
           mol_sys % sys(j) % v = mol_sys % sys(j) % v /conf % N
!~            mol_sys % sys(j) % v = mol_sys % sys(j) % v * sqrt(mol_sys % m0 / mol_sys % eps)
           j = j + 1 
        end do
!~         

                
        call set_powers(mol_sys)
        
        
    end subroutine initialize_mol_sys

!~     subroutine extend_result(res, modif)
!~         type(resultdata), intent(inout) :: res
!~         integer, intent(in) :: modif
!~         type(timeslice), pointer :: local_calc_result(:) => NULL()
!~         
!~         if (modif > 0) then
!~             allocate(local_calc_result(1:modif * size(res % calc_result)))
!~         end if
!~         
!~         local_calc_result = res % calc_result
!~         
!~         deallocate(res % calc_result)
!~         res % calc_result => local_calc_result
!~     end subroutine extend_result
    
end module internal_data_types

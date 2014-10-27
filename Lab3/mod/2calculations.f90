module calculations
    use internal_data_types
    
    implicit none
    

    
    contains
    
    subroutine simulate_mol_system(mol_sys)
        type(mol_system) :: mol_sys
        type(molecule), pointer :: MOI
        integer :: j = 1, t = 1
        integer :: max_size = 1.
        real :: dt = 0.


        dt = 1e-4
        max_size = size(mol_sys % Ekin)
        t = 1
        j = 1
        
        open(42, file="/home/doctor/Labs/FortranLabs/Lab3/results/C1.txt",action="write",status="replace")
        do while(t <= max_size)
            j = 1

            do while(j <= size(mol_sys % sys))
                MOI => mol_sys % sys(j)
                MOI % v = MOI % v + (MOI % F / MOI % m)* 0.5 * dt

                MOI % coord = MOI % coord + MOI % v * dt

                where (MOI % coord  >= 1.) MOI % coord = (MOI % coord - floor(MOI % coord )) 
                where (MOI % coord  <= 0.) MOI % coord = (1. - (abs(MOI % coord) - floor(abs(MOI % coord ))) )
                
                j = j + 1
            end do
            
            call set_powers(mol_sys)

            
            j = 1

            do while(j <= size(mol_sys % sys))
                MOI => mol_sys % sys(j)
                MOI % v = MOI % v + (MOI % F / MOI % m)* 0.5 * dt
                mol_sys % Ekin(t) = mol_sys % Ekin(t) + (MOI % m * (MOI % v ** 2)) / 2.
                j = j + 1
                
            end do

            write(42, *) mol_sys % sys(1) % coord

            t = t + 1
        end do
        close(42)
    end subroutine simulate_mol_system

end module calculations

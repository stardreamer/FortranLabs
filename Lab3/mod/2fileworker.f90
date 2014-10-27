module fileworker
use internal_data_types
use errors

contains

function read_configuration(filename) result(conf)
    character (len=*), intent(in)  :: filename
    type(configuration) :: conf
    
    integer, parameter :: flH = 10
    integer :: stats = 1, j, j0, z
    character(len = 1000) :: line
    character(len = 100) :: fst
    character(len = 100) :: snd
    integer, parameter    :: state_begin=1, state_in_fst=2, state_in_sep=3
    
    open(flH, file=filename)

    do
        read(flH, "(a)", iostat=stats) line
        if (stats<0) exit
        
        if ((line(1:1) == "#") .or. &
            (line(1:1) == ";") .or. &
            (len_trim(line) == 0)) then
            cycle
        end if  

        z = state_begin
        
        do j = 1, len_trim(line)
            if (z == state_begin) then
                if (line(j:j)/=" ") then
                    j0 = j
                    z = state_in_fst
                end if
            elseif (z == state_in_fst) then
                if (index("= ",line(j:j))>0) then
                    fst = line(j0:j-1)
                    z = state_in_sep
                end if
            elseif (z == state_in_sep) then
                    if (index(" =",line(j:j)) == 0) then
                        snd = line(j:)
                        exit
                    end if
            else
                    errorcode = FILE_ERROR
            end if

        end do


        if (fst == "N") then
            read(snd,'(i10)') conf % N  
        else if (fst == "timesteps") then
            read(snd,'(i10)') conf % timesteps  
        else if (fst == "eps") then
            read(snd,'(f10.3)') conf % eps
            conf % eps = conf % eps * 1e-21
        else if (fst == "m0") then
            read(snd,'(f10.3)') conf % m0
            conf % m0 = conf % m0 * 1e-21
        else if (fst == "sigma") then
            read(snd,'(f10.3)') conf % sigma 
!~             conf % sigma = conf % sigma * 1e-10
        else if (fst == "l") then
            read(snd,'(f10.3)') conf % l
        else
            errorcode = FILE_ERROR
        end if
        
    end do
    
    close(flH)
end function read_configuration

subroutine printToGnuplot(mol_sys)
    type(mol_system) :: mol_sys
    integer, parameter :: flH = 11
    integer :: i = 1
    
    open(flH, file="/home/doctor/Labs/FortranLabs/Lab3/results/T1.txt",action="write",status="replace")

    do while (i <= size(mol_sys % Ekin))
        write(flH, *) i, (2./(3.*(kb/mol_sys % eps)))*mol_sys % Ekin(i)
        i = i + 1
    end do


    close(flH)
end subroutine printToGnuplot

end module fileworker

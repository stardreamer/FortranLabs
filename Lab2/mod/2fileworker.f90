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

        if (fst == "mode") then
            read(snd,'(i10)') conf % mode        
        elseif (fst == "numslice") then
            read(snd,'(i10)') conf % numslice
        elseif (fst == "timestep") then
            read(snd,'(f10.3)') conf % timestep
        elseif (fst == "stoptime") then
            read(snd,'(f10.3)') conf % stoptime
        elseif (fst == "eps") then
            read(snd,'(f10.3)') conf % eps    
        else
            errorcode = FILE_ERROR
        end if
        
    end do
    
end function read_configuration

end module fileworker

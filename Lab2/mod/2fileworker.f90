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
        elseif (fst == "alpha") then
            read(snd,'(f10.3)') conf % alpha  
        elseif (fst == "start_time_slice") then
            read(snd,'(i10)') conf % start_time_slice  
        else
            errorcode = FILE_ERROR
        end if
        
    end do
    
    close(flH)
end function read_configuration

subroutine printReport(conf, answer)
    type(resultdata) :: answer
    type(configuration) :: conf
    integer, parameter :: flH = 11
    integer :: i = 1, j = 1
    
    open(flH, file="/home/doctor/Labs/FortranLabs/Lab2/bin/results.txt",action="write",status="replace")
    
    close(flH)
end subroutine printReport
subroutine printToGnuplot(answer)
    type(resultdata) :: answer
    integer, parameter :: flH = 11
    integer :: i = 1, j = 1
    
    open(flH, file="/home/doctor/Labs/FortranLabs/Lab2/bin/results.txt",action="write",status="replace")
    
    do while(i <= size(answer % calc_result))
        if (.not. (answer % calc_result(i) % time < 0) ) then
            j = 1
            do while (j <= size( answer % calc_result(i) % current_slice % x))
                write(flH, *) answer % calc_result(i) % time, &
                                                              & answer % calc_result(i) % current_slice % x(j), &
                                                              & answer % calc_result(i) % current_slice % values(j)            
            j = j + 1
            end do
        else
            exit
        end if
        write(flH, *) 
        i = i + 1
    end do
    
    close(flH)
end subroutine printToGnuplot

subroutine printToGnuplotAnim(answer)
    type(resultdata) :: answer
    integer, parameter :: flH = 11
    integer :: i = 1, j = 1
    
    open(flH, file="/home/doctor/Labs/FortranLabs/Lab2/bin/anim.txt",action="write",status="replace")
    
    do while(i <= size(answer % calc_result))
        if (.not. (answer % calc_result(i) % time < 0) ) then
            j = 1
            write(flH, *) i-1, size( answer % calc_result(i) % current_slice % x)
            do while (j <= size( answer % calc_result(i) % current_slice % x))
                write(flH, *) j, &
                                                              & answer % calc_result(i) % current_slice % x(j), &
                                                              & answer % calc_result(i) % current_slice % values(j)            
            j = j + 1
            end do
        else
            exit
        end if
        write(flH, *) 
        write(flH, *) 
        i = i + 1
    end do
    
    close(flH)
end subroutine printToGnuplotAnim

end module fileworker

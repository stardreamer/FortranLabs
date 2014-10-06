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
        elseif (fst == "out_time") then
            read(snd,'(f10.3)') conf % out_time 
        elseif (fst == "start_time_slice") then
            read(snd,'(i10)') conf % start_time_slice  
        elseif (fst == "out_freq") then
            read(snd,'(i10)') conf % out_freq
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

    open(flH, file="/home/doctor/Labs/FortranLabs/Lab2/results/report.txt",action="write",status="replace")
    write(flH, *) 'End time: ', conf % stoptime
    write(flH, *) 'Timestep: ', conf % timestep
    write(flH, *) 'Step: ', conf % step
    write(flH, *) 'Alpha: ', conf % alpha
    write(flH, *) 'Eps: ', conf % eps
    write(flH, *) 'Total timesteps: ', size(answer % calc_result)
    write(flH, *) 'Outputed timesteps: ', size(answer % calc_result) / conf % out_freq
    
    close(flH)
end subroutine printReport

subroutine printResult(conf, answer)
    type(resultdata) :: answer
    type(configuration) :: conf
    integer :: i = 1, j = 1
    
    if (conf % out_time > 0) then
        do 
        
            if (answer % calc_result(i) % time >= 0 &
            & .and. answer % calc_result(i) % time < conf % out_time &
            &.and. i < size(answer % calc_result)) then
                i = i + 1
            else
                i = i - 1
                exit
            end if
        end do

        j = 1
        do while (j <= size( answer % calc_result(i) % current_slice % x))
            write(*, *) answer % calc_result(i) % time, &
                                                          & answer % calc_result(i) % current_slice % x(j), &
                                                          & answer % calc_result(i) % current_slice % values(j)            
            j = j + 1
        end do
    else
        return
    end if
end subroutine printResult
subroutine printToGnuplot(conf, answer)
    type(resultdata) :: answer
    type(configuration) :: conf
    integer, parameter :: flH = 11
    integer :: i = 1, j = 1
    
    open(flH, file="/home/doctor/Labs/FortranLabs/Lab2/results/surface.txt",action="write",status="replace")
    
    if (conf % out_freq < 0) then
        conf % out_freq = 1
    end if
    
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
        

        i = i + conf % out_freq

        
    end do
    
    close(flH)
end subroutine printToGnuplot

subroutine printToGnuplotAnim(conf, answer)
    type(resultdata) :: answer
    type(configuration) :: conf
    integer, parameter :: flH = 11
    integer :: i = 1, j = 1
    
    open(flH, file="/home/doctor/Labs/FortranLabs/Lab2/results/anim.txt",action="write",status="replace")
    
    if ( .not. associated(answer % calc_result) ) then
        return
    end if
    
    if (conf % out_freq < 0) then
        conf % out_freq = 1
    end if

    do while(i <= size(answer % calc_result))
        if (.not. (answer % calc_result(i) % time < 0) ) then
            j = 1
            write(flH, *) i, size( answer % calc_result(i) % current_slice % x)
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
        
        i = i + conf % out_freq
        
    end do
    
    close(flH)
end subroutine printToGnuplotAnim

end module fileworker

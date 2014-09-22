module errors
implicit none
integer :: errorcode = 0
integer, parameter :: UNKNOWN_ERROR = 1, OK = 0, SMT_BAD = -1, FILE_ERROR =-2
character (len = 20), dimension(-2:1), private :: error_strings = (/&
                                                        &"File Error          ",&
                                                        &"Smt wrong           ",&
                                                        &"All Right           ",&
                                                        &"Undefinite error    "/)
contains

function getErrorString(information) result(resultString)
    character (len = *), intent(in), optional :: information
    character (len = 250) :: resultString
    
    if (errorcode <= 0) then
        resultString = trim(error_strings(errorcode));
        
        if (present(information)) then
            resultString = trim(resultString) // ": " // trim(information)
        end if
    
    else
        resultString = trim(error_strings(1))
    end if
end function getErrorString
            
end module errors

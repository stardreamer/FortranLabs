module logger
use iso_fortran_env

contains

subroutine printStr(str, outStream)
character (len=*), intent(in)  :: str
integer, intent(in) :: outStream



write(outStream, *) str

end subroutine printStr

end module logger

module numbers_to_word
    implicit none
    character (len=10), dimension(-1:9) :: markers = (/ "minus     ",&
                                                    &"zero      ",&
                                                    &"one       ",&
                                                    &"two       ",&
                                                    &"three     ",&
                                                    &"four      ",&
                                                    &"five      ",&
                                                    &"six       ",&
                                                    &"seven     ",&
                                                    &"eight     ",&
                                                    &"nine      " /)
    
    contains
    
    function NameOfNumber(num) result(numStr)
    integer, intent(in) :: num 
    character (len=20)  :: numStr

    numStr = " "
    
    if (abs(num) > 9) then
        numStr = "Too big number"
    else
        if (num < 0) then
            numStr = TRIM(markers(-1))
        end if
        
        numStr = TRIM(numStr) // " " // TRIM(markers(abs(num)))
        
    end if
    
    end function NameOfNumber
    
end module numbers_to_word

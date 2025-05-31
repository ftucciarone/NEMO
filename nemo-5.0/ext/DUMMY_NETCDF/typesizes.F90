module typesizes
  implicit none
  PUBLIC
  integer, parameter ::   OneByteInt = selected_int_kind(2), &
                          TwoByteInt = selected_int_kind(4), &
                         FourByteInt = selected_int_kind(9), &
                        EightByteInt = selected_int_kind(18)

  integer, parameter ::                                          &
                        FourByteReal = selected_real_kind(P =  6, R =  37), &
                       EightByteReal = selected_real_kind(P = 13, R = 307)
contains
  LOGICAL function byteSizesOK()
  end function byteSizesOK
end module typeSizes

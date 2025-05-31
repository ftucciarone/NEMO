MODULE trdtrc
   USE par_kind
   !!======================================================================
   !!                       ***  MODULE trdtrc  ***
   !!  Dummy module
   !!======================================================================
   !!----------------------------------------------------------------------
   !!   Dummy module                                             NO TOP use
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trd_trc( ptrtrd, kjn, ktrd, kt, Kmm )
      INTEGER ::   kt, kjn, ktrd   
      INTEGER ::   Kmm            ! time level index
      REAL(wp), DIMENSION(:,:,:) ::   ptrtrd 
      WRITE(*,*) 'trd_trc : You should not have seen this print! error?', ptrtrd(1,1,1)
      WRITE(*,*) '  "      "      : You should not have seen this print! error?', kjn, ktrd, kt
   END SUBROUTINE trd_trc

   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!======================================================================
END MODULE trdtrc

MODULE lbclnk
   !!======================================================================
   !!                       ***  MODULE  lbclnk  ***
   !! NEMO        : lateral boundary conditions
   !!=====================================================================
   !! History :  OPA  ! 1997-06  (G. Madec)  Original code
   !!   NEMO     1.0  ! 2002-09  (G. Madec)  F90: Free form and module
   !!            3.2  ! 2009-03  (R. Benshila)  External north fold treatment
   !!            3.5  ! 2012     (S.Mocavero, I. Epicoco)  optimization of BDY comm. via lbc_bdy_lnk and lbc_obc_lnk
   !!            3.4  ! 2012-12  (R. Bourdalle-Badie, G. Reffray)  add a C1D case
   !!            3.6  ! 2015-06  (O. Tintó and M. Castrillo)  add lbc_lnk_multi
   !!            4.0  ! 2017-03  (G. Madec) automatique allocation of array size (use with any 3rd dim size)
   !!             -   ! 2017-04  (G. Madec) remove duplicated routines (lbc_lnk_2d_9, lbc_lnk_2d_multiple, lbc_lnk_3d_gather)
   !!             -   ! 2017-05  (G. Madec) create generic.h90 files to generate all lbc and north fold routines
   !!----------------------------------------------------------------------
   !!           define the generic interfaces of lib_mpp routines
   !!----------------------------------------------------------------------
   !!   lbc_lnk       : generic interface for mpp_lnk_3d and mpp_lnk_2d routines defined in lib_mpp
   !!   lbc_bdy_lnk   : generic interface for mpp_lnk_bdy_2d and mpp_lnk_bdy_3d routines defined in lib_mpp
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain
   USE lib_mpp        ! distributed memory computing library
   USE lbcnfd         ! north fold
   USE in_out_manager ! I/O manager
   USE timing
#if ! defined key_mpi_off
   USE MPI
#endif

   IMPLICIT NONE
   PRIVATE

   INTERFACE lbc_lnk
      MODULE PROCEDURE   lbc_lnk_call_2d_sp, lbc_lnk_call_3d_sp, lbc_lnk_call_4d_sp
      MODULE PROCEDURE   lbc_lnk_call_2d_dp, lbc_lnk_call_3d_dp, lbc_lnk_call_4d_dp
   END INTERFACE

   INTERFACE lbc_lnk_pt2pt
      MODULE PROCEDURE   lbc_lnk_pt2pt_sp, lbc_lnk_pt2pt_dp
   END INTERFACE

   INTERFACE lbc_lnk_neicoll
      MODULE PROCEDURE   lbc_lnk_neicoll_sp ,lbc_lnk_neicoll_dp
   END INTERFACE

   INTERFACE lbc_lnk_iprobe
      MODULE PROCEDURE   lbc_lnk_iprobe_sp, lbc_lnk_iprobe_dp
   END INTERFACE

   INTERFACE lbc_lnk_waitany
      MODULE PROCEDURE   lbc_lnk_waitany_sp, lbc_lnk_waitany_dp
   END INTERFACE

   INTERFACE lbc_lnk_waitall
      MODULE PROCEDURE   lbc_lnk_waitall_sp, lbc_lnk_waitall_dp
   END INTERFACE
   !

   PUBLIC   lbc_lnk            ! ocean/ice lateral boundary conditions

   REAL(dp), DIMENSION(:), ALLOCATABLE ::   buffsnd_dp, buffrcv_dp         ! MPI send/recv buffers
   REAL(sp), DIMENSION(:), ALLOCATABLE ::   buffsnd_sp, buffrcv_sp         ! 
   INTEGER,  DIMENSION(8)              ::   nreq_p2p = MPI_REQUEST_NULL    ! request id for MPI_Isend in point-2-point communication
   INTEGER                             ::   nreq_nei = MPI_REQUEST_NULL    ! request id for mpi_neighbor_ialltoallv
   !! * Substitutions
   !!#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   !!----------------------------------------------------------------------
   !!                   ***   lbc_lnk_call_[234]d_[sd]p   ***
   !!
   !!   * Dummy Argument :
   !!       in    ==>   cdname     ! name of the calling subroutine (for monitoring)
   !!                   ptab       ! array to be loaded (2D, 3D or 4D)
   !!                   cd_nat     ! nature of pt2d array grid-points
   !!                   psgn       ! sign used across the north fold boundary
   !!       inout <=>   ptab_ptr   ! array of 2D, 3D or 4D pointers
   !!                   cdna_ptr   ! nature of ptab array grid-points
   !!                   psgn_ptr   ! sign used across the north fold boundary
   !!                   kfld       ! number of elements that has been attributed
   !!----------------------------------------------------------------------
   !
   !!----------------------------------------------------------------------
   !!
   !!                  ***   lbc_lnk_call_[234]d_[sd]p   ***
   !!                  ***     load_ptr_[234]d_[sd]p     ***
   !!
   !!----------------------------------------------------------------------
   !!
   !!   ----   SINGLE PRECISION VERSIONS
   !!
#define PRECISION sp
# define DIM_2d
#    include "lbc_lnk_call_generic.h90"
# undef  DIM_2d
# define DIM_3d
#    include "lbc_lnk_call_generic.h90"
# undef  DIM_3d
# define DIM_4d
#    include "lbc_lnk_call_generic.h90"
# undef  DIM_4d
#undef PRECISION
   !!
   !!   ----   DOUBLE PRECISION VERSIONS
   !!
#define PRECISION dp
# define DIM_2d
#    include "lbc_lnk_call_generic.h90"
# undef  DIM_2d
# define DIM_3d
#    include "lbc_lnk_call_generic.h90"
# undef  DIM_3d
# define DIM_4d
#    include "lbc_lnk_call_generic.h90"
# undef  DIM_4d
#undef PRECISION
   !
   !!----------------------------------------------------------------------
   !!                   ***  lbc_lnk_pt2pt_[sd]p  ***
   !!                  ***  lbc_lnk_neicoll_[sd]p  ***
   !!
   !!   * Argument : dummy argument use in lbc_lnk_... routines
   !!                cdname    :   name of the calling subroutine (for monitoring)
   !!                ptab      :   pointer of arrays on which the boundary condition is applied
   !!                cd_nat    :   nature of array grid-points
   !!                psgn      :   sign used across the north fold boundary
   !!                kfld      :   number of pt3d arrays
   !!                kfillmode :   optional, method to be use to fill the halos (see jpfill* variables)
   !!                pfillval  :   optional, background value (used with jpfillcopy)
   !!----------------------------------------------------------------------
   !!
   !!   ----   SINGLE PRECISION VERSIONS
   !!
#define PRECISION sp
#  define MPI_TYPE MPI_REAL
#  define BUFFSND buffsnd_sp
#  define BUFFRCV buffrcv_sp
#  include "lbc_lnk_pt2pt_generic.h90"
#  include "lbc_lnk_neicoll_generic.h90"
#  include "lbc_lnk_iprobe_generic.h90"
#  include "lbc_lnk_waitany_generic.h90"
#  include "lbc_lnk_waitall_generic.h90"
#  undef MPI_TYPE
#  undef BUFFSND
#  undef BUFFRCV
#undef PRECISION
   !!
   !!   ----   DOUBLE PRECISION VERSIONS
   !!
#define PRECISION dp
#  define MPI_TYPE MPI_DOUBLE_PRECISION
#  define BUFFSND buffsnd_dp
#  define BUFFRCV buffrcv_dp
#  include "lbc_lnk_pt2pt_generic.h90"
#  include "lbc_lnk_neicoll_generic.h90"
#  include "lbc_lnk_iprobe_generic.h90"
#  include "lbc_lnk_waitany_generic.h90"
#  include "lbc_lnk_waitall_generic.h90"
#  undef MPI_TYPE
#  undef BUFFSND
#  undef BUFFRCV
#undef PRECISION

END MODULE lbclnk

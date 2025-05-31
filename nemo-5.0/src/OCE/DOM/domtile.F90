MODULE domtile
   !!======================================================================
   !!                       ***  MODULE domtile  ***
   !! Tiling utilities
   !!======================================================================
   !! History : 4.2  !  2020-12  (D. Calvert)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dom_tile       : Set/initialise the current tile and domain indices
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain
   !
   USE prtctl         ! Print control (prt_ctl_info routine)
   USE lib_mpp , ONLY : ctl_stop, ctl_warn, mpp_min
   USE domutl  , ONLY : arr_hls
   USE in_out_manager ! I/O manager
   USE timing

   IMPLICIT NONE
   PRIVATE

   INTERFACE dom_tile_copyin
      MODULE PROCEDURE dom_tile_copyin_2d, dom_tile_copyin_3d, dom_tile_copyin_4d
   END INTERFACE dom_tile_copyin
   INTERFACE dom_tile_copyout
      MODULE PROCEDURE dom_tile_copyout_2d, dom_tile_copyout_3d, dom_tile_copyout_4d
   END INTERFACE dom_tile_copyout

   TYPE :: TILE_DATA       ! Derived types for holding data in TILE_WORK
      REAL(wp), DIMENSION(:,:),     ALLOCATABLE :: data2d
      REAL(wp), DIMENSION(:,:,:),   ALLOCATABLE :: data3d
      REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE :: data4d
      LOGICAL                                   :: in_use = .FALSE.        ! Is there an array stored in this slot?
   END TYPE TILE_DATA

   TYPE :: TILE_WORK       ! Main derived type used by dom_tile_copy* subroutines
      CHARACTER(len=20)                          :: tag = 'NOT_IN_USE'     ! Name of the array stored in this slot
      INTEGER, DIMENSION(2)                      :: nhls = -1              ! Halo size (i, j) of the array
      TYPE(TILE_DATA), ALLOCATABLE, DIMENSION(:) :: datat                  ! Stored data- tile arrays (copied from %data)
      TYPE(TILE_DATA)                            :: data                   ! Stored data- original array
   END TYPE TILE_WORK

   PUBLIC dom_tile         ! called by step.F90
   PUBLIC dom_tile_start   ! called by various
   PUBLIC dom_tile_stop    ! "      "
   PUBLIC dom_tile_init    ! called by domain.F90
   PUBLIC dom_tile_copyin
   PUBLIC dom_tile_copyout

   TYPE(TILE_WORK), DIMENSION(20) :: twrk               ! Derived type used by dom_tile_copy(in|out)

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dom_tile_init
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dom_tile_init  ***
      !!
      !! ** Purpose :   Initialise tile domain variables
      !!
      !! ** Action  : - ntsi, ntsj     : start of internal part of domain
      !!              - ntei, ntej     : end of internal part of domain
      !!              - ntile          : current tile number
      !!              - nijtile        : total number of tiles
      !!              - l_istiled      : whether tiling is currently active or not
      !!----------------------------------------------------------------------
      INTEGER ::   jt                                     ! dummy loop argument
      INTEGER ::   iitile, ijtile, iijtile                ! Local integers
      LOGICAL ::   ltile                                  ! Value of ln_tile after checks
      !!----------------------------------------------------------------------
      ntile = 0                     ! Initialise to full domain
      nijtile = 1
      ntsi = Nis0
      ntsj = Njs0
      ntei = Nie0
      ntej = Nje0
      l_istiled = .FALSE.
      ltile = ln_tile

      IF( ln_tile ) THEN            ! Calculate number of tiles
         iitile = Ni_0 / nn_ltile_i
         ijtile = Nj_0 / nn_ltile_j
         IF( MOD( Ni_0, nn_ltile_i ) /= 0 ) iitile = iitile + 1
         IF( MOD( Nj_0, nn_ltile_j ) /= 0 ) ijtile = ijtile + 1

         ! If any process has only one tile, we must disable tiling globally to avoid errors with blocking MPI
         ! communications within "IF( ntile == nijtile )" type statements. These will be executed on processes with
         ! only one tile before processes with multiple tiles, leading to incorrect pairing of MPI send/receive calls
         iijtile = iitile * ijtile
         CALL mpp_min( 'domtile', iijtile )
         IF( iijtile == 1 ) THEN
            ln_tile = .FALSE.
         ELSE
            nijtile = iitile * ijtile
         ENDIF
      ENDIF

      IF( ln_tile ) THEN            ! Calculate tile domain indices
         ALLOCATE( ntsi_a(0:nijtile), ntsj_a(0:nijtile), ntei_a(0:nijtile), ntej_a(0:nijtile) )

         ntsi_a(0) = Nis0                 ! Full domain
         ntsj_a(0) = Njs0
         ntei_a(0) = Nie0
         ntej_a(0) = Nje0

         DO jt = 1, nijtile               ! Tile domains
            ntsi_a(jt) = Nis0 + nn_ltile_i * MOD(jt - 1, iitile)
            ntsj_a(jt) = Njs0 + nn_ltile_j * ((jt - 1) / iitile)
            ntei_a(jt) = MIN(ntsi_a(jt) + nn_ltile_i - 1, Nie0)
            ntej_a(jt) = MIN(ntsj_a(jt) + nn_ltile_j - 1, Nje0)
         ENDDO
      ENDIF

      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'dom_tile : Domain tiling decomposition'
         WRITE(numout,*) '~~~~~~~~'
         IF( ln_tile ) THEN
            WRITE(numout,*) 'The domain will be decomposed into tiles of size', nn_ltile_i, 'x', nn_ltile_j
            WRITE(numout,*) '    ', iitile, 'tiles in i'
            WRITE(numout,*) '        Starting indices'
            WRITE(numout,*) '            ', (ntsi_a(jt), jt=1, iitile)
            WRITE(numout,*) '        Ending indices'
            WRITE(numout,*) '            ', (ntei_a(jt), jt=1, iitile)
            WRITE(numout,*) '    ', ijtile, 'tiles in j'
            WRITE(numout,*) '        Starting indices'
            WRITE(numout,*) '            ', (ntsj_a(jt), jt=1, nijtile, iitile)
            WRITE(numout,*) '        Ending indices'
            WRITE(numout,*) '            ', (ntej_a(jt), jt=1, nijtile, iitile)
         ELSE
            IF( ltile ) THEN
               WRITE(numout,*) 'The chosen tile sizes', nn_ltile_i, 'x', nn_ltile_j, &
                  &            'result in a single tile for at least one MPI domain- domain tiling will not be used'
            ELSE
               WRITE(numout,*) 'No domain tiling'
            ENDIF
            WRITE(numout,*) '    i indices =', ntsi, ':', ntei
            WRITE(numout,*) '    j indices =', ntsj, ':', ntej
         ENDIF
      ENDIF
   END SUBROUTINE dom_tile_init


   SUBROUTINE dom_tile( ktsi, ktsj, ktei, ktej, ktile, lddone, cstr )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dom_tile  ***
      !!
      !! ** Purpose :   Set the current tile and its domain indices
      !!
      !! ** Action  : - ktsi, ktsj     : start of internal part of domain
      !!              - ktei, ktej     : end of internal part of domain
      !!              - ktile          : set the current tile number (ntile)
      !!----------------------------------------------------------------------
      INTEGER, INTENT(out) :: ktsi, ktsj, ktei, ktej      ! Tile domain indices
      INTEGER, INTENT(in)  :: ktile                       ! Tile number
      LOGICAL, INTENT(in), OPTIONAL :: lddone             ! Whether we have finished working on the previous tile
      CHARACTER(len=*), INTENT(in), OPTIONAL   :: cstr    ! Debug information (added to warnings)
      CHARACTER(len=23) :: clstr
      LOGICAL :: lldone
      CHARACTER(len=11)   :: charout
      !!----------------------------------------------------------------------
      lldone = .TRUE.
      IF( PRESENT(lddone) ) lldone = lddone
      clstr = ''
      IF( PRESENT(cstr) ) clstr = TRIM(' ('//TRIM(cstr)//')')

      IF( .NOT. ln_tile ) CALL ctl_stop('STOP', 'Cannot use dom_tile with ln_tile = .false.')
      IF( lldone ) THEN
         IF( .NOT. l_istiled ) THEN
            CALL ctl_warn('Cannot call dom_tile when tiling is inactive'//clstr)
            RETURN
         ENDIF

         ntile = ktile                                      ! Set the new tile
         IF(sn_cfctl%l_prtctl) THEN
            WRITE(charout, FMT="('ntile =', I4)") ntile
            CALL prt_ctl_info( charout )
         ENDIF
      ENDIF

      ktsi = ntsi_a(ktile)                                  ! Set the domain indices
      ktsj = ntsj_a(ktile)
      ktei = ntei_a(ktile)
      ktej = ntej_a(ktile)
   END SUBROUTINE dom_tile


   SUBROUTINE dom_tile_start( ldhold, cstr )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dom_tile_start  ***
      !!
      !! ** Purpose : Start or resume the use of tiling
      !!
      !! ** Method  : dom_tile_start & dom_tile_stop are used to declare a tiled region of code.
      !!
      !!              Tiling is active/inactive (l_istiled = .true./.false.) within/outside of this code region.
      !!              After enabling tiling, no tile will initially be set (the full domain will be used) and dom_tile must
      !!              be called to set a specific tile to work on. Furthermore, all tiles will be marked as incomplete
      !!              (ln_tilefin(:) = .false.).
      !!
      !!              Tiling can be paused/resumed within the tiled code region by calling dom_tile_stop/dom_tile_start
      !!              with ldhold = .true.. This can be used to temporarily revert back to using the full domain.
      !!
      !!                 CALL dom_tile_start                                  ! Enable tiling
      !!                    CALL dom_tile(ntsi, ntei, ntsj, ntej, ktile=n)    ! Set current tile "n"
      !!                    ...
      !!                    CALL dom_tile_stop(.TRUE.)                        ! Pause tiling (temporarily disable)
      !!                    ...
      !!                    CALL dom_tile_start(.TRUE.)                       ! Resume tiling
      !!                 CALL dom_tile_stop                                   ! Disable tiling
      !!----------------------------------------------------------------------
      LOGICAL, INTENT(in), OPTIONAL :: ldhold            ! Resume (.true.) or start (.false.)
      LOGICAL :: llhold
      CHARACTER(len=*), INTENT(in), OPTIONAL   :: cstr   ! Debug information (added to warnings)
      CHARACTER(len=23) :: clstr
      !!----------------------------------------------------------------------
      llhold = .FALSE.
      IF( PRESENT(ldhold) ) llhold = ldhold
      clstr = ''
      IF( PRESENT(cstr) ) clstr = TRIM(' ('//TRIM(cstr)//')')

      IF( .NOT. ln_tile ) CALL ctl_stop('STOP', 'Cannot resume/start tiling as ln_tile = .false.')
      IF( l_istiled ) THEN
         CALL ctl_warn('Cannot resume/start tiling as it is already active'//clstr)
         RETURN
      ! TODO: [tiling] this warning will always be raised outside a tiling loop (cannot check for pause rather than stop)
      ELSE IF( llhold .AND. ntile == 0 ) THEN
         CALL ctl_warn('Cannot resume tiling as it is not paused'//clstr)
         RETURN
      ENDIF

      ! Whether resumed or started, the tiling is made active. If resumed, the domain indices for the current tile are used.
      IF( llhold ) CALL dom_tile(ntsi, ntsj, ntei, ntej, ktile=ntile, lddone=.FALSE., cstr='dom_tile_start'//clstr)
      l_istiled = .TRUE.
   END SUBROUTINE dom_tile_start


   SUBROUTINE dom_tile_stop( ldhold, cstr )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dom_tile_stop  ***
      !!
      !! ** Purpose : End or pause the use of tiling
      !!
      !! ** Method  : See dom_tile_start
      !!----------------------------------------------------------------------
      LOGICAL, INTENT(in), OPTIONAL :: ldhold            ! Pause (.true.) or stop (.false.)
      LOGICAL :: llhold
      CHARACTER(len=*), INTENT(in), OPTIONAL   :: cstr   ! Debug information (added to warnings)
      CHARACTER(len=23) :: clstr
      !!----------------------------------------------------------------------
      llhold = .FALSE.
      IF( PRESENT(ldhold) ) llhold = ldhold
      clstr = ''
      IF( PRESENT(cstr) ) clstr = TRIM(' ('//TRIM(cstr)//')')

      IF( .NOT. ln_tile ) CALL ctl_stop('STOP', 'Cannot pause/stop tiling as ln_tile = .false.')
      IF( .NOT. l_istiled ) THEN
         CALL ctl_warn('Cannot pause/stop tiling as it is inactive'//clstr)
         RETURN
      ENDIF

      ! Whether paused or stopped, the tiling is made inactive and the full domain indices are used.
      ! If stopped, there is no active tile (ntile = 0)
      CALL dom_tile(ntsi, ntsj, ntei, ntej, ktile=0, lddone=(.NOT. llhold), cstr='dom_tile_stop'//clstr)
      l_istiled = .FALSE.
   END SUBROUTINE dom_tile_stop


   SUBROUTINE dom_tile_copyin_2d( cdvar1, pa1, cdvar2, pa2, cdvar3, pa3, cdvar4, pa4 )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dom_tile_copyin  ***
      !!
      !! ** Purpose : The purpose of this subroutine is to ensure that operations on pa[1234] within
      !!              the tiled region are independent for each tile. It is used together with dom_tile_copyout:
      !!
      !!                   * dom_tile_copyin replaces pa[1234] with a new copy of the data for the current tile
      !!                   * Each tile then works with a different copy of pa[1234], each the size of the tile
      !!                   * dom_tile_copyout copies data from pa[1234] back into the original array, then replaces
      !!                     pa[1234] with the original array
      !!
      !! ** Method  : This subroutine must be called twice (per array) at the timestepping level:
      !!
      !!                   * Before the tiled region (immediately before the call to dom_tile_start)
      !!                   * Inside the tiled region (immediately after the call to dom_tile)
      !!
      !!              This is necessary to separate serial and parallel processes when using OpenMP.
      !!              For a 2D array (pa1), this process is as follows:
      !!
      !!                   Before the tiled region (serial part)
      !!                      * Look for an index in twrk (jt) to store pa1
      !!                      * Move the pa1 allocation to twrk(jt)%data%data2d (pa1 is deallocated)
      !!                      * Allocate tile-sized arrays for each tile in twrk(jt)%datat(:)%data2d
      !!                      * Copy data from twrk(jt)%data%data2d to twrk(jt)%datat(:)%data2d
      !!
      !!                   Inside the tiled region (parallel part)
      !!                      * Look for the index in twrk (jt) where pa1 was stored
      !!                      * Move the twrk(jt)%datat(ntile)%data2d allocation to pa1
      !!
      !!              move_alloc is used to transfer memory allocations between arrays without actually copying
      !!              any data. Data is only copied once, which must be done serially (before the tiled region)
      !!              to preserve results.
      !!
      !! ** Action  : - cdvar[1234] : A name for the array
      !!              - pa[1234]    : An ALLOCATABLE array on the horizontal MPI domain
      !!              Name `cdvar1` corresponds to array `pa1`, `cdvar2` to `pa2` and so on
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout)           :: pa1
      REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pa2, pa3, pa4
      CHARACTER(len=*),                      INTENT(in   )           :: cdvar1
      CHARACTER(len=*),                      INTENT(in   ), OPTIONAL :: cdvar2, cdvar3, cdvar4
      !!----------------------------------------------------------------------
      IF( .NOT. ln_tile ) RETURN
      CALL put_twrk( cdvar1, pa2d=pa1 )
      IF( PRESENT(pa2) .AND. PRESENT(cdvar2) ) CALL put_twrk( cdvar2, pa2d=pa2 )
      IF( PRESENT(pa3) .AND. PRESENT(cdvar3) ) CALL put_twrk( cdvar3, pa2d=pa3 )
      IF( PRESENT(pa4) .AND. PRESENT(cdvar4) ) CALL put_twrk( cdvar4, pa2d=pa4 )
   END SUBROUTINE dom_tile_copyin_2d


   SUBROUTINE dom_tile_copyin_3d( cdvar1, pa1, cdvar2, pa2, cdvar3, pa3, cdvar4, pa4 )
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE, INTENT(inout)           :: pa1
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pa2, pa3, pa4
      CHARACTER(len=*),                        INTENT(in   )           :: cdvar1
      CHARACTER(len=*),                        INTENT(in   ), OPTIONAL :: cdvar2, cdvar3, cdvar4
      !!----------------------------------------------------------------------
      IF( .NOT. ln_tile ) RETURN
      CALL put_twrk( cdvar1, pa3d=pa1 )
      IF( PRESENT(pa2) .AND. PRESENT(cdvar2) ) CALL put_twrk( cdvar2, pa3d=pa2 )
      IF( PRESENT(pa3) .AND. PRESENT(cdvar3) ) CALL put_twrk( cdvar3, pa3d=pa3 )
      IF( PRESENT(pa4) .AND. PRESENT(cdvar4) ) CALL put_twrk( cdvar4, pa3d=pa4 )
   END SUBROUTINE dom_tile_copyin_3d


   SUBROUTINE dom_tile_copyin_4d( cdvar1, pa1, cdvar2, pa2, cdvar3, pa3, cdvar4, pa4 )
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE, INTENT(inout)           :: pa1
      REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pa2, pa3, pa4
      CHARACTER(len=*),                          INTENT(in   )           :: cdvar1
      CHARACTER(len=*),                          INTENT(in   ), OPTIONAL :: cdvar2, cdvar3, cdvar4
      !!----------------------------------------------------------------------
      IF( .NOT. ln_tile ) RETURN
      CALL put_twrk( cdvar1, pa4d=pa1 )
      IF( PRESENT(pa2) .AND. PRESENT(cdvar2) ) CALL put_twrk( cdvar2, pa4d=pa2 )
      IF( PRESENT(pa3) .AND. PRESENT(cdvar3) ) CALL put_twrk( cdvar3, pa4d=pa3 )
      IF( PRESENT(pa4) .AND. PRESENT(cdvar4) ) CALL put_twrk( cdvar4, pa4d=pa4 )
   END SUBROUTINE dom_tile_copyin_4d


   SUBROUTINE dom_tile_copyout_2d( cdvar1, pa1, cdvar2, pa2, cdvar3, pa3, cdvar4, pa4 )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dom_tile_copyout  ***
      !!
      !! ** Purpose : The purpose of this subroutine is to ensure that operations on pa[1234] within
      !!              the tiled region are independent for each tile. It is used together with dom_tile_copyin:
      !!
      !!                   * dom_tile_copyin replaces pa[1234] with a new copy of the data for the current tile
      !!                   * Each tile then works with a different copy of pa[1234], each the size of the tile
      !!                   * dom_tile_copyout copies data from pa[1234] back into the original array, then replaces
      !!                     pa[1234] with the original array
      !!
      !! ** Method  : This subroutine must be called twice (per array) at the timestepping level:
      !!
      !!                   * Inside the tiled region (immediately before the end of the loop over tiles)
      !!                   * After the tiled region (immediately after the call to dom_tile_stop)
      !!
      !!              This is necessary to separate serial and parallel processes when using OpenMP.
      !!              For a 2D array (pa1), this process is as follows:
      !!
      !!                   Inside the tiled region (parallel part)
      !!                      * Look for the index in twrk (jt) where pa1 was originally stored by dom_tile_copyin
      !!                      * Move the pa1 allocation to twrk(jt)%datat(ntile)%data2d (pa1 is deallocated)
      !!                      * Copy data from twrk(jt)%datat(ntile)%data2d to twrk(jt)%data%data2d
      !!                      * Deallocate twrk(jt)%datat(ntile)%data2d
      !!
      !!                   After the tiled region (serial part)
      !!                      * Look for the index in twrk (jt) where pa1 was originally stored by dom_tile_copyin
      !!                      * Move the twrk(jt)%data%data2d allocation to pa1
      !!
      !!              move_alloc is used to transfer memory allocations between arrays without actually copying any
      !!              data. Data is only copied once, on a tile-by-tile basis within the parallel part of the code.
      !!
      !! ** Action  : - cdvar[1234] : A name for the array
      !!              - pa[1234]    : An ALLOCATABLE array on the horizontal MPI domain
      !!              Name `cdvar1` corresponds to array `pa1`, `cdvar2` to `pa2` and so on
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout)           :: pa1
      REAL(wp), DIMENSION(:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pa2, pa3, pa4
      CHARACTER(len=*),                      INTENT(in   )           :: cdvar1
      CHARACTER(len=*),                      INTENT(in   ), OPTIONAL :: cdvar2, cdvar3, cdvar4
      !!----------------------------------------------------------------------
      IF( .NOT. ln_tile ) RETURN
      CALL get_twrk( cdvar1, pa2d=pa1 )
      IF( PRESENT(pa2) .AND. PRESENT(cdvar2) ) CALL get_twrk( cdvar2, pa2d=pa2 )
      IF( PRESENT(pa3) .AND. PRESENT(cdvar3) ) CALL get_twrk( cdvar3, pa2d=pa3 )
      IF( PRESENT(pa4) .AND. PRESENT(cdvar4) ) CALL get_twrk( cdvar4, pa2d=pa4 )
   END SUBROUTINE dom_tile_copyout_2d


   SUBROUTINE dom_tile_copyout_3d( cdvar1, pa1, cdvar2, pa2, cdvar3, pa3, cdvar4, pa4 )
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE, INTENT(inout)           :: pa1
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pa2, pa3, pa4
      CHARACTER(len=*),                        INTENT(in   )           :: cdvar1
      CHARACTER(len=*),                        INTENT(in   ), OPTIONAL :: cdvar2, cdvar3, cdvar4
      !!----------------------------------------------------------------------
      IF( .NOT. ln_tile ) RETURN
      CALL get_twrk( cdvar1, pa3d=pa1 )
      IF( PRESENT(pa2) .AND. PRESENT(cdvar2) ) CALL get_twrk( cdvar2, pa3d=pa2 )
      IF( PRESENT(pa3) .AND. PRESENT(cdvar3) ) CALL get_twrk( cdvar3, pa3d=pa3 )
      IF( PRESENT(pa4) .AND. PRESENT(cdvar4) ) CALL get_twrk( cdvar4, pa3d=pa4 )
   END SUBROUTINE dom_tile_copyout_3d


   SUBROUTINE dom_tile_copyout_4d( cdvar1, pa1, cdvar2, pa2, cdvar3, pa3, cdvar4, pa4 )
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE, INTENT(inout)           :: pa1
      REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pa2, pa3, pa4
      CHARACTER(len=*),                          INTENT(in   )           :: cdvar1
      CHARACTER(len=*),                          INTENT(in   ), OPTIONAL :: cdvar2, cdvar3, cdvar4
      !!----------------------------------------------------------------------
      IF( .NOT. ln_tile ) RETURN
      CALL get_twrk( cdvar1, pa4d=pa1 )
      IF( PRESENT(pa2) .AND. PRESENT(cdvar2) ) CALL get_twrk( cdvar2, pa4d=pa2 )
      IF( PRESENT(pa3) .AND. PRESENT(cdvar3) ) CALL get_twrk( cdvar3, pa4d=pa3 )
      IF( PRESENT(pa4) .AND. PRESENT(cdvar4) ) CALL get_twrk( cdvar4, pa4d=pa4 )
   END SUBROUTINE dom_tile_copyout_4d


   INTEGER FUNCTION twrk_id( cdvar )
      !!----------------------------------------------------------------------
      !!                  ***  FUNCTION twrk_id  ***
      !!
      !! ** Purpose : Return an index into twrk(:) where the array described by cdvar is (or can be) stored
      !!
      !! ** Method  : Search twrk(:)%tag entries and return an index (twrk_id).
      !!              * If the array is already stored in twrk, twrk_id will correspond to where the array
      !!                is stored, such that twrk(twrk_id)%tag == cdvar.
      !!              * If the array is not stored in twrk, twrk_id will correspond to the first available
      !!                slot in twrk where it can be stored, such that twrk(twrk_id)%data%in_use == .FALSE.
      !!              * If the array is not stored in twrk and there are no available slots to store it,
      !!                return a null index (twrk_id = -1)
      !!
      !! ** Action  : - twrk_id : An index into twrk(:)
      !!----------------------------------------------------------------------
      CHARACTER(len=*), INTENT(in) :: cdvar
      !
      INTEGER :: jt
      !!----------------------------------------------------------------------
      twrk_id = -1                                                      ! Default- no slot found

      ! Search through twrk(:)%tag entries
      DO jt = 1, SIZE( twrk )
         IF( twrk_id == -1 .AND. .NOT. twrk(jt)%data%in_use ) THEN      ! Found a free slot
            twrk_id = jt
         ELSE IF( TRIM(twrk(jt)%tag) == TRIM(cdvar) ) THEN              ! Found the stored array
            twrk_id = jt
            EXIT
         ENDIF
      ENDDO
   END FUNCTION twrk_id


   SUBROUTINE put_twrk( cdvar, pa2d, pa3d, pa4d )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE put_twrk  ***
      !!
      !! ** Purpose : Main functionality for dom_tile_copyin (refer to the docstring for this subroutine)
      !!
      !! ** Action  : - cdvar    : A name for the array
      !!              - pa[234]d : An ALLOCATABLE array on the horizontal MPI domain
      !!----------------------------------------------------------------------
      CHARACTER(len=*),                          INTENT(in   )           :: cdvar
      REAL(wp), DIMENSION(:,:),     ALLOCATABLE, INTENT(inout), OPTIONAL :: pa2d
      REAL(wp), DIMENSION(:,:,:),   ALLOCATABLE, INTENT(inout), OPTIONAL :: pa3d
      REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pa4d
      !
      INTEGER :: it, ipk, ipl
      INTEGER, DIMENSION(2) :: ihls
      !!----------------------------------------------------------------------
      IF( ln_timing ) CALL timing_start('put_twrk')

      ! Get the index of the data in twrk
      it = twrk_id( cdvar )

      ! Untiled part
      IF( .NOT. l_istiled ) THEN
         ! Check that a valid index was returned- if not, twrk is probably full
         IF( it == -1 ) THEN
            CALL ctl_stop('STOP', 'put_twrk: twrk has no free space, suggest increasing size' )

         ! Check that cdvar is not already a stored variable
         ELSE IF( twrk(it)%data%in_use ) THEN
            CALL ctl_stop('STOP', 'put_twrk: variable "'//cdvar//'" is already stored in twrk' )
         ENDIF

         ! Determine the number of halo points in the array and move the input array to %data
         IF( PRESENT(pa2d) ) THEN
            IF( .NOT. ALLOCATED(pa2d) ) &
               &  CALL ctl_stop('STOP', 'put_twrk: the array corresponding to variable "'//cdvar//'" must be allocated' )
            ihls(:) = arr_hls( SIZE(pa2d, 1), SIZE(pa2d, 2), ldsize=.TRUE. )
            CALL move_alloc( pa2d, twrk(it)%data%data2d )

         ELSE IF( PRESENT(pa3d) ) THEN
            IF( .NOT. ALLOCATED(pa3d) ) &
               &  CALL ctl_stop('STOP', 'put_twrk: the array corresponding to variable "'//cdvar//'" must be allocated' )
            ihls(:) = arr_hls( SIZE(pa3d, 1), SIZE(pa3d, 2), ldsize=.TRUE. )
            CALL move_alloc( pa3d, twrk(it)%data%data3d )

         ELSE IF( PRESENT(pa4d) ) THEN
            IF( .NOT. ALLOCATED(pa4d) ) &
               &  CALL ctl_stop('STOP', 'put_twrk: the array corresponding to variable "'//cdvar//'" must be allocated' )
            ihls(:) = arr_hls( SIZE(pa4d, 1), SIZE(pa4d, 2), ldsize=.TRUE. )
            CALL move_alloc( pa4d, twrk(it)%data%data4d )
         ENDIF

         ! Set twrk attributes and allocate %datat
         ALLOCATE( twrk(it)%datat(nijtile) )
         twrk(it)%data%in_use = .TRUE.
         twrk(it)%datat(:)%in_use = .TRUE.
         twrk(it)%tag = cdvar
         twrk(it)%nhls(:) = ihls(:)

         ! Copy data from %data to %datat (must be done for all tiles at once)
         CALL copyin_twrk( it )

      ! Tiled part
      ELSE
         ! Check that cdvar is a stored variable
         IF( .NOT. twrk(it)%data%in_use ) THEN
             CALL ctl_stop('STOP', 'put_twrk: variable "'//cdvar//'" has not been stored in twrk' )

         ! Check that cdvar has not already been moved from twrk for this tile
         ELSE IF( .NOT. twrk(it)%datat(ntile)%in_use ) THEN
             CALL ctl_stop('STOP', 'put_twrk: has already been called for variable "'//cdvar//'"' )
         ENDIF

         ! Move %datat(ntile) to the input array
         IF( PRESENT(pa2d) ) THEN
            CALL move_alloc( twrk(it)%datat(ntile)%data2d, pa2d )
         ELSE IF( PRESENT(pa3d) ) THEN
            CALL move_alloc( twrk(it)%datat(ntile)%data3d, pa3d )
         ELSE IF( PRESENT(pa4d) ) THEN
            CALL move_alloc( twrk(it)%datat(ntile)%data4d, pa4d )
         ENDIF

         twrk(it)%datat(ntile)%in_use = .FALSE.
      ENDIF

      IF( ln_timing ) CALL timing_stop('put_twrk')
   END SUBROUTINE put_twrk


   SUBROUTINE get_twrk( cdvar, pa2d, pa3d, pa4d )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE get_twrk  ***
      !!
      !! ** Purpose : Main functionality for dom_tile_copyout (refer to the docstring for this subroutine)
      !!
      !! ** Action  : - cdvar    : A name for the array
      !!              - pa[234]d : An ALLOCATABLE array on the horizontal MPI domain
      !!----------------------------------------------------------------------
      CHARACTER(len=*),                          INTENT(in   )           :: cdvar
      REAL(wp), DIMENSION(:,:),     ALLOCATABLE, INTENT(inout), OPTIONAL :: pa2d
      REAL(wp), DIMENSION(:,:,:),   ALLOCATABLE, INTENT(inout), OPTIONAL :: pa3d
      REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE, INTENT(inout), OPTIONAL :: pa4d
      !
      INTEGER :: it
      !!----------------------------------------------------------------------
      IF( ln_timing ) CALL timing_start('get_twrk')

      ! Get the index of the data in twrk
      it = twrk_id( cdvar )

      ! Check that cdvar is a stored variable
      IF( .NOT. twrk(it)%data%in_use ) CALL ctl_stop('STOP', 'get_twrk: variable "'//cdvar//'" has not been stored in twrk' )

      ! Tiled part
      IF( l_istiled ) THEN
         ! Move the input array to %datat(ntile)
         IF( PRESENT(pa2d) ) THEN
            IF( .NOT. ALLOCATED(pa2d) ) &
               &  CALL ctl_stop('STOP', 'get_twrk: the array corresponding to variable "'//cdvar//'" must be allocated' )
            CALL move_alloc( pa2d, twrk(it)%datat(ntile)%data2d )

         ELSE IF( PRESENT(pa3d) ) THEN
            IF( .NOT. ALLOCATED(pa3d) ) &
               &  CALL ctl_stop('STOP', 'get_twrk: the array corresponding to variable "'//cdvar//'" must be allocated' )
            CALL move_alloc( pa3d, twrk(it)%datat(ntile)%data3d )

         ELSE IF( PRESENT(pa4d) ) THEN
            IF( .NOT. ALLOCATED(pa4d) ) &
               &  CALL ctl_stop('STOP', 'get_twrk: the array corresponding to variable "'//cdvar//'" must be allocated' )
            CALL move_alloc( pa4d, twrk(it)%datat(ntile)%data4d )
         ENDIF

         ! Copy data from %datat to %data
         CALL copyout_twrk( it )

      ! Untiled part
      ELSE
         ! Move %data to the input array
         IF( PRESENT(pa2d) ) THEN
            CALL move_alloc( twrk(it)%data%data2d, pa2d )
         ELSE IF( PRESENT(pa3d) ) THEN
            CALL move_alloc( twrk(it)%data%data3d, pa3d )
         ELSE IF( PRESENT(pa4d) ) THEN
            CALL move_alloc( twrk(it)%data%data4d, pa4d )
         ENDIF

         ! Reset twrk attributes and deallocate %datat
         DEALLOCATE( twrk(it)%datat )
         twrk(it)%data%in_use = .FALSE.
         twrk(it)%tag = 'NOT_IN_USE'
         twrk(it)%nhls(:) = -1
      ENDIF

      IF( ln_timing ) CALL timing_stop('get_twrk')
   END SUBROUTINE get_twrk


   SUBROUTINE copyin_twrk( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE copyin_twrk  ***
      !!
      !! ** Purpose : Called by put_twrk to perform data copying and array allocation
      !!
      !! ** Method  : For each tile (jtile) and assuming a 2D data array:
      !!                * Allocate twrk(kt)%datat(jtile)%data2d
      !!                * Copy data from twrk(kt)%data%data2d to twrk(kt)%datat(jtile)%data2d
      !!
      !! ** Action  : - kt : The index into twrk(:) containing the target data
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) :: kt
      !
      INTEGER :: ji, jj, jk, jl, jtile
      INTEGER :: ipk, ipl, ihls_max
      INTEGER, DIMENSION(2) :: ihls
      !!----------------------------------------------------------------------
      IF( ln_timing ) CALL timing_start('copyin_twrk')

      ! Use the same number of halo points as the array stored in %data
      ihls(:) = twrk(kt)%nhls(:)
      ihls_max = MAX( ihls(1), ihls(2) )

      CALL dom_tile_start
      DO jtile = 1, nijtile
         ! Get domain indices for the tile
         CALL dom_tile( ntsi, ntsj, ntei, ntej, jtile, lddone=.FALSE. )

         ! Allocate %datat(ntile) and copy data from %data to %datat(ntile)
         IF( ALLOCATED(twrk(kt)%data%data2d) ) THEN
            ALLOCATE( twrk(kt)%datat(jtile)%data2d(T2D(ihls_max)) )

            DO_2D( ihls(1), ihls(1), ihls(2), ihls(2) )
               twrk(kt)%datat(jtile)%data2d(ji,jj) = twrk(kt)%data%data2d(ji,jj)
            END_2D

         ELSE IF( ALLOCATED(twrk(kt)%data%data3d) ) THEN
            ipk = SIZE( twrk(kt)%data%data3d, 3 )
            ALLOCATE( twrk(kt)%datat(jtile)%data3d(T2D(ihls_max),ipk) )

            DO_3D( ihls(1), ihls(1), ihls(2), ihls(2), 1, ipk )
               twrk(kt)%datat(jtile)%data3d(ji,jj,jk) = twrk(kt)%data%data3d(ji,jj,jk)
            END_3D

         ELSE IF( ALLOCATED(twrk(kt)%data%data4d) ) THEN
            ipk = SIZE( twrk(kt)%data%data4d, 3 )
            ipl = SIZE( twrk(kt)%data%data4d, 4 )
            ALLOCATE( twrk(kt)%datat(jtile)%data4d(T2D(ihls_max),ipk,ipl) )

            DO jl = 1, ipl
               DO_3D( ihls(1), ihls(1), ihls(2), ihls(2), 1, ipk )
                  twrk(kt)%datat(jtile)%data4d(ji,jj,jk,jl) = twrk(kt)%data%data4d(ji,jj,jk,jl)
               END_3D
            END DO
         ENDIF
      END DO
      CALL dom_tile_stop

      IF( ln_timing ) CALL timing_stop('copyin_twrk')
   END SUBROUTINE copyin_twrk


   SUBROUTINE copyout_twrk( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE copyout_twrk  ***
      !!
      !! ** Purpose : Called by get_twrk to perform data copying and array deallocation
      !!
      !! ** Method  : For the current tile (ntile) and assuming a 2D data array:
      !!                * Copy data from twrk(kt)%datat(ntile)%data2d to twrk(kt)%data%data2d
      !!                * Deallocate twrk(kt)%datat(ntile)%data2d
      !!
      !! ** Action  : - kt : The index into twrk(:) containing the target data
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) :: kt
      !
      INTEGER :: ji, jj, jk, jl
      INTEGER :: itl, itr, itb, itt
      INTEGER :: ipk, ipl
      !!----------------------------------------------------------------------
      IF( ln_timing ) CALL timing_start('copyout_twrk')

      ! Only copy halo points if they correspond to haloes on the MPI domain
      IF( ntsi == Nis0 ) THEN ; itl = twrk(kt)%nhls(1) ; ELSE ; itl = 0 ; ENDIF
      IF( ntei == Nie0 ) THEN ; itr = twrk(kt)%nhls(1) ; ELSE ; itr = 0 ; ENDIF
      IF( ntsj == Njs0 ) THEN ; itb = twrk(kt)%nhls(2) ; ELSE ; itb = 0 ; ENDIF
      IF( ntej == Nje0 ) THEN ; itt = twrk(kt)%nhls(2) ; ELSE ; itt = 0 ; ENDIF

      ! Copy data from %datat(ntile) to %data and deallocate %datat(ntile)
      IF( ALLOCATED(twrk(kt)%data%data2d) ) THEN
         DO_2D( itl, itr, itb, itt )
            twrk(kt)%data%data2d(ji,jj) = twrk(kt)%datat(ntile)%data2d(ji,jj)
         END_2D
         DEALLOCATE( twrk(kt)%datat(ntile)%data2d )

      ELSE IF( ALLOCATED(twrk(kt)%data%data3d) ) THEN
         ipk = SIZE( twrk(kt)%data%data3d, 3 )
         DO_3D( itl, itr, itb, itt, 1, ipk )
            twrk(kt)%data%data3d(ji,jj,jk) = twrk(kt)%datat(ntile)%data3d(ji,jj,jk)
         END_3D
         DEALLOCATE( twrk(kt)%datat(ntile)%data3d )

      ELSE IF( ALLOCATED(twrk(kt)%data%data4d) ) THEN
         ipk = SIZE( twrk(kt)%data%data4d, 3 )
         ipl = SIZE( twrk(kt)%data%data4d, 4 )
         DO jl = 1, ipl
            DO_3D( itl, itr, itb, itt, 1, ipk )
               twrk(kt)%data%data4d(ji,jj,jk,jl) = twrk(kt)%datat(ntile)%data4d(ji,jj,jk,jl)
            END_3D
         END DO
         DEALLOCATE( twrk(kt)%datat(ntile)%data4d )
      ENDIF

      IF( ln_timing ) CALL timing_stop('copyout_twrk')
   END SUBROUTINE copyout_twrk
   !!======================================================================
END MODULE domtile

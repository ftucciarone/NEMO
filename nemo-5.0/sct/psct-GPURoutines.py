#! /usr/bin/python
# ======================================================================
#                     ***  psct-GPURoutines.py  ***
# ======================================================================
#  History : 5.0  !  2024  (S. Mueller)
# ----------------------------------------------------------------------
#
# PSyclone transformation script for the addition of OpenACC 'routine'
# directives to nominated procedures using the ACCRoutineTrans transformation
# of PSyclone
# (https://psyclone.readthedocs.io/en/stable/transformations.html#available-transformations).
#
# ----------------------------------------------------------------------
# NEMO 5.0 , NEMO Consortium (2024)
# Software governed by the CeCILL license (see ./LICENSE)
# ----------------------------------------------------------------------

from psyclone.transformations import ACCRoutineTrans

# ----------------------------------------------------------------------
# List of procedures that are prepared for execution on the GPU
# ----------------------------------------------------------------------
INVOKES_ROUTINES_GPU = [ 'sign_scalar' ]                                 # Procedure accessed via interface 'SIGN'
                                                                         # (lib_fortran.f90)
INVOKES_ROUTINES_GPU += [ 'psyclone_cmp_int', 'psyclone_cmp_logical' ]   # Procedures added by PSyclone when processing 'WHERE'
                                                                         # constructs (string comparison 'psyclone_cmp_char'
                                                                         # excluded)

# ----------------------------------------------------------------------
#              ***  PSyclone transformation procedure  ***
# ----------------------------------------------------------------------
def trans(psy):

    for invoke in psy.invokes.invoke_list:
        # Prepare some procedures for execution on the GPU
        if invoke.name.lower() in INVOKES_ROUTINES_GPU:
            ACCRoutineTrans().apply(invoke.schedule)

    return

#! /usr/bin/python
# ======================================================================
#                        ***  psct-OpenACC.py  ***
# ======================================================================
#  History : 5.0  !  2024  (S. Mueller)
# ----------------------------------------------------------------------
#
# This script is a PSyclone (https://github.com/stfc/PSyclone) transformation
# script that prepares the source code for compilation with an OpenACC-enabled
# Fortran compiler to enable GPGPU acceleration. This script calls a range of
# separate relevant PSyclone transformation scripts: simplification of
# tracer-type conditions (sct/psct-TracerTypeToggles.py), delineation of kernels
# regions (sct/psct-KernelsRegions.py), promotion of local arrays to module
# level (sct/psct-LocalToGlobalArrays.py), and preparation of routines for
# execution on the GPU (sct/psct-GPURoutines.py).
#
# The entry point of the transformation script is procedure 'trans', which is
# called by the 'psyclone' exectuable (via the NEMO build system, if the
# transformation is selected via option '-p OpenACC' of 'makenemo') and acts on
# and returns the provided object 'psy' (for details on the PSyclone
# transformation-script convention see
# https://psyclone.readthedocs.io/en/stable/transformations.html#script).
#
# ----------------------------------------------------------------------
# NEMO 5.0 , NEMO Consortium (2024)
# Software governed by the CeCILL license (see ./LICENSE)
# ----------------------------------------------------------------------

#psct-include(TracerTypeToggles)
#psct-include(KernelsRegions)
#psct-include(LocalToGlobalArrays)
#psct-include(GPURoutines)

# ----------------------------------------------------------------------
#              ***  PSyclone transformation procedure  ***
# ----------------------------------------------------------------------
def trans(psy):

    # Simplify 'TRA' and/or 'TRC' string comparisons
    trans_TracerTypeToggles(psy)
    
    # Delineate large OpenACC kernels regions
    trans_KernelsRegions(psy)

    # Promote local arrays to module scope
    trans_LocalToGlobalArrays(psy)

    # Selectively prepare routines for execution on the GPU
    trans_GPURoutines(psy)

    return

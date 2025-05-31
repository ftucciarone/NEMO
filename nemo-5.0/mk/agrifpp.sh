#!/bin/bash
#set -x
set -o posix
#set -u
#set -e
#+
#
# ==========
# agrifpp.sh
# ==========
#
# ----------------------------
# Preform AGrif pre-processing
# ----------------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ agrifpp.sh
#
#
# DESCRIPTION
# ===========
#
#
# Process file using ./conv in the build directory
# Standard preprocessed files are stored in the ./ppsrc/nemo subdirectory
# Processed source-code and include files are stored in ./obj and ./inc,
# respectively
# Note that agrif2model.F90 should not be preprocessed (standard one)
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./agrifpp.sh FILE_TO_PROCESS
# 
# TODO
# ====
#
# option debug
#
#
# EVOLUTIONS
# ==========
#
# $Id: agrifpp.sh 2143 2010-10-04 12:49:55Z rblod $
#
#
#
#   * creation
#
#-
MYDIR=$1
MYFILE=$(basename "$2")

if [ "$MYFILE" == "agrif2model.f90" ];then
   # generic case
   if [ -d ${MYDIR}/../WORK ]; then
      \cp ${MYDIR}/../WORK/${MYFILE/.f90/.F90} ${MYDIR}/obj/$MYFILE
   # DOMAINcfg case
   elif [ -d ${MYDIR}/../src ]; then
      \cp ${MYDIR}/../src/${MYFILE/.f90/.F90} ${MYDIR}/obj/$MYFILE
   fi
else
   cd ${MYDIR}/ppsrc/nemo ; ${MYDIR}/conv ${MYDIR}/agrif_oce.in -rm -incdir ${MYDIR}/inc -comdirout ${MYDIR}/obj -convfile ${MYFILE} #  > /dev/null 
fi

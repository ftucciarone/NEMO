#!/bin/bash
##########################################################################
# Author : Simona Flavoni for NEMO
# Contact : sflod@locean-ipsl.upmc.fr
#
# ----------------------------------------------------------------------
# NEMO/SETTE , NEMO Consortium (2010)
# Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
# ----------------------------------------------------------------------
#
# Some scripts called by sette.sh
# prepare_exe_dir.sh : script prepares execution directory for test
##########################################################################
#set -x
set -o posix
#set -u
#set -e
#+
#
# ==================
# prepare_exe_dir.sh
# ==================
#
# ----------------------------------------------
# Set of functions used by sette.sh (NEMO tests) 
# ----------------------------------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ ./prepare_exe_dir.sh
#
# DESCRIPTION
# ===========
#
# prepare_exe_dir.sh creates execution directory takes name of TEST_NAME defined in every test in sette.sh
# 
# it is necessary to define in sette.sh TEST_NAME ( example : export TEST_NAME="LONG") to create execution directory in where run test.
#
# NOTE : each test has to run in its own directory ( of execution), if not existing files are re-written (for example namelist)
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./prepare_exe_dir.sh
#
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
# $Id: $
#
#   * creation
#-


# PREPARE EXEC_DIR
#==================
#if [ -z "${CUSTOM_DIR}" ]; then
#  EXE_DIR=${CONFIG_DIR}/${SETTE_CONFIG}
#else
#  CMP_NAM_L=$(echo ${CMP_NAM} | tr '[:upper:]' '[:lower:]')
#  if [[ -n "${NEMO_DEBUG}" || ${CMP_NAM_L} =~ ("debug"|"dbg") ]]; then
#    export EXE_DIR=${CUSTOM_DIR}/${SETTE_SUB_VAL}_${NEMO_REV}_DEBUG/${SETTE_CONFIG}
#  else
#    EXE_DIR=${CUSTOM_DIR}/${SETTE_SUB_VAL}_${NEMO_REV}/${SETTE_CONFIG}
#  fi
#fi
EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}
mkdir -p ${EXE_DIR}/${TEST_NAME}

cp -a ${EXE_DIR}/EXP00/* ${EXE_DIR}/${TEST_NAME}/.
COMP_KEYS="`cat ${EXE_DIR}/cpp_${SETTE_CONFIG}.fcm | sed -e 's/.*fppkeys *//'`"

export EXE_DIR=${EXE_DIR}/${TEST_NAME}
cd ${EXE_DIR}
#
# Add summary of the sette.sh set-up used and the current list of keys added or deleted
echo "Summary of sette environment"                                > ./sette_config
echo "----------------------------"                               >> ./sette_config
echo "requested by the command          : "$cmd $cmdargs          >> ./sette_config
echo "on branch                         : "$SETTE_THIS_BRANCH     >> ./sette_config
printf "%-33s : %s\n" USING_TIMING $USING_TIMING                  >> ./sette_config
printf "%-33s : %s\n" USING_ICEBERGS $USING_ICEBERGS              >> ./sette_config
printf "%-33s : %s\n" USING_ABL $USING_ABL                        >> ./sette_config
printf "%-33s : %s\n" USING_EXTRA_HALO $USING_EXTRA_HALO          >> ./sette_config
printf "%-33s : %s\n" USING_TILING $USING_TILING                  >> ./sette_config
printf "%-33s : %s\n" USING_COLLECTIVES $USING_COLLECTIVES        >> ./sette_config
printf "%-33s : %s\n" USING_QCO $USING_QCO                        >> ./sette_config
printf "%-33s : %s\n" USING_XIOS $USING_XIOS                      >> ./sette_config
printf "%-33s : %s\n" USING_MPMD $USING_MPMD                      >> ./sette_config
printf "%-33s : %s\n" USING_RK3 $USING_RK3                        >> ./sette_config
printf "%-33s : %s\n" USER_INPUT $USER_INPUT                      >> ./sette_config
printf "%-33s : %s\n" "Common compile keys added" "$ADD_KEYS"     >> ./sette_config
printf "%-33s : %s\n" "Common compile keys deleted" "$DEL_KEYS"   >> ./sette_config
printf "%-33s : %s\n" "Compile keys actually used" "${COMP_KEYS}" >> ./sette_config

# Remove previously generated output files used for test evaluation
# (if any)
[ -f ./ocean.output ] && mv ./ocean.output ./ocean.output.old
[ -f ./run.stat ]     && mv ./run.stat     ./run.stat.old
[ -f ./tracer.stat ]  && mv ./tracer.stat  ./tracer.stat.old
[ -f ./obs.stat ]     && mv ./obs.stat     ./obs.stat.old

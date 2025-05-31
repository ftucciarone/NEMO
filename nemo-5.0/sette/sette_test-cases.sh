#!/bin/bash
############################################################
# Author : Simona Flavoni for NEMO
# Contact: sflod@locean-ipsl.upmc.fr
#
# sette_test-cases.sh   : principal script of SET TEsts for NEMO (SETTE)
#                       : this script : compiles, run and tests TEST_CASES
#
#                       : TO DO: test if nitend is equal to end of run.stat
# ----------------------------------------------------------------------
# NEMO/SETTE , NEMO Consortium (2018)
# Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
# ----------------------------------------------------------------------
#
#############################################################
#set -vx
set -o posix
#set -u
#set -e
# ===========
# DESCRIPTION
# ===========
#
# Variables to be checked by user:
#
# COMPILER          : name of compiler as defined in NEMOGCM/ARCH directory 
# BATCH_COMMAND_PAR :  name of the command for submitting parallel batch jobs
# BATCH_COMMAND_SEQ :  name of the command for submitting sequential batch jobs  
# INTERACT_FLAG     : flag to run in interactive mode "yes"
#                           to run in batch mode "no"
# MPIRUN_FLAG       : flag to run in parallel (MPI) "yes"
#                           to run in sequential mode (NB_PROC = 1) "no"
# NUM_XIOSERVERS    : number of stand-alone IO servers to employ
#                     set to zero if USING_MPMD="no"
#
# Principal script is sette_test-cases.sh, that calls 
#
#  makenemo  : to create successive exectuables in ${CONFIG_NAME}/BLD/bin/nemo.exe 
#              and links to nemo in ${CONFIG_NAME}/EXP00)
#
#  param.cfg : sets and loads following directories:
#              This will have been run by the parent sette.sh and values exported here
#
#   FORCING_DIR         : is the directory for forcing files (tarfile)
#   INPUT_DIR           : is the directory for input files storing 
#   TMPDIR              : is the temporary directory (if needed)
#   NEMO_VALIDATION_DIR : is the validation directory
#
#   (NOTE: this file is the same for all configrations to be tested with sette_test-cases.sh)
#
#   all_functions.sh : loads functions used by sette (note: new functions can be added here)
#   set_namelist     : function declared in all_functions that sets namelist parameters 
#   post_test_tidyup : creates validation storage directory and copies required output files 
#                      (run.stat and ocean.output) in it after execution of test.
#
#  VALIDATION tree is:
#
#   NEMO_VALIDATION_DIR/WCONFIG_NAME/WCOMPILER_NAME/TEST_NAME/REVISION_NUMBER(or DATE)
#
#  prepare_exe_dir.sh : defines and creates directory where the test is executed
#                       execution directory takes name of TEST_NAME defined for every test 
#                       in sette_test-cases.sh. (each test in executed in its own directory)
#
#  set_valid_dir       : rename ocean.output/run.stat and tracer.stat to avoid checking them in the report 
#
#  clean_valid_dir    : rename ocean.output/run.stat and tracer.stat to avoid checking them in the report 
#                       ( not doing it could lead to false positive )
#
#  prepare_job.sh     : to generate the script run_job.sh
#
#  fcm_job.sh         : run in batch (INTERACT_FLAG="no") or interactive (INTERACT_FLAG="yes")
#                        see sette_test-cases.sh and BATCH_TEMPLATE directory
#
#  NOTE: jobs requiring initial or forcing data need to have an input_CONFIG.cfg in which 
#        can be found paths to the input tar file)
#  NOTE: if job is not launched for any reason you have the executable ready in ${EXE_DIR} 
#        directory
#  NOTE: the changed namelists are left in ${EXE_DIR} directory whereas original namelists 
#        remain in ${NEW_CONF}/EXP00
# 
#  NOTE: a log file, output.sette, is created in ${SETTE_DIR} with the echoes of 
#        executed commands
#
#  NOTE: if sette_test-cases.sh is stopped in output.sette there is written the last command 
#        executed by sette_test-cases.sh
#
# example use: ./sette_test-cases.sh 
#########################################################################################
#
# LOAD param value
SETTE_DIR=$(cd $(dirname "$0"); pwd)
MAIN_DIR=$(dirname $SETTE_DIR)

export BATCH_COMMAND_PAR=${BATCH_CMD}
export BATCH_COMMAND_SEQ=${BATCH_CMD}
export INTERACT_FLAG="no"
export MPIRUN_FLAG="yes"
#
# Settings which control the use of stand alone servers (only relevant if using xios)
#
export NUM_XIOSERVERS=4
export JOB_PREFIX=${JOB_PREFIX_MPMD}
#
if [ ${USING_MPMD} == "no" ] 
 then
   export NUM_XIOSERVERS=0
   export JOB_PREFIX=${JOB_PREFIX_NOMPMD}
fi
#
# Directory to run the tests
CONFIG_DIR0=${MAIN_DIR}/tests
TOOLS_DIR=${MAIN_DIR}/tools

if [ -n "${CUSTOM_DIR}" ]; then
  CMP_NAM_L=$(echo ${CMP_NAM} | tr '[:upper:]' '[:lower:]')
  if [[ -n "${NEMO_DEBUG}" || ${CMP_NAM_L} =~ ("debug"|"dbg") ]]; then
    export CMP_DIR=${CUSTOM_DIR}/${SETTE_SUB_VAL}_${NEMO_REV}_DEBUG
  else
    export CMP_DIR=${CUSTOM_DIR}/${SETTE_SUB_VAL}_${NEMO_REV}
  fi
fi
CMP_NAM=${1:-$COMPILER}
CMP_NAM_L=$(echo ${CMP_NAM} | tr '[:upper:]' '[:lower:]')
# Copy job_batch_COMPILER file for specific compiler into job_batch_template
cd ${SETTE_DIR}
cp BATCH_TEMPLATE/${JOB_PREFIX}-${COMPILER} job_batch_template || exit 1
# Description of configuration tested:
# OVERFLOW       : TEST s-coordinates : (tracers) Advection schemes: FCT2, FCT4, ubs 
#                                     & (dynamics) advection schemes: flux form (ubs, centered), vector form (een)
#                       zps-coordinates : (tracers) Advection schemes: FCT2, FCT4, ubs
#                                     & (dynamics) advection schemes: flux form (ubs, centered), vector form (een, and een + Hollingsworth correction)
# LOCK_EXCHANGE  : 
# VORTEX         : 
# ICE_AGRIF      : 
# ISOMIP+         : 
# WAD

. ./all_functions.sh
for config in ${TEST_CONFIGS[@]}
do
    for (( l_t=-1 ; l_t < ${#SCTRANSFORMS[@]} ; l_t++ )); do

        CONFIG_SUFFIX=${SETTE_STG}
        DO_RESTART_1=${DO_RESTART}
        DO_RESTART_2=${DO_RESTART}
        DO_REPRO_1=${DO_REPRO}
        DO_REPRO_2=${DO_REPRO}
        DO_PHYOPTS_0=${DO_PHYOPTS}
        TRANSFORM_OPT=""
        if [[ ${DO_TRANSFORM} == "1" ]] ; then
            # Ensure reference run for TRANSFORM test
            DO_RESTART_1="1"
            if [ ${l_t} -ge 0 ]; then
                TRANSFORM_OPT="-p passthrough"
                CONFIG_SUFFIX="+PT"${SETTE_STG}
                if [ ! "${SCTRANSFORMS[${l_t}]}" == "passthrough" ]; then
                    TRANSFORM_OPT="-p ${SCTRANSFORMS[${l_t}]}"
                    CONFIG_SUFFIX=`printf "+T%02i" ${l_t}`${SETTE_STG}
                fi
                # Only perform runs required for TRANSFORM test
                DO_RESTART_2="0"
                DO_REPRO_1="0"
                DO_REPRO_2="0"
                DO_PHYOPTS_0="0"
            fi
        fi

# ---------
#  OVERFLOW
# ---------
if [ ${config} == "OVERFLOW" ];  then
    SETTE_CONFIG="OVERFLOW"${CONFIG_SUFFIX}
    if [[ -n "${NEMO_DEBUG}" || ${CMP_NAM_L} =~ ("debug"|"dbg") ]]
    then
        ITEND=12
    else
        ITEND=120
    fi

    if [ ${DO_COMPILE} == "1" ] ;  then
        cd ${MAIN_DIR}
        #
        # syncronisation if target directory/file exist (not done by makenemo)
        clean_config ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}
        sync_config  ${CONFIG_DIR0}/${config} ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}
        #
        ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -a OVERFLOW ${CUSTOM_DIR:+-t ${CMP_DIR}} -k 0 ${NEMO_DEBUG} \
                   -j ${CMPL_CORES} ${TRANSFORM_OPT} add_key "${ADD_KEYS}" del_key "${DEL_KEYS}" || exit 1
    fi

    # Configure and submit test runs for the OVERFLOW SETTE configuration
    if [ ${DO_RESTART} == "1" -o ${DO_PHYOPTS} == "1" -o ${DO_TRANSFORM} == "1" ] ; then

        # Default test-run configuration for the OVERFLOW SETTE configuration
        EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
        cd ${EXE_DIR}
        set_namelist namelist_cfg cn_exp \"OVF\"
        set_namelist namelist_cfg nn_it000 1
        set_namelist namelist_cfg nn_itend ${ITEND}
	if [ ${USING_RK3} == "no" ] ;  then
	    set_namelist namelist_cfg rn_Dt 10.
	    set_namelist namelist_cfg nn_bt_flt 1
	    set_namelist namelist_cfg rn_bt_alpha 0.
	fi
        set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_xio_using_server iodef.xml ${USING_MPMD}
        NPROC=1

        ## Restartability tests for OVERFLOW
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            export TEST_NAME="LONG"
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
        fi
        if [ ${DO_RESTART_1} == "1" ] ;  then
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}  
            set_namelist namelist_cfg cn_exp \"OVF_LONG\"
            set_namelist_rst namelist ${ITEND}
            set_namelist namelist_cfg sn_cfctl%l_runstat .true.
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            export TEST_NAME="SHORT"
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"OVF_SHORT\"
            set_namelist_rst namelist ${ITEND} "OVF_LONG" "OCE"
            set_namelist namelist_cfg sn_cfctl%l_runstat .true.
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        fi

        ## Test for all advection, vert. coordinates, vector form, flux form: test runability and complete all time steps
        ## Needed namelist-xxxx for every type of run tested
        if [ ${DO_PHYOPTS_0} == "1" ] ;  then
            if [[ -n "${NEMO_DEBUG}" || ${CMP_NAM_L} =~ ("debug"|"dbg") ]]
            then
                ITEND=12
            else
                ITEND=6120
            fi
            cd ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
            for file in $(echo `ls namelist_*_cfg `) ; do
                TEST_NAME=`echo $file | sed -e "s/namelist_//" | sed -e "s/_cfg//"`
                TEST_NAME="EXP-${TEST_NAME}"
                if [ ! -d ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/${TEST_NAME} ] ; then mkdir -p ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/${TEST_NAME} ; fi
                export TEST_NAME="${TEST_NAME}"
                cd ${SETTE_DIR}
                . ./prepare_exe_dir.sh
                set_valid_dir
                clean_valid_dir
                JOB_FILE=${EXE_DIR}/run_job.sh
                if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
                cd ${EXE_DIR}
                rm namelist_*_*_*_* namelist_cfg
                cp -pL ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00/$file namelist_cfg
                set_namelist namelist_cfg nn_itend ${ITEND}
                cd ${SETTE_DIR}
                . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
                cd ${SETTE_DIR}
                . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
             done
        fi

    fi

fi

# --------------
#  LOCK_EXCHANGE
# --------------
if [ ${config} == "LOCK_EXCHANGE" ] ;  then
    SETTE_CONFIG="LOCK_EXCHANGE"${CONFIG_SUFFIX}
    if [[ -n "${NEMO_DEBUG}" || ${CMP_NAM_L} =~ ("debug"|"dbg") ]]
    then
        ITEND=12
    else
        ITEND=120
    fi

    if [ ${DO_COMPILE} == "1" ] ;  then
        cd ${MAIN_DIR}
        #
        # syncronisation if target directory/file exist (not done by makenemo)
        clean_config ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}
        sync_config  ${CONFIG_DIR0}/${config} ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}
        #
        ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -a LOCK_EXCHANGE ${CUSTOM_DIR:+-t ${CMP_DIR}} -k 0 ${NEMO_DEBUG} \
                   -j ${CMPL_CORES} ${TRANSFORM_OPT} add_key "${ADD_KEYS}" del_key "${DEL_KEYS}" || exit 1
    fi

    # Configure and submit test runs for the LOCK_EXCHANGE SETTE configuration
    # (if any)
    if [ ${DO_RESTART} == "1" -o ${DO_PHYOPTS} == "1" -o ${DO_TRANSFORM} == "1" ] ; then

        # Default test-run configuration for the LOCK_EXCHANGE SETTE
        # configuration
        EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
        cd ${EXE_DIR}
        set_namelist namelist_cfg cn_exp \"LOCK\"
        set_namelist namelist_cfg nn_it000 1
        set_namelist namelist_cfg nn_itend ${ITEND}
	if [ ${USING_RK3} == "no" ] ;  then
            set_namelist namelist_cfg rn_Dt 1.
            set_namelist namelist_cfg nn_bt_flt 1
            set_namelist namelist_cfg rn_bt_alpha 0.
        fi
        set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_xio_using_server iodef.xml ${USING_MPMD}
        NPROC=1

        ## Restartability tests for LOCK_EXCHANGE
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            export TEST_NAME="LONG"
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
        fi
        if [ ${DO_RESTART_1} == "1" ] ;  then
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"LOCK_LONG\"
            set_namelist_rst namelist ${ITEND}
            set_namelist namelist_cfg sn_cfctl%l_runstat .true.
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            export TEST_NAME="SHORT"
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"LOCK_SHORT\"
            set_namelist_rst namelist ${ITEND} "LOCK_LONG" "OCE"
            set_namelist namelist_cfg sn_cfctl%l_runstat .true.
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        fi

        ## Test for all advection, vector form, flux form: test runability and complete all time steps
        ## Needed namelist-xxxx for every type of run tested
        if [ ${DO_PHYOPTS_0} == "1" ] ;  then
            if [[ -n "${NEMO_DEBUG}" || ${CMP_NAM_L} =~ ("debug"|"dbg") ]]
            then
                ITEND=12
            else
                ITEND=61200
            fi
            cd ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
            for file in $(echo `ls namelist_*_cfg `) ; do
                echo ''
                TEST_NAME=`echo $file | sed -e "s/namelist_//" | sed -e "s/_cfg//"`
                TEST_NAME="EXP-${TEST_NAME}"
                mkdir -p ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/${TEST_NAME}
                export TEST_NAME="${TEST_NAME}"
                cd ${SETTE_DIR}
                . ./prepare_exe_dir.sh
                set_valid_dir
                clean_valid_dir
                JOB_FILE=${EXE_DIR}/run_job.sh
                if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
                cd ${EXE_DIR}
                rm namelist_*_*_*_* namelist_cfg
                cp -pL ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00/$file namelist_cfg
                set_namelist namelist_cfg nn_itend ${ITEND}
                cd ${SETTE_DIR}
                . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
                cd ${SETTE_DIR}
                . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
                echo ''
            done
        fi

    fi

fi

# ------
# IWAVE 
# ------
if [ ${config} == "IWAVE" ] ;  then
    SETTE_CONFIG="IWAVE"${CONFIG_SUFFIX}
    if [[ -n "${NEMO_DEBUG}" || ${CMP_NAM_L} =~ ("debug"|"dbg") ]]
    then
        ITEND=12
    else
        ITEND=120
    fi

    if [ ${DO_COMPILE} == "1" ] ;  then
        cd ${MAIN_DIR}
        #
        # syncronisation if target directory/file exist (not done by makenemo)
        clean_config ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}
        sync_config  ${CONFIG_DIR0}/${config} ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}
        #
        ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -a IWAVE ${CUSTOM_DIR:+-t ${CMP_DIR}} -k 0 ${NEMO_DEBUG} \
                   -j ${CMPL_CORES} ${TRANSFORM_OPT} add_key "${ADD_KEYS}" del_key "${DEL_KEYS}" || exit 1
    fi

    # Configure and submit test runs for the IWAVE SETTE configuration
    # (if any)
    if [ ${DO_RESTART} == "1" -o ${DO_PHYOPTS} == "1" -o ${DO_TRANSFORM} == "1" ] ; then

        # Default test-run configuration for the IWAVE SETTE
        # configuration
        EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
        cd ${EXE_DIR}
        set_namelist namelist_cfg cn_exp \"IWAVE\"
        set_namelist namelist_cfg nn_it000 1
        set_namelist namelist_cfg nn_itend ${ITEND}
	if [ ${USING_RK3} == "no" ] ;  then
            set_namelist namelist_cfg rn_Dt 1.
            set_namelist namelist_cfg nn_bt_flt 1
            set_namelist namelist_cfg rn_bt_alpha 0.
        fi
        set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_xio_using_server iodef.xml ${USING_MPMD}
        NPROC=1

        ## Restartability tests for IWAVE 
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            export TEST_NAME="LONG"
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
        fi
        if [ ${DO_RESTART_1} == "1" ] ;  then
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"IWAVE_LONG\"
            set_namelist_rst namelist ${ITEND}
            set_namelist namelist_cfg sn_cfctl%l_runstat .true.
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            export TEST_NAME="SHORT"
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"IWAVE_SHORT\"
            set_namelist_rst namelist ${ITEND} "IWAVE_LONG" "OCE"
            set_namelist namelist_cfg sn_cfctl%l_runstat .true.
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        fi

    fi

fi

# ------
# VORTEX
# ------
if [ ${config} == "VORTEX" ] ;  then
    SETTE_CONFIG="VORTEX"${CONFIG_SUFFIX}
    if [[ -n "${NEMO_DEBUG}" || ${CMP_NAM_L} =~ ("debug"|"dbg") ]]
    then
        ITEND=12
    else
        ITEND=240
    fi

    if [ ${DO_COMPILE} == "1" ] ;  then
        cd ${MAIN_DIR}
        #
        # syncronisation if target directory/file exist (not done by makenemo)
        clean_config ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}
        sync_config  ${CONFIG_DIR0}/${config} ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}
        #
        ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -a VORTEX ${CUSTOM_DIR:+-t ${CMP_DIR}} -k 0 ${NEMO_DEBUG} \
                   -j ${CMPL_CORES}  ${TRANSFORM_OPT} add_key "${ADD_KEYS}" del_key "${DEL_KEYS}" || exit 1
    fi

    # Configure and submit test runs for the VORTEX SETTE configuration (if
    # any)
    if [ ${DO_RESTART} == "1" -o ${DO_REPRO} == "1" -o ${DO_TRANSFORM} == "1" ] ; then

        # Default test-run configuration for the VORTEX SETTE configuration
        EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
        cd ${EXE_DIR}
        set_namelist namelist_cfg cn_exp \"VORTEX\"
        set_namelist namelist_cfg nn_it000 1
        set_namelist namelist_cfg nn_itend ${ITEND}
        set_namelist namelist_cfg nn_stock ${ITEND}
	if [ ${USING_RK3} == 'no' ] ; then
            set_namelist namelist_cfg rn_Dt 1440.
            set_namelist namelist_cfg nn_e 24
        fi
        set_namelist namelist_cfg sn_cfctl%l_runstat .true.
        set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_namelist 1_namelist_cfg cn_exp \"VORTEX\"
        set_namelist 1_namelist_cfg nn_it000 1
        set_namelist 1_namelist_cfg nn_itend $(( ${ITEND} * 3 ))
        set_namelist 1_namelist_cfg nn_stock $(( ${ITEND} * 3 ))
	if [ ${USING_RK3} == 'no' ] ; then
            set_namelist 1_namelist_cfg rn_Dt 480.
            set_namelist 1_namelist_cfg nn_e 24
        fi
        set_namelist 1_namelist_cfg sn_cfctl%l_runstat .true.
        set_namelist_opt 1_namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist_opt 1_namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt 1_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt 1_namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_xio_using_server iodef.xml ${USING_MPMD}
        NPROC=6

        ## Restartability tests for VORTEX
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            export TEST_NAME="LONG"
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi 
        fi
        if [ ${DO_RESTART_1} == "1" ] ;  then
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"VORTEX_LONG\"
            set_namelist_rst namelist ${ITEND}
            set_namelist 1_namelist_cfg cn_exp \"VORTEX_LONG\"
            set_namelist_rst 1_namelist $(( ${ITEND} * 3 ))
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            export TEST_NAME="SHORT"
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"VORTEX_SHORT\"
            set_namelist_rst namelist ${ITEND} "VORTEX_LONG" "OCE"
            set_namelist 1_namelist_cfg cn_exp \"VORTEX_SHORT\"
            set_namelist_rst 1_namelist $(( ${ITEND} * 3 )) "VORTEX_LONG" "OCE"
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        fi

        ## Reproducibility tests for VORTEX
        names=""
        [[ ${DO_REPRO_1} == "1" ]] && names="${names} REPRO_2_3"
        [[ ${DO_REPRO_2} == "1" ]] && names="${names} REPRO_3_2"
        for name in ${names}; do
            export TEST_NAME=${name}
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
            cd ${EXE_DIR}
            for nname in namelist_cfg 1_namelist_cfg; do
                if [[ ${name} == "REPRO_2_3" ]]; then
                    set_namelist ${nname} cn_exp \"VORTEX_23\"
                    set_namelist ${nname} jpni 2
                    set_namelist ${nname} jpnj 3
                else
                    set_namelist ${nname} cn_exp \"VORTEX_32\"
                    set_namelist ${nname} jpni 3
                    set_namelist ${nname} jpnj 2
                fi
            done
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        done

    fi

fi

# ---------
# ICE_AGRIF
# ---------
if [ ${config} == "ICE_AGRIF" ] ;  then
    SETTE_CONFIG="ICE_AGRIF"${CONFIG_SUFFIX}
    if [[ -n "${NEMO_DEBUG}" || ${CMP_NAM_L} =~ ("debug"|"dbg") ]]
    then
        ITEND=10
    else
        ITEND=200
    fi

    if [ ${DO_COMPILE} == "1" ] ;  then
        cd ${MAIN_DIR}
        #
        # syncronisation if target directory/file exist (not done by makenemo)
        clean_config ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}
        sync_config  ${CONFIG_DIR0}/${config} ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}
        #
        # ICE_AGRIF uses linssh so remove key_qco if added by default
        ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -a ICE_AGRIF ${CUSTOM_DIR:+-t ${CMP_DIR}} -k 0 ${NEMO_DEBUG} \
                   -j ${CMPL_CORES} ${TRANSFORM_OPT} add_key "${ADD_KEYS/key_qco/}" del_key "${DEL_KEYS}" || exit 1
    fi

    # Configure and submit test runs for the ICE_AGRIF SETTE configuration (if
    # any)
    if [ ${DO_RESTART} == "1" -o ${DO_REPRO} == "1" -o ${DO_TRANSFORM} == "1" ] ; then

        # Default test-run configuration for the ICE_AGRIF SETTE configuration
        EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
        cd ${EXE_DIR}
        set_namelist namelist_cfg cn_exp \"ICE_AGRIF\"
        set_namelist namelist_cfg nn_it000 1
        set_namelist namelist_cfg nn_itend ${ITEND}
        set_namelist namelist_cfg nn_stock $(( ${ITEND} / 2 ))
	if [ ${USING_RK3} == 'no' ] ; then
            set_namelist namelist_cfg rn_Dt 1200.
        fi
        set_namelist namelist_cfg sn_cfctl%l_runstat .true.
        set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_namelist 1_namelist_cfg cn_exp \"ICE_AGRIF\"
        set_namelist 1_namelist_cfg nn_it000 1
        set_namelist 1_namelist_cfg nn_itend $(( ${ITEND} * 3 ))
        set_namelist 1_namelist_cfg nn_stock $(( ${ITEND} * 3 / 2 ))
	if [ ${USING_RK3} == 'no' ] ; then
            set_namelist 1_namelist_cfg rn_Dt 400.
        fi
        set_namelist 1_namelist_cfg sn_cfctl%l_runstat .true.
        set_namelist_opt 1_namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist_opt 1_namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt 1_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt 1_namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_xio_using_server iodef.xml ${USING_MPMD}
        NPROC=6

        ## Restartability tests for ICE_AGRIF
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            export TEST_NAME="LONG"
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi 
        fi
        if [ ${DO_RESTART_1} == "1" ] ;  then
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"ICE_AGRIF_LONG\"
            set_namelist_rst namelist ${ITEND}
            set_namelist 1_namelist_cfg cn_exp \"ICE_AGRIF_LONG\"
            set_namelist_rst 1_namelist $(( ${ITEND} * 3 ))
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_ICE_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            export TEST_NAME="SHORT"
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"ICE_AGRIF_SHORT\"
            set_namelist_rst namelist ${ITEND} "ICE_AGRIF_LONG" "OCE ICE"
            set_namelist 1_namelist_cfg cn_exp \"ICE_AGRIF_SHORT\"
            set_namelist_rst 1_namelist $(( ${ITEND} * 3 )) "ICE_AGRIF_LONG" "OCE ICE"
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_ICE_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        fi

        ## Reproducibility tests for ICE_AGRIF
        names=""
        [[ ${DO_REPRO_1} == "1" ]] && names="${names} REPRO_2_3"
        [[ ${DO_REPRO_2} == "1" ]] && names="${names} REPRO_3_2"
        for name in ${names}; do
            export TEST_NAME=${name}
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
            cd ${EXE_DIR}
            for nname in namelist_cfg 1_namelist_cfg; do
                if [[ ${name} == "REPRO_2_3" ]]; then
                    set_namelist ${nname} cn_exp \"ICE_AGRIF_23\"
                    set_namelist ${nname} jpni 2
                    set_namelist ${nname} jpnj 3
                else
                    set_namelist ${nname} cn_exp \"ICE_AGRIF_32\"
                    set_namelist ${nname} jpni 3
                    set_namelist ${nname} jpnj 2
                fi
            done
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_ICE_AGRIF.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        done

    fi

fi

# -------
# ISOMIP+
# -------
if [ ${config} == "ISOMIP+" ]; then 
    SETTE_CONFIG="ISOMIP+"${CONFIG_SUFFIX}
    if [[ -n "${NEMO_DEBUG}" || ${CMP_NAM_L} =~ ("debug"|"dbg") ]]
    then
        ITEND=12
    else
        ITEND=1200
    fi

    if [ ${DO_COMPILE} == "1" ] ;  then
        cd ${MAIN_DIR}
        #
        # syncronisation if target directory/file exist (not done by makenemo)
        clean_config ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}
        sync_config  ${CONFIG_DIR0}/${config} ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}
        #
        # ISOMIP+ uses ln_hpg_isf so remove key_qco if added by default
        ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -a ISOMIP+ ${CUSTOM_DIR:+-t ${CMP_DIR}} -k 0 ${NEMO_DEBUG} \
                   -j ${CMPL_CORES} ${TRANSFORM_OPT} add_key "${ADD_KEYS/key_qco/}" del_key "${DEL_KEYS}" || exit 1
    fi

    # Configure and submit test runs for the ISOMIP+ SETTE configuration (if
    # any)
    if [ ${DO_RESTART} == "1" -o ${DO_REPRO} == "1" -o ${DO_TRANSFORM} == "1" ] ; then

        # Default test-run configuration for the ISOMIP+ SETTE configuration
        EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
        cd ${EXE_DIR}
        set_namelist namelist_cfg cn_exp \"ISOMIP+\"
        set_namelist namelist_cfg nn_it000 1
        set_namelist namelist_cfg nn_itend ${ITEND}
        set_namelist namelist_cfg jpni 9
        set_namelist namelist_cfg jpnj 3
	if [ ${USING_RK3} == 'no' ] ; then
            set_namelist namelist_cfg rn_Dt 720.
	    set_namelist namelist_cfg nn_bt_flt 1
	    set_namelist namelist_cfg rn_bt_alpha 0.
        fi
        set_namelist namelist_cfg sn_cfctl%l_runstat .true.
        set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_xio_using_server iodef.xml ${USING_MPMD}
        NPROC=27

        ## Restartability tests
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            export TEST_NAME="LONG"
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
        fi
        if [ ${DO_RESTART_1} == "1" ] ;  then
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"ISOMIP+_LONG\"
            set_namelist_rst namelist ${ITEND}
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_ISOMIP+.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            export TEST_NAME="SHORT"
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"ISOMIP+_SHORT\"
            set_namelist_rst namelist ${ITEND} "ISOMIP+_LONG" "OCE"
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_ISOMIP+.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        fi

        ## Reproducibility tests
        names=""
        [[ ${DO_REPRO_1} == "1" ]] && names="${names} REPRO_9_3"
        [[ ${DO_REPRO_2} == "1" ]] && names="${names} REPRO_8_4"
        for name in ${names}; do
            export TEST_NAME=${name}
            if [[ ${name} == "REPRO_9_3" ]]; then
                if [[ -n "${NEMO_DEBUG}" || ${CMP_NAM_L} =~ ("debug"|"dbg") ]]
                then
                    ITEND=12
                else
                    ITEND=600
                fi
            fi
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            [[ ${name} == "REPRO_9_3" ]] && NPROC=27
            [[ ${name} == "REPRO_8_4" ]] && NPROC=32
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
            cd ${EXE_DIR}
            if [[ ${name} == "REPRO_9_3" ]]; then
                set_namelist namelist_cfg cn_exp \"ISOMIP+_93\"
            else
                set_namelist namelist_cfg cn_exp \"ISOMIP+_84\"
                set_namelist namelist_cfg jpni 8
                set_namelist namelist_cfg jpnj 4
            fi
            set_namelist namelist_cfg nn_itend ${ITEND}
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_ISOMIP+.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        done

    fi

fi

# ---
# SWG
# ---
if [ ${config} == "SWG" ] && [ ${USING_QCO} == "yes" ] ;  then
    SETTE_CONFIG="SWG"${CONFIG_SUFFIX}
    if [[ -n "${NEMO_DEBUG}" || ${CMP_NAM_L} =~ ("debug"|"dbg") ]]
    then
        ITEND=12
    else
        ITEND=1728
    fi

    if [ ${DO_COMPILE} == "1" ] ;  then
        cd ${MAIN_DIR}
        #
        # syncronisation if target directory/file exist (not done by makenemo)
        clean_config ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}
        sync_config  ${CONFIG_DIR0}/${config} ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}
        #
        ./makenemo -m ${CMP_NAM} -n ${SETTE_CONFIG} -a SWG ${CUSTOM_DIR:+-t ${CMP_DIR}} -k 0 ${NEMO_DEBUG} \
                   -j ${CMPL_CORES} ${TRANSFORM_OPT} add_key "${ADD_KEYS}" del_key "${DEL_KEYS}" || exit 1
    fi

    # Configure and submit test runs for the SWG SETTE configuration (if any)
    if [ ${DO_RESTART} == "1" -o ${DO_REPRO} == "1" -o ${DO_TRANSFORM} == "1" ] ; then

        # Default test-run configuration for the SWG SETTE configuration
        EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
        cd ${EXE_DIR}
        set_namelist namelist_cfg cn_exp \"SWG\"
        set_namelist namelist_cfg nn_it000 1
        set_namelist namelist_cfg nn_itend ${ITEND}
        set_namelist namelist_cfg nn_stock ${ITEND}
	if [ ${USING_RK3} == 'no' ] ; then
            set_namelist namelist_cfg rn_Dt 1800.
        fi
        set_namelist namelist_cfg sn_cfctl%l_runstat .true.
        set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_xio_using_server iodef.xml ${USING_MPMD}
        NPROC=1

        ## Restartability tests for SWG
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            export TEST_NAME="LONG"
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi 
        fi
        if [ ${DO_RESTART_1} == "1" ] ;  then
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"SWG_LONG\"
            set_namelist_rst namelist ${ITEND}
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            export TEST_NAME="SHORT"
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"SWG_SHORT\"
            set_namelist_rst namelist ${ITEND} "SWG_LONG" "OCE"
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        fi

        ## Reproducibility tests for SWG
        names=""
        [[ ${DO_REPRO_1} == "1" ]] && names="${names} REPRO_2_3"
        [[ ${DO_REPRO_2} == "1" ]] && names="${names} REPRO_3_2"
        for name in ${names}; do
            export TEST_NAME=${name}
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            JOB_FILE=${EXE_DIR}/run_job.sh
            NPROC=6
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
            cd ${EXE_DIR}
            if [[ ${name} == "REPRO_2_3" ]]; then
                set_namelist namelist_cfg cn_exp \"SWG_23\"
                set_namelist namelist_cfg jpni 2
                set_namelist namelist_cfg jpnj 3
            else
                set_namelist namelist_cfg cn_exp \"SWG_32\"
                set_namelist namelist_cfg jpni 3
                set_namelist namelist_cfg jpnj 2
            fi
            set_namelist namelist_cfg sn_cfctl%l_prtctl .true.
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_EMPTY.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        done

    fi

fi

#----
    done
done
#
# Return to SETTE_DIR (last fcm_job.sh will have moved to EXE_DIR)
cd ${SETTE_DIR}

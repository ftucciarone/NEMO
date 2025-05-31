#!/bin/bash -f
# simple SETTE report generator.
#
# This version should be run in the SETTE directory. 
# The machine name will be picked up from the sette.sh script but the location of the
# validation directory needs to be set here (currently assumed to reside in the ../cfgs directory)
#
#########################################################################################
######################### Start of function definitions #################################
#set -x

# report format
format_field1="%-35s"

# exit codes
declare -i {REPRO_EC,RESTA_EC,TRANSFORM_EC,REFCMP_EC,CPUCMP_EC,OCEOUT_EC,AGRIF_EC,PHYOPT_EC}=0

function get_dorv() {
  if [ $lastchange == 'old' ] ; then 
    dorv=`ls -1rt $vdir/$mach/ | tail -1l `
    dorv=`echo $dorv | sed -e 's:.*/::'`
    dorv2=`ls -1rt $vdir/$mach/ 2>/dev/null | tail -1l `
    dorv2=`echo $dorv2 | sed -e 's:.*/::'`
  else
    dorv=$lastchange
    dorv2=$lastchange
  fi
}

function get_ktdiff() {
  ktdiff=`diff ${1} ${2} | head -2 | grep it | awk '{ print $4 }'`
}

function get_ktdiff2() {
  ktdiff=`diff ${1} ${2} |  head -2 | tail -1l | awk '{print $2}'`
}

function resttest() { 
#
# Restartability checks. Expects LONG and SHORT run directories
# Compares end of LONG stat files with equivalent entries from the SHORT stat files.
#
  vdir=$1
  nam=$2
  pass=$3
#
# get $dorv
  get_dorv
#
# check if directory is here
  if [ ! -d $vdir/$mach/$dorv/$nam ]; then
    printf "${format_field1} %s %s\n" $nam  " directory                    MISSING :" $dorv
    echo " please check $vdir/$mach/$dorv/$nam"
    RESTA_EC=1
    return
  fi

  if [ -d $vdir/$mach/$dorv/$nam ]; then
    # check ocean output
    runtest $vdir $nam $pass RST
    #
    # run restartibility test
    f1o=$vdir/$mach/$dorv/$nam/LONG/ocean.output
    f1s=$vdir/$mach/$dorv/$nam/LONG/run.stat
    f1t=$vdir/$mach/$dorv/$nam/LONG/tracer.stat
    f1h=$vdir/$mach/$dorv/$nam/LONG/obs.stat
    f2o=$vdir/$mach/$dorv/$nam/SHORT/ocean.output
    f2s=$vdir/$mach/$dorv/$nam/SHORT/run.stat
    f2t=$vdir/$mach/$dorv/$nam/SHORT/tracer.stat
    f2h=$vdir/$mach/$dorv/$nam/SHORT/obs.stat

    if  [ ! -f $f1s ] &&  [ ! -f $f1t ] ; then 
      printf "${format_field1} %s\n" $nam " incomplete test"
      RESTA_EC=1
      return
    fi
    if  [ ! -f $f2s ] &&  [ ! -f $f2t ] ; then 
      printf "${format_field1} %s\n" $nam " incomplete test"
      RESTA_EC=1
      return
    fi
#
    done_oce=0

    if  [  -f $f1s ] && [  -f $f2s ]; then 
      nl=(`wc -l $f2s`)
      tail -${nl[0]} $f1s > f1.tmp$$
      cmp -s f1.tmp$$ $f2s
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then 
          printf "${format_field1} %s %s\n" $nam "run.stat    restartability   passed  :" $dorv
        fi
      else
        get_ktdiff f1.tmp$$ $f2s
        printf "\e[38;5;196m${format_field1} %s %s %s %-5s %s\e[0m\n" $nam "run.stat    restartability   FAILED : " $dorv " (results are different after " $ktdiff " time steps)"
        RESTA_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view run.stat differences"
          read y
          sdiff f1.tmp$$ $f2s
          echo "<return> to view ocean.output differences"
          read y
          sdiff $f1o $f2o | grep "|"
          done_oce=1
          echo "<return> to continue"
          read y
        fi
      fi
    fi
#
# Check tracer.stat files (if they exist)
#
    if  [  -f $f1t ] && [  -f $f2t ]; then
      nl=(`wc -l $f2t`)
      tail -${nl[0]} $f1t > f1.tmp$$
      cmp -s f1.tmp$$ $f2t
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then 
          printf "${format_field1} %s %s\n" $nam "tracer.stat restartability   passed  :" $dorv
        fi
      else
        get_ktdiff2 f1.tmp$$ $f2t
        printf "\e[38;5;196m${format_field1} %s %s %s %-5s %s\e[0m\n" $nam "tracer.stat    restartability   FAILED : " $dorv " (results are different after " $ktdiff " time steps)"
        RESTA_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view tracer.stat differences"
          read y
          sdiff f1.tmp$$ $f2t
#
# Only offer ocean.output view if it has not been viewed previously
#
          if [ $done_oce == 0 ]; then
            echo "<return> to view ocean.output differences"
            read y
            sdiff $f1o $f2o | grep "|"
          fi
          echo "<return> to continue"
          read y
        fi
      fi
    fi
#
# Check obs.stat files (if they exist)
#
    if  [  -f $f1h ] && [  -f $f2h ]; then
      nl=(`wc -l $f2h`)
      tail -${nl[0]} $f1h > f1.tmp$$
      cmp -s f1.tmp$$ $f2h
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then 
          printf "${format_field1} %s %s\n" $nam "obs.stat    restartability   passed  :" $dorv
        fi
      else
        get_ktdiff2 f1.tmp$$ $f2h
        printf "\e[38;5;196m${format_field1} %s %s %s %-5s %s\e[0m\n" $nam "obs.stat    restartability   FAILED  :" $dorv " (results are different after " $ktdiff " time steps)"
        RESTA_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view obs.stat differences"
          read y
          sdiff f1.tmp$$ $f2h
#
# Only offer ocean.output view if it has not been viewed previously
#
          if [ $done_oce == 0 ]; then
            echo "<return> to view ocean.output differences"
            read y
            sdiff $f1o $f2o | grep "|"
          fi
          echo "<return> to continue"
          read y
        fi
      fi
    fi
    rm f1.tmp$$
  fi
}

function reprotest(){
#
# Reproducibility checks. Expects REPRO_N_M and REPRO_I_J run directories
# Compares end of stat files from each
#
  vdir=$1
  nam=$2
  pass=$3
#
# get $dorv
  get_dorv
#
# check if directory is here
  if [ ! -d $vdir/$mach/$dorv/$nam ]; then
    printf "${format_field1} %s %s\n" $nam  " directory                    MISSING :" $dorv
    echo " please check $vdir/$mach/$dorv/$nam"
    REPRO_EC=R1
    return
  fi
#
  if [ -d $vdir/$mach/$dorv/$nam ]; then
    # check ocean output
    runtest $vdir $nam $pass REPRO
    #
    # check reproducibility
    rep1=`ls -1rt $vdir/$mach/$dorv/$nam/ | grep REPRO | tail -2l | head -1 `
    rep2=`ls -1rt $vdir/$mach/$dorv/$nam/ | grep REPRO | tail -1l`
    if [ $rep1 == $rep2 ]; then
       rep2=''
    fi
    f1o=$vdir/$mach/$dorv/$nam/$rep1/ocean.output
    f1s=$vdir/$mach/$dorv/$nam/$rep1/run.stat
    f1t=$vdir/$mach/$dorv/$nam/$rep1/tracer.stat
    f1h=$vdir/$mach/$dorv/$nam/$rep1/obs.stat
    f2o=$vdir/$mach/$dorv/$nam/$rep2/ocean.output
    f2s=$vdir/$mach/$dorv/$nam/$rep2/run.stat
    f2t=$vdir/$mach/$dorv/$nam/$rep2/tracer.stat
    f2h=$vdir/$mach/$dorv/$nam/$rep2/obs.stat

    if  [ ! -f $f1s ] && [ ! -f $f1t ] ; then 
      printf "${format_field1} %s\n" $nam " incomplete test"
      REPRO_EC=1
      return
    fi
    if  [ ! -f $f2s ] && [ ! -f $f2t ] ; then 
      printf "${format_field1} %s\n" $nam " incomplete test"
      REPRO_EC=1
      return
    fi
#
    done_oce=0

    if  [ -f $f1s ] && [ -f $f2s ] ; then
      cmp -s $f1s $f2s
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then 
          printf "${format_field1} %s %s\n" $nam  "run.stat    reproducibility  passed  :" $dorv
        fi
      else
        get_ktdiff $f1s $f2s
        printf "\e[38;5;196m${format_field1} %s %s %s %-5s %s\e[0m\n" $nam "run.stat    reproducibility  FAILED : " $dorv " (results are different after " $ktdiff " time steps)"
        REPRO_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view run.stat differences"
          read y
          sdiff $f1s $f2s
          echo "<return> to view ocean.output differences"
          read y
          sdiff $f1o $f2o | grep "|"
          done_oce=1
          echo "<return> to continue"
          read y
        fi
      fi
    fi
#
# Check tracer.stat files (if they exist)
#
    if  [ -f $f1t ] && [ -f $f2t ] ; then
      cmp -s $f1t $f2t
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then           printf "${format_field1} %s %s\n" $nam "tracer.stat reproducibility  passed  :" $dorv
        fi
      else
        get_ktdiff2 $f1t $f2t
        printf "\e[38;5;196m${format_field1} %s %s %s %-5s %s\e[0m\n" $nam "tracer.stat reproducibility  FAILED : " $dorv " (results are different after " $ktdiff " time steps)"
        REPRO_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view tracer.stat differences"
          read y
          sdiff $f1t $f2t
#
# Only offer ocean.output view if it has not been viewed previously
#
          if [ $done_oce == 0 ]; then
            echo "<return> to view ocean.output differences"
            read y
            sdiff $f1o $f2o | grep "|"
          fi
          echo "<return> to continue"
          read y
        fi
      fi
    fi
#
# Check obs.stat files (if they exist)
#
    if  [ -f $f1h ] && [ -f $f2h ] ; then
      cmp -s $f1h $f2h
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then           printf "${format_field1} %s %s\n" $nam "obs.stat    reproducibility  passed  :" $dorv
        fi
      else
        get_ktdiff2 $f1h $f2h
        printf "\e[38;5;196m${format_field1} %s %s %s %-5s %s\e[0m\n" $nam "obs.stat    reproducibility  FAILED  :" $dorv " (results are different after " $ktdiff " time steps)"
        REPRO_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view obs.stat differences"
          read y
          sdiff $f1h $f2h
#
# Only offer ocean.output view if it has not been viewed previously
#
          if [ $done_oce == 0 ]; then
            echo "<return> to view ocean.output differences"
            read y
            sdiff $f1o $f2o | grep "|"
          fi
          echo "<return> to continue"
          read y
        fi
      fi
    fi
  fi
}

function getavgtime() {
    if [ `grep -c -e 'Average ' $1` -eq 1 ]; then
	grep -e 'Average ' $1 | cut -d '|' -f 3 | sed -e 's/[^0-9\.]//g'
    else
	grep -e 'avg over all MPI processes ' $1 | head -n 1 | sed -e 's/[^0-9\.]//g'
    fi
}

function transformtest() {
#
# Transformability checks
#
# This check expects results from configurations <CONF>+PT or <CONF>+T?? as
# well as a reference configuration <CONF>, and compares the *.stat files from
# either the LONG or a REPRO_?_? run
#
# Function arguments (validation-directory path, test name, pass counter) and
# revision number
  vdir=$1
  nam=$2
  pass=$3
  get_dorv
#
# Stop if the reference-output directory is missing
  if [ ! -d ${vdir}/${mach}/${dorv}/${nam} ]; then
    printf "${format_field1} %s %s\n" ${nam}  " directory                    MISSING :" ${dorv}
    TRANSFORM_EC=1
  else
# List of available reference runs
    RUNNAMES0=$(ls -1 ${vdir}/${mach}/${dorv}/${nam}/ | grep -e "^LONG\$" -e "^REPRO_[0-9]_[0-9]\$")
    for RUNNAME in ${RUNNAMES0}; do
      runtest ${vdir} ${nam} ${pass} ${RUNNAME}
    done
    RUNNAMES0=" ${RUNNAMES0} "
# List of transformed configurations
    DIRNAMES=$(ls -1 ${vdir}/${mach}/${dorv}/ | grep -e "^${nam}+PT\$" -e "^${nam}+T[0-9][0-9]\$")
    found_var=0
    for dirnam in ${DIRNAMES} ; do
      if [ -d ${vdir}/${mach}/${dorv}/${dirnam} ]; then
# One of the runs is selected,
        RUNNAME=`ls -1 --color=never ${vdir}/${mach}/${dorv}/${dirnam}/ | grep -m 1 -e '^LONG$' -e '^REPRO_[0-9]_[0-9]$'`
        if [ -n "${RUNNAME}" ]; then
#   but only if a corresponding reference run is available
          if [ ! "${RUNNAMES0/${RUNNAME}/}" == "${RUNNAMES0}" ]; then
            runtest ${vdir} ${dirnam} ${pass} ${RUNNAME}
            found_var=1
            # Compare timing.output (if available)
            f1t=${vdir}/${mach}/${dorv}/${nam}/${RUNNAME}/timing.output
            f2t=${vdir}/${mach}/${dorv}/${dirnam}/${RUNNAME}/timing.output
            ntime="-1"
            if [ -f ${f1t} -a -f ${f2t} ]; then
	      t0=$( getavgtime $f1t )
	      t1=$( getavgtime $f2t )
              if [[ -n "${t0}" ]] && [[ -n "${t1}" ]]; then
                rt=`echo "100 * (${t1} - ${t0}) / ${t0}" | bc -l`
                ntime=`echo "${t1} > ${t0}" | bc -l`
              fi
            fi
            done_cmp=0
            done_oce=0
            # Compare run.stat and tracer.stat (if available)
            for sfile in run.stat tracer.stat; do
              f1=${vdir}/${mach}/${dorv}/${nam}/${RUNNAME}/${sfile}
              f2=${vdir}/${mach}/${dorv}/${dirnam}/${RUNNAME}/${sfile}
              if [ -f ${f1} -a -f ${f2} ]; then
                done_cmp=1
                cmp -s $f1 $f2
                if [ $? == 0 ]; then
                  if [ ${pass} == 0 ]; then
                    if [ ${ntime} == "0" ]; then
                      printf "${format_field1} %-11s %-25s %-17s - elapsed time: \\e[42;01;196m%10.3f s (%+6.2f %%)\\e[0m\n" ${dirnam} ${sfile} "transformability passed  :" ${dorv} ${t1} ${rt};
                    elif [ ${ntime} == "1" ]; then
                      printf "${format_field1} %-11s %-25s %-17s - elapsed time: \\e[41;33;196m%10.3f s (%+6.2f %%)\\e[0m\n" ${dirnam} ${sfile} "transformability passed  :" ${dorv} ${t1} ${rt};
                    else
                      printf "${format_field1} %-11s %-25s %s\n" ${dirnam} ${sfile} "transformability passed  :" ${dorv};
                    fi
                  fi
                else
                  get_ktdiff $f1 $f2
                  printf "\e[38;5;196m${format_field1} %-11s %-25s %s %s %-5s %s\e[0m\n" ${dirnam} "${sfile}" "transformability FAILED  :" ${dorv} " (results are different after " ${ktdiff} " time steps)"
                  TRANSFORM_EC=1
                  if [ ${pass} == 1 ]; then
                    echo "<return> to view ${sfile} differences"
                    read y
                    sdiff $f1 $f2
                    if [ ${done_oce} == 0 ]; then
                      echo "<return> to view ocean.output differences"
                      read y
                      sdiff ${vdir}/${mach}/${dorv}/${nam}/${RUNNAME}/ocean.output ${vdir}/${mach}/${dorv}/${dirnam}/${RUNNAME}/ocean.output
                      done_oce=1
                    fi
                    echo "<return> to continue"
                    return y
                  fi
                fi
              fi
            done
# Test failure report if no output file has been compared
            if [ ${done_cmp} -eq 0 ]; then
              printf "${format_field1} %s\n" ${dirnam} "incomplete test"
              TRANSFORM_EC=1
              return
            fi
          fi
        fi
      fi
    done
    [ ${found_var} -eq 0 ] && printf "${format_field1} %s %s\n" ${nam}  "transformed variants         MISSING :" ${dorv}
  fi
}

function runcmpres(){
#
# compare *.stat file with reference file from a previous sette test or previous version
# store in NEMO_VALID_REF at revision NEMO_REV_REF
# Compares end of stat files from each
#
  vdir=$1
  nam=$2
  vdirref=$3
  dorvref=$4
  pass=$5
#
# get $dorv
  get_dorv
#
# check if reference directory is present
  if [ ! -d $vdirref/$mach/$dorvref/$nam ]; then
    printf "${format_field1} %s\n" $nam "REFERENCE directory at $dorvref is MISSING"
    echo " please check $vdirref/$mach/$dorvref/$nam"
    REFCMP_EC=1
    return
  fi
  if [ ! -d $vdir/$mach/$dorv/$nam ]; then
    printf "${format_field1} %s\n" $nam "VALID     directory at $dorv is MISSING"
    echo " please check $vdir/$mach/$dorv/$nam"
    REFCMP_EC=1
    return
  fi

#
  if [ -d $vdir/$mach/$dorv/$nam ]; then
    # Selection of the test run used for the comparison (LONG or one of the reproducibility-test runs)
    TESTD=$(ls -1 ${vdir}/${mach}/${dorv}/${nam}/ | grep -m 1 -e '^LONG$' -e '^REPRO_'); TESTD=${TESTD:-LONG}
    f1s=$vdir/$mach/$dorv/${nam}/${TESTD}/run.stat
    f1t=$vdir/$mach/$dorv/${nam}/${TESTD}/tracer.stat
    f1h=$vdir/$mach/$dorv/${nam}/${TESTD}/obs.stat
    f2s=$vdirref/$mach/$dorvref/${nam}/${TESTD}/run.stat
    f2t=$vdirref/$mach/$dorvref/${nam}/${TESTD}/tracer.stat
    f2h=$vdirref/$mach/$dorvref/${nam}/${TESTD}/obs.stat
    if  [ ! -f $f1s ] && [ ! -f $f1t ] ; then
      printf "${format_field1} %s\n" $nam "incomplete test"
      REFCMP_EC=1
      return
    fi
    if  [ ! -f $f2s ] && [ ! -f $f2t ] ; then
      printf "${format_field1} %s\n" $nam "incomplete test"
      REFCMP_EC=1
      return
    fi
#
    done_oce=0

    if  [ -f $f1s ] && [ -f $f2s ] ; then
      cmp -s $f1s $f2s
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then
          printf "${format_field1} %-28s %s (%s)\n" $nam "run.stat" "files are identical " ${TESTD}
        fi
      else
        get_ktdiff $f1s $f2s
        printf "${format_field1} %-28s %s %s %-5s (%s)\n" $nam "run.stat" "files are DIFFERENT (results are different after " $ktdiff " time steps) " ${TESTD}
        REFCMP_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view run.stat differences"
          read y
          sdiff $f1s $f2s
          done_oce=1
          echo "<return> to continue"
          read y
        fi
      fi
    fi
    # Check tracer.stat files (if they exist)
#
    if  [ -f $f1t ] && [ -f $f2t ] ; then
      cmp -s $f1t $f2t
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then          
          printf "${format_field1} %-28s %s (%s)\n" $nam "tracer.stat" "files are identical " ${TESTD}
        fi
      else
        get_ktdiff2 $f1t $f2t
        printf "${format_field1} %-28s %s %s %-5s (%s)\n" $nam "tracer.stat" "files are DIFFERENT (results are different after " $ktdiff " time steps) " ${TESTD}
        REFCMP_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view tracer.stat differences"
          read y
          sdiff $f1t $f2t
        fi
      fi
    fi
    # Check obs.stat files (if they exist)
#
    if  [ -f $f1h ] && [ -f $f2h ] ; then
      cmp -s $f1h $f2h
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then          
          printf "${format_field1} %-28s %s (%s)\n" $nam "obs.stat" "files are identical " ${TESTD}
        fi
      else
        get_ktdiff2 $f1h $f2h
        printf "${format_field1} %-28s %s %s %-5s (%s)\n" $nam "obs.stat" "files are DIFFERENT (results are different after " $ktdiff " time steps) " ${TESTD}
        REFCMP_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view obs.stat differences"
          read y
          sdiff $f1h $f2h
        fi
      fi
    fi
  fi
}

function runcmptim(){
#
# compare timing.output file with reference file from a previous sette test or previous version
#
  vdir=$1
  nam=$2
  vdirref=$3
  dorvref=$4
  pass=$5
#
# get $dorv
  get_dorv
#
# check if reference directory is present
  if [ ! -d $vdirref/$mach/$dorvref/$nam ]; then
    CPUCMP_EC=1
    return
  fi
  if [ ! -d $vdir/$mach/$dorv/$nam ]; then
    CPUCMP_EC=1
    return
  fi

#
  if [ -d $vdir/$mach/$dorv/$nam ]; then
    # Selection of the test run used for the comparison (LONG or one of the reproducibility-test runs)
    TESTD=$(ls -1 ${vdir}/${mach}/${dorv}/${nam}/ | grep -m 1 -e '^LONG$' -e '^REPRO_'); TESTD=${TESTD:-LONG}
    f1a=$vdir/$mach/$dorv/${nam}/${TESTD}/timing.output
    f2a=$vdirref/$mach/$dorvref/${nam}/${TESTD}/timing.output
#
# Report average CPU time differences (if available)
#
    if  [ -f $f1a ] && [ -f $f2a ] ; then
      tnew=$( getavgtime $f1a )
      tref=$( getavgtime $f2a )
      if [[ $? == 0 ]] && [[ -n "${tnew}" ]] && [[ -n "${tref}" ]]; then
        if [ $pass == 0 ]; then
          tdif=$( echo ${tnew} ${tref} | awk '{print $1 - $2}')
          if (( $(echo "$tnew > $tref" |bc -l) )); then
            printf "${format_field1} %10s %10s %14s %10s %14s \\e[41;33;196m%10s\\e[0m\n" $nam "ref. time:" $tref "cur. time:" $tnew "diff.:" $tdif
          else
            printf "${format_field1} %10s %10s %14s %10s %14s \\e[42;01;196m%10s\\e[0m\n" $nam "ref. time:" $tref "cur. time:" $tnew "diff.:" $tdif
          fi
        fi
      fi
    fi
  fi
}

function runtest(){
#
# Check test-run success
#
# This function tests for the presence of the ocean.output file for a specified
# test run (argument 4, test-run name, 'RST' for both restart runs, or 'EXP'
# for PHYOPTS variants) in the test-configuration (argument 2) validation
# sub-directory (argument 1) and for the absence of string "E R R O R" in this
# file during pass 0 or 1 (argument 3) of the test-report generation
#
  vdir=$1                                                   # validation sub-directory
  naml=$2                                                   # test configuration
  pass=$3                                                   # pass (0 or 1)
  ttype=$4                                                  # test-run type: test-run name,
  [[ $ttype == 'RST' ]] && ttype="LONG|SHORT" && phyopt=0   #    'RST' (checks both 'LONG' and 'SHORT' test runs), or
  [[ $ttype == 'EXP' ]] && ttype="^EXP-"      && phyopt=1   #    'EXP' (checks PHYOPTS test runs)
#
# get $dorv
  get_dorv
#
# no print needed if the repository is not here (already catch before)
#
  if [ -d $vdir/$mach/$dorv/${naml}/ ]; then
    #
    # apply check for all ttype directory
    rep1=$(ls -rt $vdir/$mach/$dorv/${naml}/ | grep -E $ttype)
    for tdir in $rep1 ; do
       f1o=$vdir/$mach/$dorv/${naml}/$tdir/ocean.output
       naml2=$naml
       [ $phyopt == 1 ] && naml2="${naml}/${tdir#EXP-}"
       if  [ ! -f $f1o ] ; then
          if [ $pass == 0 ]; then printf "${format_field1} %s %s\n" "${naml2}" "ocean.output                 MISSING :" $dorv ; fi
          [ $phyopt == 0 ] && OCEOUT_EC=1 && return   # record error and stop testing unless there are
          [ $phyopt == 1 ] && PHYOPT_EC=1             #    further PHYOPTS test variants to be tested
       else 
          nerr=`grep 'E R R O R' $f1o | wc -l`
          if [[ $nerr > 0 ]]; then
             printf "\e[38;5;196m${format_field1} %s %s %s\e[0m\n" "${naml2}" "run                          FAILED : " $dorv " ( E R R O R in ocean.output) " 
             if [ $pass == 1 ]; then
                echo "<return> to view end of ocean.output"
                read y
                tail -100 $f1o
                echo ''
                echo "full ocean.output available here: $f1o"
             fi
             [ $phyopt == 0 ] && OCEOUT_EC=1 && return   # record error and stop testing unless there are
             [ $phyopt == 1 ] && PHYOPT_EC=1             #    further PHYOPTS test variants to be tested
          elif [ $phyopt == 1 ]; then
             printf "${format_field1} %s %s\n" "${naml2}" "ocean.output phyopts         passed  :" $dorv
          fi
       fi
    done
  else
     if [ $pass == 0 ]; then printf "${format_field1} %s %s\n" ${naml} "directory                    MISSING :" $dorv ; fi
     [ $phyopt == 0 ] && OCEOUT_EC=1
     [ $phyopt == 1 ] && PHYOPT_EC=1
     return
  fi
}

function identictest(){
#
# Checks AGRIF does not corrupt results with no AGRIF zoom by comparing run.stat files
#
  vdir=$1
  nam=$2
  nam2=$3
  pass=$4
#
  get_dorv
#
  if [ -d $vdir/$mach/$dorv/$nam ] ; then
   rep=`ls -1rt $vdir/$mach/$dorv/$nam/ |  tail -1l`
   f1s=${vdir}/${mach}/${dorv}/${nam}/${rep}/run.stat
   f2s=${vdir}/${mach}/${dorv2}/${nam2}/${rep}/run.stat
#
   if  [ -f $f1s ] && [ -f $f2s ] ; then
      cmp -s $f1s $f2s
      if [ $? == 0 ]; then
          if [ $pass == 0 ]; then 
	      printf "${format_field1} %s %s %s\n" "${rep} AGRIF vs ${rep} NOAGRIF" "run.stat    unchanged        passed  :" $dorv $dorv2
          fi
      else
          get_ktdiff $f1s $f2s
          printf "\e[38;5;196m${format_field1} %s %s %s %s %-5s %s\e[0m\n" "${rep} AGRIF vs ${rep} NOAGRIF" "run.stat    changed        FAILED : " $dorv $dorv2 " (results are different after " $ktdiff " time steps)"
          AGRIF_EC=1
#
# Offer view of differences on the second pass
#
          if [ $pass == 1 ]; then
	      echo "<return> to view run.stat differences"
	      read y
	      sdiff $f1s $f2s
	      echo "<return> to continue"
	      read y
          fi
      fi
   else
      printf "${format_field1} %-27s %s\n" $nam $nam2 " incomplete test"
   fi
  else
      printf "${format_field1} %-27s %s\n" " " " " " non-existent test directory"
  fi
}
########################### END of function definitions #################################
##                                                                                     ##
##    Main script                                                                      ##
##                                                                                     ##
#########################################################################################
#
# LOAD param variable (COMPILER, NEMO_VALIDATION_DIR )
  SETTE_DIR=$(cd $(dirname "$0"); pwd)
  MAIN_DIR=$(dirname $SETTE_DIR)
  . ./param.cfg
  TEST_CONFIGS_AVAILABLE=${TEST_CONFIGS_AVAILABLE[@]:-${TEST_CONFIGS[@]}}     # Workaround for some dated param.cfgs files
  if [ -z $USER_INPUT ] ; then USER_INPUT='yes' ; fi        # Default: yes => request user input on decisions.
                                                            # (but may br inherited/imported from sette.sh)
  mach=${COMPILER}
# overwrite revision (later) or compiler
  if [ $# -gt 0 ]; then
    echo ""
    while getopts n:r:R:c:v:V:ubh option; do
       case $option in
          c) mach=$OPTARG;;
          r) rev=$OPTARG;;
          R) refrev=$OPTARG;;
          v) SETTE_SUB_VAL=$OPTARG;;
          V) SETTE_SUB_VAL2=$OPTARG
             if [ -d ${NEMO_VALIDATION_DIR}/${SETTE_SUB_VAL2} ] ; then
               NEMO_VALIDATION_REF=${NEMO_VALIDATION_DIR}/${SETTE_SUB_VAL2}
             else
               echo "Requested comparison subdirectory: ${NEMO_VALIDATION_DIR}/${SETTE_SUB_VAL2} does not exist"
             fi
             ;;
          u) USER_INPUT='no';;
          b) mach=${mach//_DEBUG}_DEBUG
             DEBUG="with DEBUG (-b) option"
             echo "-b: will use DEBUG compilation directory"
             echo "";;
          n) OPTSTR="$OPTARG"
             TEST_CONFIGS=(${OPTSTR})
             echo "-n: Configuration(s) ${TEST_CONFIGS[@]} will be tested if they are available"
             echo "";;
          h | *) echo ''
                 echo 'sette_rpt.sh : ' 
                 echo '     display result for the latest change'
                 echo ' -c COMPILER_name :'
                 echo '     display result for the specified compiler'
                 echo ' -r REVISION_number :'
                 echo '     display sette results for the specified revision (set old for the latest revision available for each config)'
                 echo ' -R REFERENCE REVISION_number :'
                 echo '     compare sette results against the specified revision (use to over-ride value set in param.cfg)'
                 echo ' -v sub_dir :'
                 echo '     validation sub-directory below NEMO_VALIDATION_DIR'
                 echo ' -V sub_dir2 :'
                 echo '     2nd validation sub-directory below NEMO_VALIDATION_DIR'
                 echo '     if set the comparison is between two subdirectory trees beneath NEMO_VALIDATION_DIR'
                 echo ' -u to run sette_rpt.sh without any user interaction'
                 echo ' -b to check DEBUG directory of COMPILER_name'
                 echo ''
                 exit 42;;
       esac
    done
    shift $((OPTIND - 1))
  fi
# if $1 (remaining arguments)
  if [[ ! -z $1 ]] ; then rev=$1 ; fi

# https://stackoverflow.com/questions/6059336/how-to-find-the-current-git-branch-in-detached-head-state
branchname=${CI_COMMIT_BRANCH:-$(git log -1 --pretty=%D HEAD | sed 's|.*origin/||g;s|, .*||g;s|.*-> ||g' )}
if [ ! -z $SETTE_SUB_VAL ] ; then
   NEMO_VALIDATION_DIR=$NEMO_VALIDATION_DIR/$SETTE_SUB_VAL
   if [ -d $NEMO_VALIDATION_REF/$SETTE_SUB_VAL ] && [ -z $SETTE_SUB_VAL2 ] && [ ${USER_INPUT} == "yes" ] ; then
      while true; do
      read -p "$NEMO_VALIDATION_REF/$SETTE_SUB_VAL exists. Do you wish to use it as a reference? " yn
      case $yn in
          [Yy]* ) NEMO_VALIDATION_REF=$NEMO_VALIDATION_REF/$SETTE_SUB_VAL; break;;
          [Nn]* ) echo "Ok, continuing with ${NEMO_VALIDATION_REF}/${branchname} as the reference directory"
                  NEMO_VALIDATION_REF=${NEMO_VALIDATION_REF}/${branchname}
                  break
                  ;;
          * ) echo "Please answer yes or no.";;
      esac
  done
 # No user input: make a best guess as to intent
 elif [ -d $NEMO_VALIDATION_REF/$SETTE_SUB_VAL ] && [ -z $SETTE_SUB_VAL2 ] ; then
    NEMO_VALIDATION_REF=$NEMO_VALIDATION_REF/$SETTE_SUB_VAL
 # No user input: default to branchname or MAIN
 elif [ -z $SETTE_SUB_VAL2 ] ; then
    NEMO_VALIDATION_REF=$NEMO_VALIDATION_REF/${branchname}
 fi
else
   NEMO_VALIDATION_DIR=${NEMO_VALIDATION_DIR}/${branchname}
   if [ -z $SETTE_SUB_VAL2 ] ; then
      NEMO_VALIDATION_REF=$NEMO_VALIDATION_REF/${branchname}
   fi
fi
NEMO_VALID=${NEMO_VALIDATION_DIR}
NEMO_VALID_REF=${NEMO_VALIDATION_REF}
if [ ! -z $refrev ] ; then
   NEMO_REV_REF=${refrev}
fi

if [ ! -d $NEMO_VALID ]; then
  echo "$NEMO_VALID validation directory not found"
  exit
fi
#
#
# Show current revision tag and branch name
#
echo ""
nemo_revision=$(git -C ${MAIN_DIR} rev-parse --short=8 HEAD 2> /dev/null)
rev_date0=`git log -1 | grep Date | sed -e 's/.*Date: *//' -e's/ +.*$//'`
rev_date=`${DATE_CONV}"${rev_date0}" +"%y%j"`
revision=${rev_date}_${nemo_revision}
localchanges=`git status --short -uno | wc -l`
if [[ $localchanges > 0 ]] ; then
 lastchange=${revision}+
else
 lastchange=$revision
fi

# by default use the current lastchanged revision
lastchange=${rev:-$lastchange}

echo ""
echo "SETTE validation report generated for : "
echo ""
if [[ $localchanges > 0 ]] ; then
 echo "       $branchname @ $nemo_revision (with local changes)"
else
 echo "       $branchname @ $nemo_revision"
fi
echo ""
echo "       on $COMPILER arch file $DEBUG"
echo ""

#
# The script also needs the date or revision tag. Currently this is taken from the latest sub-directory found in each directory
#  
for pass in  $RPT_PASSES 
do
#
 if [ $pass == 0 ]; then 
   echo "" 
   echo "!!---------------1st pass------------------!!"
 fi
 if [ $pass == 1 ]; then
    echo ""
    echo "!!---------------2nd pass------------------!!"
 fi
#

# Restartability test
 echo ""
 echo "   !----restart----!   "
 for restart_test in ${TEST_CONFIGS[@]/ORCA2_ICE_OBS}
 do
   [ "${restart_test}" != "ORCA2_ICE_OBS" ] && resttest $NEMO_VALID $restart_test $pass
 done
#
# Reproducibility tests
 echo ""
 echo "   !----repro----!   "
 for repro_test in ${TEST_CONFIGS[@]/C1D_PAPA}
 do
   if [[ ${repro_test} != *"OVERFLOW"* && ${repro_test} != *"LOCK_EXCHANGE"* && ${repro_test} != *"IWAVE"* ]]; then
      reprotest $NEMO_VALID $repro_test $pass
   fi
 done
# Transformability tests
 echo ""
 echo "   !----transform----!   "
 for transform_test in ${TEST_CONFIGS[@]}
 do
   transformtest ${NEMO_VALID} ${transform_test} ${pass}
 done

# PHYOPTS tests
 echo ""
 echo "   !----phyopt----!   "
 for phyopt_test in ${TEST_CONFIGS[@]}; do
    runtest $NEMO_VALID $phyopt_test $pass "EXP"
 done

# AGRIF special check to ensure results are unchanged with and without key_agrif
 if [[ ${TEST_CONFIGS[@]} =~ "AGRIF" ]]; then
   echo ""
   echo "   !----agrif check----!   "
   dir1=AGRIF_DEMO_NOAGRIF
   dir2=AGRIF_DEMO
   identictest $NEMO_VALID $dir1 $dir2 $pass 
 fi
#
# before/after tests
 if [ $lastchange == 'old' ] ; then
    echo ""
    echo "   !---- 'old' specified as revision => no comparison with reference results ----!   "
    echo ""
 else
   echo ""
   echo "   !----result comparison check----!   "
   if [ $NEMO_VALID_REF != "/path/to/reference/sette/results" ] && [ $NEMO_REV_REF != "0000" ]; then
     echo ''
     echo 'check result differences between :'
     echo "VALID directory : $NEMO_VALID at rev $lastchange"
     echo 'and'
     echo "REFERENCE directory : $NEMO_VALID_REF at rev $NEMO_REV_REF"
     echo ''
     for runcmp_test in ${TEST_CONFIGS[@]}
     do
       runcmpres $NEMO_VALID $runcmp_test $NEMO_VALID_REF $NEMO_REV_REF $pass
     done
     echo ''
     echo 'Report timing differences between REFERENCE and VALID (if available) :'
     for repro_test in ${TEST_CONFIGS[@]}
     do
       runcmptim $NEMO_VALID $repro_test $NEMO_VALID_REF $NEMO_REV_REF $pass
     done
   else
     echo ''
     echo ' No path or revision for comparison specified. Result are not compare with any other revision. '
     echo ' To do it please fill NEMO_VALID_REF and NEMO_REV_REF in param.cfg. '
     echo ''
   fi
 fi
done
#
SETTE_EC=$((REPRO_EC+RESTA_EC+TRANSFORM_EC+REFCMP_EC+CPUCMP_EC+OCEOUT_EC+AGRIF_EC+PHYOPT_EC))
echo "SETTE Report Exit Code: ${SETTE_EC}"
exit $SETTE_EC

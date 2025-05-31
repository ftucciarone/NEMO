#!/bin/bash
# set -vx
# Simple script to robustly run a full suite of SETTE tests
#
########################################
# Start of main script
########################################
FULLSET=( ORCA2_ICE_PISCES ORCA2_OFF_PISCES AMM12 AGRIF WED025 GYRE_PISCES SAS ORCA2_ICE_OBS SWG ICE_AGRIF OVERFLOW LOCK_EXCHANGE VORTEX ISOMIP+ IWAVE )
#
GROUP_SETS=( "-g 0 -r" "-q -g 2 -v NO_QCO -r" "-i -t -g 3 -v NO_ICB1 -r" "-C -g 5 -v NO_COLL -r" )
#
# These groups sets correspond to the following test regimes:
#
# A. 2 complete sets with various combinations of options:
#
  printf "%-93s %s\n" "Full tests - MAIN with default options (using *_ST0 config dirs) : "  "${GROUP_SETS[0]}"
  printf "%-93s %s\n" "Full tests - NO_QCO without qco (using *_ST1 config dirs) : " "${GROUP_SETS[1]}"
#
# B. 2 different option choices with ORCA2_ICE_PISCES only:
#
  printf "%-93s %s\n" "ORCA2_ICE_PISCES tests - NO_ICB2 without icebergs (using *_ST2 config dirs) : " "${GROUP_SETS[2]}"
  printf "%-93s %s\n" "ORCA2_ICE_PISCES tests - NO_COLL without collective comms (using *_ST3 config dirs) : " "${GROUP_SETS[3]}"
#
# A. Full tests 
for gs in 0 1
do
 for n in `seq 0 1 $(( ${#FULLSET[@]} - 1 ))`
 do
   confstr="${FULLSET[$n]}"
   # run the test 
   echo ./sette.sh ${GROUP_SETS[$gs]} -x "RESTART REPRO CORRUPT" -n "$confstr"
        ./sette.sh ${GROUP_SETS[$gs]} -x "RESTART REPRO CORRUPT" -n "$confstr"
 done
done
#
# B. ORCA2_ICE_PISCES special tests
for gs in 2 3
do
 # run the test
 echo ./sette.sh ${GROUP_SETS[$gs]} -x "RESTART REPRO" -n ORCA2_ICE_PISCES
      ./sette.sh ${GROUP_SETS[$gs]} -x "RESTART REPRO" -n ORCA2_ICE_PISCES
done
exit

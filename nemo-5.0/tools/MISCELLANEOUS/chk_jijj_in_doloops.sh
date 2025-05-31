#!/bin/bash
#
# check if a do loop, starting with DO_2D, DO_3D ou DO ji=, contains (:,:
# that most probably should be (ji,jj
#
set -u

for ff in $( find src -name "*90" ) */*/MY_SRC/*90
do
    
    for ll1 in $( grep -n DO_2D $ff | sed -s "s/:.*//" )
    do
	nb=$( sed -n ${ll1},/END_2D/p  $ff | sed -e "s/\!.*//" | grep -c "( *: *, *:" )
	if [ $nb -ne 0 ]
	then
	    echo "----------------------------------------------"
	    echo
	    echo "error in DO_2D: $ff $nb"
	    sed -n ${ll1},/END_2D/p $ff
	    echo
	fi
    done
    
    for ll1 in $( grep -n DO_3D $ff | sed -s "s/:.*//" )
    do
	nb=$( sed -n ${ll1},/END_3D/p $ff | sed -e "s/\!.*//" | grep -c "( *: *, *:" )
	if [ $nb -ne 0 ]
	then
	    echo "----------------------------------------------"
	    echo
	    echo "error in DO_3D: $ff $nb"
	    sed -n ${ll1},/END_3D/p $ff
	    echo
	fi
    done
    
    for ll1 in $( grep -in "DO  *ji *=" $ff | sed -s "s/:.*//" )
    do
	nb=$( sed -n ${ll1},/"[eE][nN][dD] *[dD][oO]"/p $ff | sed -e "s/\!.*//" | grep -c "( *: *, *:" )
	if [ $nb -ne 0 ]
	then
	    echo "----------------------------------------------"
	    echo
	    echo "error in END DO: $ff $nb"
	    sed -n ${ll1},/"[eE][nN][dD] *[dD][oO]"/p $ff
	    echo
	fi
    done
    
done






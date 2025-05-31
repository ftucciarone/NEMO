#!/bin/bash
#set -x
set -o posix
#set -u
#set -e
#+
#
# ==================
# Flist_archfile.sh
# ==================
#
# --------------------------
# Check the compilation file
# --------------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ Flist_archfile.sh Institute
#
#
# DESCRIPTION
# ===========
#
#
# List arch file available.
# The first line of each file in NEMO/arch directory is echoed.  
#
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./Flist_archfile.sh 
#
#  $ ./Flist_archfile.sh CNRS
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
# $Id: Flist_archfile.sh 9651 2018-05-28 06:47:14Z nicolasmartin $
#
#
#
#   * creation
#
#-

archfile_loop() {

	for file in $( ls $1/*.fcm ); do
		zvar1=$( basename $file | sed -e 's/arch-\(.*\).fcm/\1/' )
		zvar2=$( head -1  $file | tr -d '#'                   )
		printf "%-30s %-s\n" ${zvar1} "${zvar2}"
	done

}

echo -e "\n  ¤ Generic computing architectures"

archfile_loop ${1}/arch

for dir in $( ls ${1}/arch | grep -v "fcm$" ); do
    echo -e "\n  ¤ Specific HPC architectures for "${dir}
    archfile_loop ${1}/arch/$dir
done

echo

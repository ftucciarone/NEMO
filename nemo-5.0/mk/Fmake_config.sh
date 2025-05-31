#!/bin/bash
#set -x
set -o posix
#set -u
#set -e
#+
#
# ===============
# Fmake_config.sh
# ===============
#
# ---------------
# Make the config 
# ---------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ Fmake_config.sh
#
#
# DESCRIPTION
# ===========
#
#
# - Make the config directory if needed
# - Create repositories if needed :
#   - EXP00 for namelist, xml and AGRIF_FixedGrids.in
#   - MY_SRC for user sources
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./Fmake_config.sh CONFIG_NAME REF_CONFIG_NAME 
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
# $Id: Fmake_config.sh 9719 2018-05-31 15:57:27Z nicolasmartin $
#
#
#
#   * creation
#
#-
# function to mimic "readlink -f" command (not available on MacOS)
# source: https://stackoverflow.com/a/1116890
rlf ()
{
    TARGET_FILE=$1;
    cd `dirname $TARGET_FILE`;
    TARGET_FILE=`basename $TARGET_FILE`;
    while [ -L "$TARGET_FILE" ]; do
        TARGET_FILE=`readlink $TARGET_FILE`;
        cd `dirname $TARGET_FILE`;
        TARGET_FILE=`basename $TARGET_FILE`;
    done;
    PHYS_DIR=`pwd -P`;
    RESULT=$PHYS_DIR/$TARGET_FILE;
    echo $RESULT
}

[ ! -d ${1} ] && \mkdir -p ${1}

# CPP
[ "${2}" != "${1}" ] && \cp -n ${2}/cpp_${2##*/}.fcm ${1}/cpp_${1##*/}.fcm

# EXP00
if [ ! -d ${1}/EXP00 ]
then
    echo "   Creating ${1}/EXP00"
    \mkdir -p ${1}/EXP00
    echo "    -> Copying existing xml, namelist and AGRIF_FixedGrids.in files from ${2}/EXPREF to ${1}/EXP00"
    for f in $( ls -1 ${2}/EXPREF/*.xml ${2}/EXPREF/*namelist* ${2}/EXPREF/AGRIF_FixedGrids.in 2>/dev/null )
    do
	if [[ -L ${f} && $( rlf ${f} ) =~ "SHARED" ]]
	then
	    # create absolute(relative) symlinks if config directory is outside(inside) nemo directory
            if [[ $(dirname ${1}) != $(dirname ${2}) ]]; then
              \ln -sf $( rlf ${f} ) ${1}/EXP00/$( basename ${f} )   # keep link from SHARED
            else
              (cd ${1}/EXP00; \ln -sf ../../../cfgs/SHARED/$(basename $(rlf ${f}) ) $( basename ${f} ))
            fi
	else
	    \cp ${f} ${1}/EXP00/.
	fi
    done
fi

# MY_SRC
if [ ! -d ${1}/MY_SRC ]
then
    if [ -d ${2}/MY_SRC ]
    then
	echo "   Copying ${2}/MY_SRC to ${1}/MY_SRC"
	\cp -a ${2}/MY_SRC ${1}/MY_SRC 2> /dev/null
    else
	echo "   Creating ${1}/MY_SRC"
	\mkdir -p ${1}/MY_SRC  # create an empty directory
    fi
fi

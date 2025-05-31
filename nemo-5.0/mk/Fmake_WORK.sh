#!/bin/bash
######################################################
# Author : Rachid Benshila for NEMO
# Contact : rblod@locean-ipsl.upmc.fr
#
# Some functions called from makenemo
# Fmake_WORK      : create links in the WORK
######################################################
#set -vx
set -o posix
#set -u
#set -e
#+
#
# =============
# Fmake_WORK.sh
# =============
#
# -----------------------
# Make the WORK directory
# -----------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ Fmake_WORK.sh
#
#
# DESCRIPTION
# ===========
#
#
# Make the WORK directory:
#
# - Create line in CUR_CONF/WORK
# - Use specified sub-directories previously
# - OCE has to be done first !!!
#
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./Fmake_WORK.sh ORCA2_LIM OCE ICE
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
# $Id: Fmake_WORK.sh 15223 2021-09-01 12:12:03Z gsamson $
#
#
#
#   * creation
#
#-
ZTAB=( $1 )
ZSRC=( $2 )
ZCONF=$3
NDIR=${#ZTAB[@]}

echo ''
echo 'Creating '${ZCONF}'/WORK = '${ZTAB[*]}' for '${ZCONF##*/}
echo ''

[ ! -d ${ZCONF}/MY_SRC ] && \mkdir -p ${ZCONF}/MY_SRC
[ ! -d ${ZCONF}/BLD    ] && \mkdir -p ${ZCONF}/BLD
[   -d ${ZCONF}/WORK   ] || \mkdir -p ${ZCONF}/WORK

for comp in ${ZTAB[*]}; do
    find $comp -name \*.[Ffh]90 -exec ln -sf {} ${ZCONF}/WORK \;
done

for ZDIR in ${ZSRC[*]}; do
    if [ -d ${ZCONF}/${ZDIR} ] ; then
        d=${ZCONF}/${ZDIR}
    elif [ -d ${ZDIR} ] ; then
        d=${ZDIR}
    fi

    for ff in `(find ${d} -name \*.[Ffh]90 2>/dev/null)`
    do
        if [ "$ff" != "${ff#/}" ]; then
          ln -sf $ff ${ZCONF}/WORK/.
        else
          ln -sf ../$ff ${ZCONF}/WORK/.
        fi
    done
    echo ${d}' content is linked to '${ZCONF}/WORK
done

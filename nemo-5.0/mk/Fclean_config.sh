#!/bin/bash
######################################################
# Author : Simona Flavoni for NEMO
# Contact : sflod@locean-ipsl.upmc.fr
#
# Some functions called from makenemo
# Fclean_config   : config removing 
######################################################
#set -x
set -o posix
#set -u
#set -e
#+
#
# ================
# Fclean_config.sh
# ================
#
# ------------------------
# Remove the configuration
# ------------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ Fclean_config.sh CONFNAME
#
#
# DESCRIPTION
# ===========
#
#
# Remove the configuration:
#
# - remove CONFIG_NAME/WORK
# - remove CONFIG_NAME/BLD
# - remove CONFIG_NAME from TOOLS/mk/cfg.txt 
# 
# EXAMPLES
# ========
#
# ::
#
#  $ ./Fclean_config.sh ORCA2_LIM
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
# $Id: Fclean_config.sh 2158 2010-10-20 17:30:03Z sflod $
#
#
#
#   * creation
#
#-

CONF=$(basename ${1})
CONFIG_DIR=$(dirname ${1})

if [ ${#CONF} -eq 0 ] ; then
	echo " "
	echo "No configuration specified, please use makenemo -n CONFIG clean_config "
else
	echo " "
	echo "Are you sure that you want to remove this directory $CONF? [y/n] "
	read answer
	answer=`echo $answer | sed -e 's/^[y].*$/y/'`

	if [  -z "$answer" -o "x$answer" = "xy" ]; then

		rm -rf ${CONFIG_DIR}/${CONF}
		sed -i'' -e "/^${CONF} /d"  ${CONFIG_DIR}/work_cfgs.txt >  ${CONFIG_DIR}/work_cfgs.tmp
		echo "${CONF} configuration REMOVED" 

	else
		echo " "
		echo "nothing to remove"
	fi

fi 

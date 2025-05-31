#!/bin/bash
######################################################
# Author : Simona Flavoni for NEMO
# Contact : sflod@locean-ipsl.upmc.fr
#
# Some functions called from makenemo
# Fdel_keys   : del keys in cpp.fcm file  
######################################################
#set -x
set -o posix
#set -u
#set -e
#+
#
# ================
# Fdel_keys.sh
# ================
#
# --------------------------
# Add compilation keys
# --------------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ Fdel_keys.sh CONFIG_NAME del_key "LIST_KEYS"
#
#
# DESCRIPTION
# ===========
#
#
# Add cpp keys when compiling a configuration, key list has to be enclosed with " ".
# We perform a 'sed' on the CONFIG_NAME/CPP.fcm file, containing the list of keys. 
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./Fdel_keys.sh CONFIG_NAME del_key "key_agrif"
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
# $Id: Fdel_keys.sh 2158 2010-10-20 17:30:03Z sflod $
#
#
#
#   * creation
#
#-

echo "Removing keys ${2} in : ${1}"

for i in ${2} ; do
     if [ "$(echo ${i} | grep -c key_nproc )" -gt 0 ]; then
        sed -i'' -e "s/key_nproc[ij]=.* //" ${1}/cpp_$(basename ${1}).fcm
     elif [ "$(cat ${1}/cpp_$(basename ${1}).fcm | grep -c "$i")" -gt 0 ]; then
         sed -i'' -e "s/\b${i}\b//" ${1}/cpp_$(basename ${1}).fcm
         echo "deleted key $i in $(basename ${1})"
     fi
done

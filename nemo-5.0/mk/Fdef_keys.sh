#!/bin/bash
######################################################
# Author : 
# Contact : 
#
# Some functions called from makenemo
# Fdef_keys   : define keys in cpp.fcm file  
######################################################
#set -x
set -o posix
#set -u
#set -e
#+
#
# ================
# Fdef_keys.sh
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
#  $ Fdef_keys.sh CONFIG_NAME def_key "LIST_KEYS"
#
#
# DESCRIPTION
# ===========
#
#
# define cpp keys when compiling a configuration, key list has to be enclosed with " ".
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./Fdef_keys.sh CONFIG_NAME def_key "key_agrif"
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
#
#   * creation
#
#-

echo "Defining keys in : $(dirname ${1})"
echo " bld::tool::fppkeys ${2}" > ${1}/cpp_$(basename ${1}).fcm

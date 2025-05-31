#!/bin/bash

echo -e "\n  ¤ Reference configurations with default sub-components (can be updated by a new set)"
cat ${1}/cfgs/ref_cfgs.txt \
| awk '{printf "%-20s", $1}{$1 = ""; printf "%s\n", $0}'

echo -e "\n  ¤ Demonstrations cases (see https://github.com/sflavoni/NEMO-test-cases for more)"
cat ${1}/tests/demo_cfgs.txt \
| awk '{printf "%-20s", $1}{$1 = ""; printf "%s\n", $0}'

echo -e "\n  ¤ Available sub-components ('OCE' is mandatory in any set)"
ls ${1}/src | awk -F/ '{ print $NF }' | column

echo

#!/bin/bash
#MSUB -r rebuild_nemo_test
#MSUB -o rebuild_nemo_test.o%j
#MSUB -e rebuild_nemo_test.e%j
#MSUB -n 3
#MSUB -x
#MSUB -T 1200
#MSUB -A gen6035
#MSUB -q rome
#MSUB -m store,workflash,scratch

indir=/ccc/work/cont003/gen6035/mathiotp/NEMO/NEMO/NEMO_branches/tools/REBUILD_NEMO

ulimit -s unlimited
ulimit -n 131072

module purge
module load mpi/openmpi/4.0.5
module load hdf5/1.8.20
module load netcdf-fortran/4.4.4

# restart
echo 'rebuild restart mpp and no mpp'
nam_rebuild=nam_rebuild_rst
echo '    - mpp version'
time ccc_mprun ${indir}/mpp_rebuild_nemo.exe $nam_rebuild > log_rst_mpp 2>&1 || exit $?
mv DATA/restart-31.nc rst_mpp.nc
MD5sum_rst_mpp=`md5sum rst_mpp.nc`

echo '    - nompp version'
time ${indir}/rebuild_nemo.exe $nam_rebuild > log_rst_nompp || exit $?
mv DATA/restart-31.nc rst_nompp.nc
MD5sum_rst_nompp=`md5sum rst_nompp.nc`

# 1-4d output
echo 'rebuild grid mpp and no mpp'
nam_rebuild=nam_rebuild_grid
echo '    - nompp version'
time ccc_mprun ${indir}/mpp_rebuild_nemo.exe $nam_rebuild > log_grd_mpp || exit $?
mv XIOS/eORCA1.L121-OPM003_1d_gridT.nc grid_mpp.nc
MD5sum_grd_mpp=`md5sum grid_mpp.nc`

echo '    - nompp version'
time ${indir}/rebuild_nemo.exe $nam_rebuild > log_grd_nompp || exit $?
mv XIOS/eORCA1.L121-OPM003_1d_gridT.nc grid_nompp.nc
MD5sum_grd_nompp=`md5sum grid_nompp.nc`

# 5d output
echo 'rebuild gnzl mpp and no mpp'
nam_rebuild=nam_rebuild_gnzl
echo '    - mpp version'
time ccc_mprun ${indir}/mpp_rebuild_nemo.exe $nam_rebuild > log_gnz_mpp || exit $?
mv XIOS/eORCA1.L121-OPM003_1d_gnzl.nc gnzl_mpp.nc
MD5sum_gnz_mpp=`md5sum gnzl_mpp.nc`

echo '    - nompp version'
time ${indir}/rebuild_nemo.exe $nam_rebuild > log_gnz_nompp || exit $?
mv XIOS/eORCA1.L121-OPM003_1d_gnzl.nc gnzl_nompp.nc
MD5sum_gnz_nompp=`md5sum gnzl_nompp.nc`

# restart check
echo ''
if [ ${MD5sum_rst_mpp% *} == ${MD5sum_rst_nompp% *} ]; then 
	echo 'MPP and noMPP version of rebuild give restart exactly same results'
else
	echo 'MPP and noMPP version of rebuild give restart different results:'
	echo "    - MPP  : $MD5sum_rst_mpp"
	echo "    - NOMPP: $MD5sum_rst_nompp"
	echo "One of the possible issue is the value of ntslice_max, make sure it is the same"
fi
echo ''

# grd check
if [ ${MD5sum_grd_mpp% *} == ${MD5sum_grd_nompp% *} ]; then
	echo 'MPP and noMPP version of rebuild grid (1-4d) give exactly same results'
else
        echo 'MPP and noMPP version of rebuild grid (1-4d) give different results:'
        echo "    - MPP  : $MD5sum_grd_mpp"
        echo "    - NOMPP: $MD5sum_grd_nompp"
        echo "One of the possible issue is the value of ntslice_max, make sure it is the same"
fi
echo ''

# gznl check
if [ ${MD5sum_gzn_mpp% *} == ${MD5sum_gzn_nompp% *} ]; then
        echo 'MPP and noMPP version of rebuild gznl (5d) give exactly same results'
else
        echo 'MPP and noMPP version of rebuild gznl (5d) give different results:'
        echo "    - MPP  : $MD5sum_gzn_mpp"
        echo "    - NOMPP: $MD5sum_gzn_nompp"
        echo "One of the possible issue is the value of ntslice_max, make sure it is the same"
fi
echo ''

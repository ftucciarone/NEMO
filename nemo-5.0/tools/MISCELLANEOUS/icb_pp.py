#!/usr/bin/env python
# coding: utf-8

import xarray as xr
import pandas as pd
import numpy as np
from argparse import ArgumentParser
import sys

# load arguments
#
# Basic iceberg trajectory post-processing python script.
# This script collates iceberg trajectories from the distributed datasets written
# out by each processing region and rearranges the ragged arrays into contiguous
# streams for each unique iceberg. The output arrays are 2D (ntraj, ntimes) arrays.
# Note that some icebergs may only exist for a subset of the possible times. In these
# cases the missing instances are filled with invalid (NaN) values.
#
# Version 2.0 August 2017. Adapted to process all variables and retain original
#                          datatypes. (acc@noc.ac.uk)
# Version 3.0 May 2023. Re-write using xarray and pandas to speedup and simplify the script
#                       Output data shape similar to historical script

# default setting
pathstart_default='trajectory_icebergs_004248_'
procnum_default=0
pathout_default='trajsout.nc'

# read arguments
parser = ArgumentParser(description='produce collated trajectory file \
                                     from distributed output files, e.g. \
                                     \n python ./icb_pp.py -t  {} -n {} \
                                     -o {}'.format(pathstart_default,procnum_default,pathout_default) )

parser.add_argument('-t',dest='froot',
                         help='fileroot_of_distrbuted_data; root name \
                               of  distributed trajectory output (usually \
                               completed with XXXX.nc, where  XXXX is the \
                               4 digit processor number)',
                      default=pathstart_default)

parser.add_argument('-n',dest='fnum',help='number of distributed files to process',
                         type=int, default=procnum_default)

parser.add_argument('-o',dest='fout',
                         help='collated_output_file; file name to receive \
                              the collated trajectory data', default=pathout_default)

args = parser.parse_args()

pathstart = args.froot
procnum = args.fnum
pathout = args.fout

# sanity check
#    on the command in case default is used
if ( (pathout==pathout_default) or (pathstart==pathstart_default) or (procnum==procnum_default) ):
   print('At least one default value will be used; command executing is:')
   print('icb_pp.py -t ',pathstart,' -n ',procnum,' -o ',pathout)

#    in case no input files
if procnum < 1:
   print('Need some files to collate! procnum = ',procnum)
   sys.exit(11)

# Build to complete dataframe
df_lst=[]
for n in range(0,procnum):
    print('{}{:04d}.nc'.format(pathstart,n))
    ds=xr.open_dataset('{}{:04d}.nc'.format(pathstart,n),decode_times=False)
    if ds.dims['n'] > 0:
        zdf=ds.sel(k=[0]).squeeze(dim='k').to_dataframe()
        print('subdomain: {:04d}'.format(n))
        print('Min Max ts: {}, {}'.format(zdf.timestep.min(), zdf.timestep.max()))
        print('Number unique icebergs= {}'.format(len(zdf.iceberg_number.unique())))
        print('')
        df_lst.append(zdf)

# build dataframe by removing k dimension
if df_lst==[]:
   df=ds.sel(k=[0]).squeeze(dim='k').to_dataframe()
else:
   df=pd.DataFrame().append(df_lst, ignore_index=True)

# sanity check print
npos=len(df.iceberg_number.unique())
if npos > 0 :
    print('{:d} unique icebergs found across all datasets'.format(npos))
    print('Icebergs ids range from: {: 12d} to {: 12d}'.format(df.iceberg_number.min(), df.iceberg_number.max()))
    print('times        range from: {: 12d} to {: 12d}'.format( df.timestep.min(), df.timestep.max()))
    print('')
else:
    print('{:d} unique icebergs found across all datasets'.format(npos))

# sort dataframe ready to by converted to xarray dataset (target format: same as previous tool)
df_out=df.sort_values(by=['timestep','iceberg_number']).set_index(['iceberg_number','timestep'])

# convertion to xarray dataset
ds_out=df_out.to_xarray()

# add source files attributes
  # att of the variables
for key in iter(ds_out.keys()):
    ds_out[key].attrs=ds[key].attrs
  # att of the dimensions
for key in iter(ds_out.dims):
    ds_out[key].attrs=ds[key].attrs
    if key=='iceberg_number':
        ds_out[key].attrs['long_name']='iceberg number'

# Define encoding with _FillValue based on variable's data type
# define encoding
encoding = {var: dict(zlib=True, complevel=1) for var in ds_out.data_vars}

# write output
print('Write output: {}'.format(pathout) )
ds_out.to_netcdf(pathout, encoding=encoding, mode='w',engine="netcdf4")

print('')
print('Rebuild iceberg trajectories succesfully done')
print('')

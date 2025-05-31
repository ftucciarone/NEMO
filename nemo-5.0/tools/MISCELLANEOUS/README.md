icb_pp.py
=========

icb_pp.py is a script to rebuild trajectory output.

Requirement:
------------
- python3
- xarray
- pandas

Usage:
------
```
python ./icb_pp.py -t  trajectory_icebergs_004248_ -n 296 -o trajsout.nc
```
With:
- `trajectory_icebergs_004248_` the file name prefix
- `296` the number of proc
- `trajout.nc` the output file

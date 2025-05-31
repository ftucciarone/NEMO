import numpy as np
from argparse import ArgumentParser
from argparse import RawTextHelpFormatter
parser = ArgumentParser(description='Calculate netCDF4 chunks sizes that XIOS will use \
based on domain size \nand chunk_weight and chunk_blocksize_targets set as XML attributes, e.g.: \
\n \
\n  python3 ./calc_xios_chunks.py -i isize -j jsize -k ksize \
\n                                -t target -p fp_precision \
\n                                -wi wgt_i -wj wgt_j -wk wgt_k \
\n \
\nAll arguments are optional with default settings equivalent to \
\nXIOS defaults with a eORCA025-size domain', 
formatter_class=RawTextHelpFormatter )
#
parser.add_argument('-i', dest='isize', help='First dimension size of the domain (usually longitudinal) [1440]', default=1440)
parser.add_argument('-j', dest='jsize', help='Second dimension size of the domain (usually latitudinal) [1206]', default=1206)
parser.add_argument('-k', dest='ksize', help='Third dimension size of the domain (usually depth) [75]', default=75)
parser.add_argument('-t', dest='target',help='Target chunk blocksize in MB [20.0]', default=20)
parser.add_argument('-p', dest='prec',  help='Floating-point byte-size of op variables, (usually 4 or 8) [4]', default=4)
parser.add_argument('-wi',dest='wi',    help='Weight applied to the first dimension [1.0]',  default=1.0)
parser.add_argument('-wj',dest='wj',    help='Weight applied to the second dimension [1.0]', default=1.0)
parser.add_argument('-wk',dest='wk',    help='Weight applied to the third dimension [1.0]',  default=1.0)
#
args = parser.parse_args()
#
isize = 1440 if args.isize  is None else int(args.isize)
jsize = 1206 if args.jsize  is None else int(args.jsize)
ksize = 75   if args.ksize  is None else int(args.ksize)
target = 20  if args.target is None else float(args.target)
prec = 4     if args.prec   is None else int(args.prec)
wi = 1.0     if args.wi     is None else float(args.wi)
wj = 1.0     if args.wj     is None else float(args.wj)
wk = 1.0     if args.wk     is None else float(args.wk)
#
print("-----------------------------------------------------")
print(" XYZ domain size     : ",int(isize),"x",int(jsize),"x",int(ksize))
print(" Target chunksize    : ",int(target),"MB,  FP precision: ",int(prec))
print(" i- j- and k- weights: ",wi, wj, wk)
#
rec3d=isize*jsize*ksize*prec
rec2d=isize*jsize*prec
targmb=target*1024*1024
chunkratio3d=rec3d/targmb
chunkratio2d=rec2d/targmb
#
# 4D chunks (xyzt). t is the record dimension and always has a chunksize of 1
#
norm3d=wi+wj+wk
prodwgts3d=wi*wj*wk/(norm3d**3)
rcnt3d=1.0/3.0
#
ratio_dim_i_3d=(wi/norm3d)*((chunkratio3d/prodwgts3d)**rcnt3d)
ratio_dim_j_3d=(wj/norm3d)*((chunkratio3d/prodwgts3d)**rcnt3d)
ratio_dim_k_3d=(wk/norm3d)*((chunkratio3d/prodwgts3d)**rcnt3d)
#
if(ratio_dim_i_3d > 1.0):
 chk3d_i=np.ceil(isize/np.ceil(ratio_dim_i_3d))
 resid=ratio_dim_i_3d/(isize/chk3d_i)
else:
 chk3d_i=isize
 resid=1.0
#
if(resid*ratio_dim_j_3d > 1.0):
 chk3d_j=np.ceil(jsize/np.ceil(resid*ratio_dim_j_3d))
 resid=ratio_dim_j_3d/(jsize/chk3d_j)
else:
 chk3d_j=jsize
 resid=1.0
#
if(resid*ratio_dim_k_3d > 1.0):
 chk3d_k=np.ceil(ksize/np.ceil(resid*ratio_dim_k_3d))
else:
 chk3d_k=ksize
#
print(" 4D TZYX chunk sizes : ",int(1),int(chk3d_k),int(chk3d_j),int(chk3d_i))
#
# 3D chunks (xyt). t is the record dimension and always has a chunksize of 1
#
norm2d=wi+wj
prodwgts2d=wi*wj/(norm2d**2)
rcnt2d=1.0/2.0
#
ratio_dim_i_2d=(wi/norm2d)*((chunkratio2d/prodwgts2d)**rcnt2d)
ratio_dim_j_2d=(wj/norm2d)*((chunkratio2d/prodwgts2d)**rcnt2d)
#
if(ratio_dim_i_2d > 1.0):
 chk2d_i=np.ceil(isize/np.ceil(ratio_dim_i_2d))
 resid=ratio_dim_i_2d/(isize/chk2d_i)
else:
 chk2d_i=isize
 resid=1.0
#
if(resid*ratio_dim_j_2d > 1.0):
 chk2d_j=np.ceil(jsize/np.ceil(resid*ratio_dim_j_2d))
else:
 chk2d_j=jsize
#
print(" 3D TYX chunk sizes  : ",int(1),int(chk2d_j),int(chk2d_i))
print("-----------------------------------------------------")

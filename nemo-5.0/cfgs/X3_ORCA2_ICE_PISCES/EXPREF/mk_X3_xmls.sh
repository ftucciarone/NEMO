#!/bin/bash
#################################################################################
# source this script (. ./mk_X3_xmls.sh) to provide the following bash functions:
#
# bash function              || purpose
#------------------------------------------------------------------
# create_oce_field_def       :: creates field_def_nemo-oce.xml
# create_ice_field_def       :: creates file_def_nemo-pisces.xml
# create_pisces_field_def    :: creates field_def_nemo-ice.xml
# create_axis_def            :: creates field_def_nemo-pisces.xml
# create_domain_def          :: creates axis_def_nemo.xml
# create_oce_file_def        :: creates domain_def_nemo.xml
# create_ice_file_def        :: creates file_def_nemo-oce.xml
# create_pisces_file_def     :: creates file_def_nemo-ice.xml
#
# These functions can be run without arguments in the X3_ORCA2_ICE_PISCES/EXPREF
# directory to build XIOS3-ready xmls from the ../../SHARED versions by:
#----------------------------------------------------------------------
# A. field_def functions to insert an overall field_group that sets the 
#    chunking_blocksize_target. 
#----------------------------------------------------------------------
# B. axis and domain functions to update deprecated zoom_axis and zoom_domain 
#    attributes and insert chunking weights 
#----------------------------------------------------------------------
# C. file_def functions insert chunking and compression attributes 
#    into the original ORCA2_ICE_PISCES/EXPREF versions
#----------------------------------------------------------------------
#################################################################################
#
#--------------------------
create_oce_field_def(){
#--------------------------
      file=field_def_nemo-oce.xml
      if [ ! -f $file ] ; then
        echo "$file does not exist and will be created"
        if [ ! -f $file ] ; then
          cp ../../SHARED/$file .
          match="field_definition level"
          insert='<field_group id="all_ocean" chunking_blocksize_target="3.0">'
          sed -i "s/\(${match}.*$\)/\1\n$insert/" $file
          #
          match='</field_definition'
          insert='</field_group>   <!-- close all_ocean group -->'
          sed -i "s;\(${match}.*$\);$insert\n\1;" $file
        else
          echo "../../SHARED/$file does not exist"
          echo "unable to continue (source file not found)"
          exit
        fi
      else
        echo "$file already exists"
        echo "unable to continue (refuse to overwrite)"
      fi
}
      #
#--------------------------
create_ice_field_def(){
#--------------------------
      file=field_def_nemo-ice.xml
      if [ ! -f $file ] ; then
        echo "$file does not exist and will be created"
        if [ ! -f $file ] ; then
          cp ../../SHARED/$file .
          match='<field_group id="SBC"'
          insert=' chunking_blocksize_target="3.0"'
          sed -i "s/${match}/${match}$insert/" $file
        else
          echo "../../SHARED/$file does not exist"
          echo "unable to continue (source file not found)"
          exit
        fi
      else
        echo "$file already exists"
        echo "unable to continue (refuse to overwrite)"
      fi
}
#
#--------------------------
create_pisces_field_def(){
#--------------------------
      file=field_def_nemo-pisces.xml
      if [ ! -f $file ] ; then
        echo "$file does not exist and will be created"
        if [ ! -f $file ] ; then
          cp ../../SHARED/$file .
          match="field_definition level"
          insert='<field_group id="all_pisces" chunking_blocksize_target="3.0">'
          sed -i "s/\(${match}.*$\)/\1\n$insert/" $file
          #
          match='</field_definition'
          insert='</field_group>   <!-- close all_pisces group -->'
          sed -i "s;\(${match}.*$\);$insert\n\1;" $file
        else
          echo "../../SHARED/$file does not exist"
          echo "unable to continue (source file not found)"
          exit
        fi
      else
        echo "$file already exists"
        echo "unable to continue (refuse to overwrite)"
      fi
}
#
#--------------------------
create_axis_def(){
#--------------------------
      file=axis_def_nemo.xml
      if [ ! -f $file ] ; then
        echo "$file does not exist and will be created"
        if [ ! -f $file ] ; then
          cp ../../SHARED/$file .
          match='axis id="depth."'
          insert='chunking_weight="12.0"'
          sed -i "/${match}/s:/>:$insert />:" $file
          #
          match='"deptht300"'
          insert='chunking_weight="12.0"'
          sed -i "/${match}/s:>:$insert >:" $file
          #
          match='"nlayice"'
          insert='chunking_weight="1.0"'
          sed -i "/${match}/s:/>:$insert />:" $file
          #
          sed -i "s;zoom_axis;extract_axis;g" $file
        else
          echo "../../SHARED/$file does not exist"
          echo "unable to continue (source file not found)"
          exit
        fi
      else
        echo "$file already exists"
        echo "unable to continue (refuse to overwrite)"
      fi
}
#
#--------------------------
create_domain_def(){
#--------------------------
      file=domain_def_nemo.xml
      if [ ! -f $file ] ; then
        echo "$file does not exist and will be created"
        if [ ! -f $file ] ; then
          cp ../../SHARED/$file .
          match='domain id="grid_'
          insert=' chunking_weight_i="1.0" chunking_weight_j="1.0"'
          sed -i "/${match}/s:/>:${insert}/>:" $file
          #
          sed -i "s;zoom_domain;extract_domain;g" $file
        else
          echo "../../SHARED/$file does not exist"
          echo "unable to continue (source file not found)"
          exit
        fi
      else
        echo "$file already exists"
        echo "unable to continue (refuse to overwrite)"
      fi
}
#
#--------------------------
create_oce_file_def(){
#--------------------------
      file=file_def_nemo-oce.xml
      if [ ! -f $file ] ; then
        echo "$file does not exist and will be created"
        if [ ! -f $file ] ; then
          cp ../../ORCA2_ICE_PISCES/EXPREF/$file .
          match='"one_file"'
          insert=' compression_level="1"'
          sed -i "s:${match}:${match} ${insert}:" $file
          #
          insert=' mode="write" gatherer="tgatherer" writer="twriter" using_server2="true"'
          for match in 'name_suffix="_grid_T"' 'name_suffix="_grid_W"'
          do
           sed -i "s:${match}:${match} ${insert}:" $file
          done
          #
          insert=' mode="write" gatherer="ugatherer" writer="uwriter" using_server2="true"'
          for match in 'name_suffix="_grid_U"' 'name_suffix="_grid_V"' 'name_suffix="_diaptr.D"'
          do
           sed -i "s:\(${match}\):\1 ${insert}:" $file
          done
          #
          match='name_suffix="_scalar"'
          insert='compression_level="0" mode="write" gatherer="tgatherer" writer="twriter" using_server2="true"'
          sed -i "s:${match}:${match} ${insert}:" $file
          #
        else
          echo "../../SHARED/$file does not exist"
          echo "unable to continue (source file not found)"
          exit
        fi
      else
        echo "$file already exists"
        echo "unable to continue (refuse to overwrite)"
      fi
}
#
#--------------------------
create_ice_file_def(){
#--------------------------
      file=file_def_nemo-ice.xml
      if [ ! -f $file ] ; then
        echo "$file does not exist and will be created"
        if [ ! -f $file ] ; then
          cp ../../ORCA2_ICE_PISCES/EXPREF/$file .
          match='"one_file"'
          insert=' compression_level="1"'
          sed -i "s:${match}:${match} ${insert}:" $file
          #
          insert='mode="write" gatherer="igatherer" writer="iwriter" using_server2="true"'
          match='name_suffix="_icemod"'
          sed -i "s:${match}:${match} ${insert}:" $file
          #
          insert='compression_level="0" mode="write" gatherer="igatherer" writer="iwriter" using_server2="true"'
          match='name_suffix="_SBC_scalar"'
          sed -i "s:${match}:${match} ${insert}:" $file
          #
          insert='compression_level="0"'
          match='file_group id="1m"'
          sed -i "s:${match}:${match} ${insert}:" $file
          #
        else
          echo "../../SHARED/$file does not exist"
          echo "unable to continue (source file not found)"
          exit
        fi
      else
        echo "$file already exists"
        echo "unable to continue (refuse to overwrite)"
      fi
}
#
#--------------------------
create_pisces_file_def(){
#--------------------------
      file=file_def_nemo-pisces.xml
      if [ ! -f $file ] ; then
        echo "$file does not exist and will be created"
        if [ ! -f $file ] ; then
          cp ../../ORCA2_ICE_PISCES/EXPREF/$file .
          match='"one_file"'
          insert=' compression_level="1"'
          sed -i "s:${match}:${match} ${insert}:" $file
          #
          insert=' mode="write" gatherer="pgatherer" writer="pwriter" using_server2="true"'
          for match in 'name_suffix="_ptrc_T"' 'name_suffix="_diad_T"'
          do
           sed -i "s:${match}:${match} ${insert}:" $file
          done
          #
          match='name_suffix="_bioscalar"'
          insert='compression_level="0" mode="write" gatherer="pgatherer" writer="pwriter" using_server2="true"'
          sed -i "s:${match}:${match} ${insert}:" $file
          #
        else
          echo "../../SHARED/$file does not exist"
          echo "unable to continue (source file not found)"
          exit
        fi
      else
        echo "$file already exists"
        echo "unable to continue (refuse to overwrite)"
      fi
}

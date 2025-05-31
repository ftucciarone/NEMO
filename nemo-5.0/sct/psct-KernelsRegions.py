#! /usr/bin/python
# ======================================================================
#                    ***  psct-KernelsRegions.py  ***
# ======================================================================
#  History : 5.0  !  2024  (S. Mueller)
# ----------------------------------------------------------------------
#
# This script is a PSyclone (https://github.com/stfc/PSyclone) transformation
# script for the annotation of potentially parallelisable code regions with
# OpenACC directives in order to instruct OpenACC-enabled Fortran compilers to
# prepare such regions for execution on a GPGPU accelerator. Starting from
# array assignments and computational kernels within spatial and tracer loops
# identified by PScylone, it iteratively grows such seed regions i) through
# merging of neighbouring regions, ii) by including scalar assignments, and
# iii) by hoisting the scope of regions to include surrouding spatial and
# tracer loops or if constructs, until a steady state is reached. This script
# can be regarded as an alternative implementation of the recursive
# 'kernels_trans.py' and 'acc_kernels_trans.py' transformation scripts of
# PSyclone version 2.4.0
# (<PSYCLONE_2_4_0_ROOT>/examples/nemo/scripts/kernels_trans.py) and PSyclone
# version 2.5.0
# (<PSYCLONE_2_5_0_ROOT>/examples/nemo/scripts/acc_kernels_trans.py),
# respectively.
#
# The entry point of the transformation script is procedure 'trans'. This
# script could be used directly with the 'psyclone' command (e.g., via the NEMO
# build system), but it can also be incorporated into a
# "super-transformation-script" that collates various PSyclone transformation
# scripts (such as sct/psct-OpenACC.py).
#
# This script has been developed by building the NEMO BENCH test case from the
# transformed source code using the Cray Fortran Compiler version 15.0.0 with
# option '-hacc'; the invokes (module procedures) listed in variable
# INVOKES_REJECT_PREFIXES are excluded in order to avoid failures during the
# build process of this configuration.
#
# ----------------------------------------------------------------------
# NEMO 5.0 , NEMO Consortium (2024)
# Software governed by the CeCILL license (see ./LICENSE)
# ----------------------------------------------------------------------

from psyclone.psyir.nodes                 import Assignment, ACCKernelsDirective, CodeBlock, Call
from psyclone.psyir.nodes                 import IntrinsicCall, IfBlock, ArrayReference, Literal, Return
from psyclone.transformations             import ACCKernelsTrans, ACCLoopTrans
from psyclone.nemo                        import NemoLoop

# ----------------------------------------------------------------------
#                      ***  Configuration  ***
# ----------------------------------------------------------------------
#
PSCT_LOG_PREFIX = "[PSyclone transformation] OpenACC"

# ----------------------------------------------------------------------
# Rejection of modules and invokes (all names in lowercase)
# ----------------------------------------------------------------------
#
# Reject modules related to profiling and MPI exchanges for now
MODULES_REJECT_PREFIXES = [ 'timing', 'lbc', 'lib_mpp', 'mpp' ]
# and for compatibility with PSyclone 2.5.0
MODULES_REJECT_PREFIXES += [ 'agrif2model', 'agrif_user' ]
# Reject invokes whose compilation would lead to an error with at least one
# commonly used Fortran compiler
INVOKES_REJECT_PREFIXES = [ 'copy_obfbdata',             # Derived-type issues at linking stage
                            'dia_dct',                   # Issue potentially related to 'RESHAPE' intrinsic
                            'dia_obs_wri',               # Derived-type issues at linking stage
                            'merge_obfbdata',            # Derived-type issues at linking stage
                            'obs_rea_prof',              # Unsupported string operations
                            'obs_rea_surf',              # Unsupported string operations
                            'obs_surf_compress',         # Unsupported derived-type assignment
                            'obs_prof_compress',         # Unsupported derived-type assignment
                            'obs_prof_decompress',       # Issue at linking stage
                            'obs_wri_prof',              # Derived-type issue at linking stage
                            'obs_wri_surf',              # Derived-type issue at linking stage
                            'prt_ctl_write_sum',         # Unsupported string operations
                            'ptr_mpp_sum_2d',            # Issue potentially related to 'RESHAPE' intrinsic
                            'ptr_mpp_sum_3d',            # Issue potentially related to 'RESHAPE' intrinsic
                            'ptr_mpp_sum_4d',            # Issue potentially related to 'RESHAPE' intrinsic
                            'put_twrk',                  # Unsupported string assignment
                            'subsamp_obfbdata' ]         # Derived-type issues at linking stage
# and reject initialisation procedures whose execution on a specific GPU has
# been found to lead to model-result differences in comparison to the
# corresponding run without GPU support
INVOKES_REJECT_PREFIXES += [ 'ldf_c1d', 'ldf_c2d' ]

# ----------------------------------------------------------------------
# Auxiliary functions
# ----------------------------------------------------------------------

# Auxiliary function that checks the inclusion of a node in a list of nodes
def is_included(node, node_list):
    return any([node is n for n in node_list])

# Auxiliary function that returns the older (lower position) sibling of a node,
# if present; it returns False if no older sibling is available
def older_sibling(node):
    if node.position > 0:
        return node.parent.children[node.position-1]
    return False

# Auxiliary function that returns the younger (higher position) sibling of a
# node, if present; it returns False if no younger sibling is available
def younger_sibling(node):
    if node.position < len(node.parent.children)-1:
        return node.parent.children[node.position+1]
    return False

# Auxiliary function to check if a set of nodes comprises a complete and
# ordered set of a parent node's children
def complete_ordered_family(nodes):
    for n in range(len(nodes)):
        if not nodes[0].sameParent(nodes[n]) and not nodes[n].position == n:
            return False
    return len(nodes[0].parent.children) == len(nodes)

# Auxiliary function to check if a node contains a CodeBlock (uninterpreted
# Fortran code)
def has_codeblock(node):
    if len(node.walk(CodeBlock)):
        return True
    return False

# Auxiliary function to check if a node contains any procedure call other than
# calls of intrinsic procedures; as PSyclone cannot currently reliably
# distinguish between function calls and array accesses of functions or arrays
# that are use-associated in the current context, some known function calls are
# explicitly identified here (KNOWN_USE_ASSOCIATED_FUNCTIONS)
KNOWN_USE_ASSOCIATED_FUNCTIONS = [ 'cd_from_z0',
                                   'cdn_f_lg15_light',
                                   'f_h_louis',
                                   'f_m_louis',
                                   'grt_cir_dis',
                                   'grt_cir_dis_saa',
                                   'icb_utl_mass',
                                   'ice_var_sshdyn',
                                   'in_hdom',
                                   'lbnd_ij',
                                   'local_sum',
                                   'local_2dsum',
                                   'nf90_close',
                                   'nf90_enddef',
                                   'nf90_put_var',
                                   'q_air_rh',
                                   'q_sat',
                                   'qlw_net',
                                   'rho_air',
                                   'ri_bulk',
                                   'sbc_dcy',
                                   'solfrac',
                                   'theta_exner',
                                   'virt_temp' ]
def has_nonintrinsic_call(node):
    has_1 = len([r for r in node.walk(ArrayReference) if r.name.lower() in KNOWN_USE_ASSOCIATED_FUNCTIONS]) > 0
    has_2 = len([c for c in node.walk(Call) if not (isinstance(c, IntrinsicCall) or c.routine.name == 'psyclone_internal_cmp')]) > 0
    return has_1 or has_2

# Auxiliary function to detect the presence of string literals, as a value
# being assigned to a variable or as a value used in a string comparison, or
# the use of the 'TRIM' intrinsic.
def has_character_literal_or_trim_intrinsic(node):
    for literal in node.walk(Literal):
        if literal.datatype.intrinsic.name.lower() == 'character':
            return True
    for call in node.walk(IntrinsicCall):
        if call.routine.name.lower() == 'trim':
            return True
    return False

# Auxiliary function to detect the presence of a 'RETURN' statement in the node
def has_return_statement(node):
    return len(node.walk(Return)) > 0

# Auxiliary function that tests for the absence of a range of elements that are
# to be avoided in kernels regions, incl. for the absence of nodes that have
# already been added to a kernels region
def is_acceptable_node(node, nodes_in_kernels_regions):
    return not (has_codeblock(node) or
                has_nonintrinsic_call(node) or
                has_character_literal_or_trim_intrinsic(node) or
                has_return_statement(node) or
                is_included(node, nodes_in_kernels_regions))

# ----------------------------------------------------------------------
#             ***  PSyclone transformation procedure  ***
# ----------------------------------------------------------------------
def trans(psy):

    print()
    print("=================================")
    print(PSCT_LOG_PREFIX)
    print("=================================")
    print()

    # Overall kernels-region counter and invoke list
    nkernels = 0
    invoke_list = ''

    # List of accepted invokes
    invokes = []
    if not len([ m for m in MODULES_REJECT_PREFIXES if psy.name.lower().startswith('psy_'+m) ]):
        for invoke in psy.invokes.invoke_list:
            if not len([ m for m in INVOKES_REJECT_PREFIXES if invoke.name.lower().startswith(m) ]):
                invokes.append(invoke)
            else:
                print(PSCT_LOG_PREFIX+": module "+psy.name+", invoke "+invoke.name+" rejected")
    else:
        print(PSCT_LOG_PREFIX+": module "+psy.name+" rejected")

    # Find kernels regions separately for each invoke
    for invoke in invokes:

        print(PSCT_LOG_PREFIX+": module "+psy.name+", invoke "+invoke.name)
        schedule = invoke.schedule

        # Find seed regions from which to grow kernels regions for
        # parallelisation: these seeds include array assignments as well as the
        # innermost content of identified NemoLoop nodes or loop nests (loops
        # with the conventional loop variables jn, jk, jj, ji). The seeds are
        # stored as lists of nodes for which an OpenACC kernel region can be
        # created as 'ACCKernelsTrans().apply(<list of nodes>)' and which
        # retains the order these nodes appear in the overall schedule,
        kernels = []
        # as well as in a list of all nodes that have been added to a kernels
        # region.
        nodes_included = []
        # Find acceptable loop and assignment nodes, but
        for kernel in [node for node in schedule.walk((NemoLoop,Assignment)) if is_acceptable_node(node, [])]:
            # only accept seeds that solely contain an array assignment
            if [node for node in kernel.walk(Assignment) if node.is_array_assignment]:
                if isinstance(kernel, Assignment):
                    kernels.append([ kernel ])
                    nodes_included.append(kernel)
            # or NemoLoop nodes that do not contain another NemoLoop or array
            # assignment
            elif isinstance(kernel, NemoLoop) and len(kernel.walk(NemoLoop)) == 1:
                kernels.append([ kernel ])
                nodes_included.append(kernel)

        # Main iteration: the list of kernels regions is being consolidated
        # until a steady state is reached; also record collapsible horizontal
        # loops
        steady = False
        niter = 1
        collapse_loops = []
        while not steady:
            #
            # Unless a kernels region is modified during one of the four steps
            # below, the state is considered to be steady by default
            steady = True
            #
            # Step 1 - Merge neighbouring kernels regions
            # Start with the first available kernels region (if any) ...
            kernels_new = kernels[:1]
            # ... and try to merge each subsequent region with the previous one
            for kernel in kernels[1:]:
                # Check if the first node of the next region has an "older"
                # sibling and if this "older" sibling is the last node of the
                # previous region
                if older_sibling(kernel[0]) and kernels_new[-1][-1] is older_sibling(kernel[0]):
                    # If so, merge regions and mark the kernels-region list as
                    # having changed
                    kernels_new[-1] = kernels_new[-1] + kernel
                    steady = False
                else:
                    # Otherwise, silently copy the next entry to the revised
                    # list
                    kernels_new.append(kernel)
            # Replace current list with revised list
            kernels = kernels_new
            #
            # Step 2 - Add assignments (scalar or array assignments, although
            # array assignments should already have been included in kernels
            # regions) that are adjacent siblings of existing regions
            kernels_new = []
            for kernel in kernels:
                # Find siblings on either side
                kext1 = older_sibling(kernel[0])
                kext2 = younger_sibling(kernel[-1])
                # Is any of them an assignment node and acceptable? If so,
                # merge it into the kernel on the appropriate side and mark the
                # kernels-region list as having changed
                if kext1 and isinstance(kext1, Assignment) and is_acceptable_node(kext1, nodes_included):
                    kernels_new.append([kext1] + kernel)
                    nodes_included.append(kext1)
                    steady = False
                elif kext2 and isinstance(kext2, Assignment) and is_acceptable_node(kext2, nodes_included):
                    kernels_new.append(kernel + [kext2])
                    nodes_included.append(kext2)
                    steady = False
                else:
                    kernels_new.append(kernel)
            # Replace current list with revised list
            kernels = kernels_new
            #
            # Step 3 - Hoist regions to include surrounding loops
            kernels_new = []
            for kernel in kernels:
                # Is the kernels region the complete (ordered) content of a
                # loop?
                hoisted_region = kernel[0].parent.parent
                if isinstance(hoisted_region, NemoLoop) and is_acceptable_node(hoisted_region, nodes_included) and complete_ordered_family(kernel):
                    # Do we happen to have found a nested pair of loops of
                    # types 'lat' and 'lon'? If so, the addition of an OpenACC
                    # directive to collpase the horizontal loops is scheduled.
                    if isinstance(kernel[0], NemoLoop) and kernel[0].loop_type == 'lon' and hoisted_region.loop_type == 'lat' and len(kernel[0].parent.children) == 1:
                        collapse_loops.append(hoisted_region)
                    # Replace kernels region with hoisted region and mark the
                    # kernels-region list as having changed
                    kernels_new.append([ hoisted_region ])
                    nodes_included.append(hoisted_region)
                    steady = False
                else:
                    kernels_new.append(kernel)
            # Replace current list with revised list
            kernels = kernels_new
            #
            # Step 4 - Extend regions around IfBlocks with bodies that are
            # fully included in kernels regions
            kernels_new = []
            block = []
            for kernel in kernels:
                # Is the kernels region part of the 'if' section of an if
                # construct?
                if_block = kernel[0].parent.parent
                if not (isinstance(if_block, IfBlock) and is_included(kernel[0], if_block.if_body)):
                    if_block = False
                # If so, does it cover the complete 'if' section and can
                # the conditional be included in a kernels region?
                if_complete = False
                if if_block and is_acceptable_node(if_block.condition, nodes_included) and len(kernel) == len(if_block.if_body.children):
                    if_complete = True
                    # If so, accept the if construct provisionally and check if
                    # there is an 'else' section?
                    if if_block.else_body and len(if_block.else_body.children) > 0:
                        # If so, is the 'else' section completely covered by a
                        # kernels region?
                        if_complete = False
                        for kernel2 in kernels:
                            if if_block.else_body.children[0].sameParent(kernel2[0]) and len(kernel2) == len(if_block.else_body.children):
                                if_complete = True
                                # If so, accept the if construct, and record
                                # that the 'else' section is being taken care
                                # of.
                                block.append(kernel2[0])
                # If the content of the if construct is completely covered by
                # kernels regions, replace the regions by the surrounding if
                # construct and mark the kernels-region list as having changed
                if if_complete:
                    kernels_new.append([if_block])
                    nodes_included.append(if_block)
                    steady = False
                elif not is_included(kernel[0], block):
                    kernels_new.append(kernel)
            # Replace current list with revised list
            kernels = kernels_new
            #
            niter += 1

        # Insert OpenACC directives:
        #    - kernel regions
        for kernel in kernels:
            ACCKernelsTrans().apply(kernel)
        #    - horizontal-loop collapse
        for loop in collapse_loops:
            # Make sure the loop collapse is inside a kernels region
            if loop.ancestor(ACCKernelsDirective):
                try:
                    ACCLoopTrans().apply(loop, {'independent': True, 'collapse': 2})
                except:
                    print(PSCT_LOG_PREFIX+": module "+psy.name+", invoke "+invoke.name+" - OpenACC loop collapse rejected")

        # Collate information for reporting
        if len(kernels):
            nkernels += len(kernels)
            if len(invoke_list):
                invoke_list += ", "
            invoke_list += invoke.name

    if nkernels > 0:
        print(PSCT_LOG_PREFIX+": module "+psy.name+" "+str(nkernels)+" kernels regions added to invokes "+invoke_list)

    return

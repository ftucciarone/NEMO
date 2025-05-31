#! /usr/bin/python
# ======================================================================
#                     ***  psct-list_symbols.py  ***
# ======================================================================
# History : 4.3  ! 2023-06  (S. Mueller) Initial version
# ----------------------------------------------------------------------
#
# PSyclone (https://github.com/stfc/PSyclone) transformation script, which,
# when supplied to the PSyclone source-code processing build stage (./makenemo
# -p list_symbols [...]), results in the output of the module and procedure
# symbol tables of the processed source-code file and otherwise in the passing
# of the source code through the PSyclone processing stage without functional
# transformations. See
# https://psyclone.readthedocs.io/en/stable/transformation.html#sec-transformation-script
# for information on PSyclone transformation-script conventions.
#
# ----------------------------------------------------------------------
# NEMO 4.3 , NEMO Consortium (2023)
# Software governed by the CeCILL license (see ./LICENSE)
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
#               ***  PSyclone transformation procedure  ***
# ----------------------------------------------------------------------
def trans(psy):

    print()
    print("[PSyclone transformation] Passthrough and list of module ('Container') and procedure ('NemoInvokeSchedule') symbols")
    print("===================================================================================================================")
    print()

    # Print module and procedure symbol tables
    invokes = psy.invokes.invoke_list
    if len(invokes):
        for invoke in invokes:
            st = invoke.schedule.symbol_table
            if invoke == invokes[0]:
                pst = st.parent_symbol_table()
                if len(pst.symbols):
                    print(pst)
            if len(st.symbols):
                print(st)

    return

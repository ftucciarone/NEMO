#! /usr/bin/python
# ======================================================================
#                   ***  psct-TracerTypeToggles.py  ***
# ======================================================================
#  History : 5.0  !  2024  (S. Mueller)
# ----------------------------------------------------------------------
#
# PSyclone transformation script that replaces comparisons of a string variable
# (typically passed as argument) with either 'TRA' or 'TRC' by a suitably
# defined boolean variable, in order to enable the extension of kernels regions
# (within which string operations are not accepted) beyond such comparisons.
#
# ----------------------------------------------------------------------
# NEMO 5.0 , NEMO Consortium (2024)
# Software goverened by the CeCILL license (see ./LICENSE)
# ----------------------------------------------------------------------

from psyclone.psyir.nodes     import Reference, Literal, Assignment, IfBlock, BinaryOperation
from psyclone.psyir.symbols   import DataSymbol, BOOLEAN_TYPE

# ----------------------------------------------------------------------
#              ***  PSyclone transformation procedure  ***
# ----------------------------------------------------------------------
def trans(psy):

    for invoke in psy.invokes.invoke_list:
        schedule = invoke.schedule
        st = schedule.symbol_table
        for ts in [ 'TRA', 'TRC' ]:
            # Hoist comparisons with string literals 'TRA' and 'TRC' to the
            # beginning of the procedure to define a corresponding boolean, and
            # replace the original comparisons with this boolean
            cmps = [ l.parent for l in schedule.walk(Literal) if l.value == ts and isinstance(l.parent, BinaryOperation) and l.parent.operator == BinaryOperation.Operator.EQ ]
            if len(cmps) > 0:
                # Create local boolean variable
                lts = st.new_symbol(root_name='ll'+ts, symbol_type=DataSymbol, datatype=BOOLEAN_TYPE)
                for cmp in cmps:
                    # Replace original comparisons
                    cmp.replace_with(Reference(lts))
                # Insert conditional assignment of the newly added boolean
                t = Assignment.create(Reference(lts), Literal('true', datatype=BOOLEAN_TYPE))
                f = Assignment.create(Reference(lts), Literal('false', datatype=BOOLEAN_TYPE))
                schedule.children.insert(0, IfBlock.create(cmp, [t], [f]))
    return
#!/bin/bash
set -u
#
rm -f netcdf*90 typeSizes.F90
rm -rf netcdf-fortran
git clone https://github.com/Unidata/netcdf-fortran.git


ncdfdir=netcdf-fortran/fortran
#sed -e "s/F90/h90/" $ncdfdir/netcdf.F90  > netcdf.F90 
sed -e "s/F90/h90/" $ncdfdir/netcdf4.F90 > netcdf4.F90
cp $ncdfdir/../RELEASE_NOTES.md .
for flist in $( grep "#include" $ncdfdir/netcdf4.F90  | sed -e 's/.* "//' -e 's/".*//' ) typeSizes.F90

do
    ff=$ncdfdir/$flist
    echo $ff
    # remove tab, remove headind &, delete comments, use only lowercase for some names
#    sed -e "s/\t//g" $ff | sed -e "/^ *\!/d" -e "s/\(^[^#\'\"]*\)\!.*$/\1/" \
#    sed -e "s/\t//g" $ff | sed -e "s/^\( *\)&\( *\)/\1 \2/" -e "/^ *\!/d" -e "s/\(^[^#\'\"]*\)\!.*$/\1/" \
    sed -e "s/\t//g" $ff | sed -e "s/^ *& *//" -e "/^ *\!/d" -e "s/\(^[^#\'\"]*\)\!.*$/\1/" \
			       -e "s/[fF][uU][nN][cC][tT][iI][oO][nN]/function/" \
			       -e "s/[sS][uU][bB][rR][oO][uU][tT][iI][nN][eE]/subroutine/" \
			       -e "s/[cC][oO][nN][tT][aA][iI][nN][sS]/contains/" \
			       -e "s/[rR][eE][sS][uU][lL][tT]/result/" $ff > tmp

    # define lines to be kept
    sed -e "s/\(^.*[iI][nN][tT][eE][nN][tT] *( *[iIoO][nNuU][oOtT]*[uUtT]*[tT]* *)\)/To_Keep\1/" \
	-e "s/\(^ *#\)/To_Keep\1/" \
	-e "s/\(^ *[^ ].* subroutine * [^ ][^ ]* *$\)/To_Keep\1/" \
	-e "s/\(^ *[^ ].* subroutine * [^ ][^ ]* *(\)/To_Keep\1/" \
	-e "s/\(^ *subroutine * [^ ][^ ]* *$\)/To_Keep\1/" \
	-e "s/\(^ *subroutine * [^ ][^ ]* *(\)/To_Keep\1/" \
	-e "s/\(^ *[^ ].* function * [^ ][^ ]* *$\)/To_Keep\1/" \
	-e "s/\(^ *[^ ].* function * [^ ][^ ]* *(\)/To_Keep\1/" \
	-e "s/\(^ *function * [^ ][^ ]* *$\)/To_Keep\1/" \
	-e "s/\(^ *function * [^ ][^ ]* *(\)/To_Keep\1/" \
	-e "s/\(^ *[mM][oO][dD][uU][lL][eE] * [^ ][^ ]* *$\)/To_Keep\1/" \
	-e "s/\(^ *[eE][nN][dD] * [mM][oO][dD][uU][lL][eE] * [^ ][^ ]* *$\)/To_Keep\1/" \
	-e "s/\(^ *[pP][rR][oO][gG][rR][aA][mM] * [^ ][^ ]* *$\)/To_Keep\1/" \
	-e "s/\(^ *[eE][nN][dD] * [pP][rR][oO][gG][rR][aA][mM] * [^ ][^ ]* *$\)/To_Keep\1/" \
	-e "s/\(^ *contains *$\)/To_Keep\1/" \
	-e "s/\(^ *[pP][uU][bB][lL][iI][cC] *\)/To_Keep\1/" \
	-e "s/\(^.*, *[pP][uU][bB][lL][iI][cC] *\)/To_Keep\1/" \
	-e "s/\(^ *[pP][rR][iI][vV][aA][tT][eE] *\)/To_Keep\1/" \
	-e "s/\(^.*, *[pP][rR][iI][vV][aA][tT][eE] *\)/To_Keep\1/" \
	-e "s/\(^ *[iI][mM][pP][lL][iI][cC][iI][tT] * [nN][oO][nN][eE] *$\)/To_Keep\1/" \
	-e "s/\(^ *[uU][sS][eE] * \)/To_Keep\1/" \
	-e "s/\(^ *[eE][xX][tT][eE][rR][nN][aA][lL] * \)/To_Keep\1/" \
	-e "s/\(^ *integer.*dimension.*&.*\)/To_Keep\1/" \
	-e "s/\(^ *real.*dimension.*&.*\)/To_Keep\1/" \
	-e "s/\(^ *character.*dimension.*&.*\)/To_Keep\1/" \
	tmp > tmpp
    mv tmpp tmp

    
    #-------------------------------------------------------------------------------------------
    # multiples lines
    tst=$( grep -ic "^To_Keep.*& *$" tmp )
    while [ $tst -ne 0 ]
    do
	
	# add "To_Keep" for the line following a line starting with "To_Keep" and ending with "&"
#	sed -e "/^To_Keep.*& *$/{:a ; N; s/ *& *\n\([^T][^o]\)/ \1/ ; ta}" tmp > tmpp
	sed -e "/^To_Keep.*& *$/{:a ; N; /\nTo_Keep/!s/ *& *\n/ / ; ta}" tmp > tmpp
	cmp tmp tmpp > /dev/null ; tst=$?
	# add "To_Keep" on 3rd line if 1st line start with "To_Keep" and ends with "&" and if 2nd line start with "To_Keep *#"	
	awk '
BEGIN { OFS="	\n" }
{
    lines[NR] = $0
}
END {
    for (i = 1; i <= NR; i++) {
        if (match(lines[i], /^To_Keep.*& *$/) && i+2 <= NR && match(lines[i+1], /^To_Keep *#/)) {
            if (!match(lines[i+2], /^To_Keep/)) {
                lines[i+2] = "To_Keep " lines[i+2]
            }
        }
    }
    for (i = 1; i <= NR; i++) {
        print lines[i]
    }
}
' tmpp > tmp
	cmp tmp tmpp > /dev/null ; tst=$(( $tst + $? ))
    done
    
    #-------------------------------------------------------------------------------------------
    # deal with functions, such as :
    #
    #  RECURSIVE FUNCTION nodal_factort( kformula ) RESULT( zf )
    #  REAL(wp) FUNCTION bdy_segs_surf(phu, phv)
    #  IF(lwp) WRITE(numout,*) '   ==>>   Stability functions from Galperin'
    #  IF( 1._wp /= SIGN(1._wp,-0._wp)  )   CALL ctl_stop( 'nemo_ctl: The intrinsec SIGN function follows f2003 standard.',  &
    #  FUNCTION local_/**/XDOPER/**/D/**/OPER/**/_/**/XDIN/**/Din/**/_/**/KIND( ptab, pcumul ) RESULT( pout )
    #  impure ELEMENTAL FUNCTION pres_temp_sclr( pqspe, pslp, pz, ptpot, pta, l_ice )
    
    for nn in $( grep "^To_Keep.*function" tmp | grep -iv "end *function" | sed -e "s/^.*function *\([^(]*\)(.*$/\1/" )
    do
	if [ $( grep -c "^To_Keep.*function * ${nn//\*/\\*} *(.*) *result *(" tmp ) -gt 0 ]
	then
	    nnok=$( grep "^To_Keep.*function * ${nn//\*/\\*} *(.*) *result *(" tmp | sed -e "s/^.*result *( *\([^) ]*\) *).*$/\1/" )
	else
	    nnok=$nn
	fi
	for xx in $nnok
	do
	    xxx=${xx//\*/\\*}   ;   xxx=${xxx//\//\\/}
	    nnn=${nn//\*/\\*}   ;   nnn=${nnn//\//\\/}
	    sed -e "/^To_Keep.*function * ${nnn} *(.*)/,/^To_Keep.*[eE][nN][dD] * function * ${nnn} *$/s/\(^.*::\) *${xxx} *$/To_Keep\1 ${xxx}/"   \
		-e "/^To_Keep.*function * ${nnn} *(.*)/,/^To_Keep.*[eE][nN][dD] * function * ${nnn} *$/s/\(^.*::\) *${xxx} *,.*$/To_Keep\1 ${xxx}/"   \
		-e "/^To_Keep.*function * ${nnn} *(.*)/,/^To_Keep.*[eE][nN][dD] * function * ${nnn} *$/s/\(^.*::\).*, *${xxx} *$/To_Keep\1 ${xxx}/"   \
		-e "/^To_Keep.*function * ${nnn} *(.*)/,/^To_Keep.*[eE][nN][dD] * function * ${nnn} *$/s/\(^.*::\).*, *${xxx} *,.*$/To_Keep\1 ${xxx}/" tmp > tmpp
	    mv tmpp tmp
	done
    done

    #-------------------------------------------------------------------------------------------
    # keep To_Keep lines
    if [ $( grep -c "^To_Keep *contains *$" tmp ) -ne 0 ]
    then
	# keep everything until the first contains
	sed -n -e "1,/^To_Keep *contains *$/p" tmp  > tmpp
	# keep only To_Keep lines from contains
	sed -n -e "/^To_Keep *contains *$/,$ p" tmp | sed -e "1d" | grep "^To_Keep" >> tmpp
	mv tmpp tmp
    else
	if [ $(( $( grep -c "^To_Keep.*function" tmp ) + $( grep -c "^To_Keep.*subroutine" tmp ) )) -gt 0 ]
	then
	    # keep only To_Keep lines
	    grep "^To_Keep" tmp > tmpp
	    mv tmpp tmp
	#else
	    # keep everything
	fi
	if [ $( basename $ff | grep -c "_scheme.h90$" ) -gt 0 ]
	then
	    # keep only To_Keep lines
	    grep "^To_Keep" tmp > tmpp
	    mv tmpp tmp
	fi
    fi

    #-------------------------------------------------------------------------------------------
    # remove To_Keep
    sed -e "s/To_Keep//g" tmp > tmpp
    mv tmpp tmp

    #-------------------------------------------------------------------------------------------
    # set default values
    
    # default definition of ln_* to false.
    sed -e "s/[lL][oO][gG][iI][cC][aA][lL]/LOGICAL/" \
	-e "s/[pP][uU][bB][lL][iI][cC]/PUBLIC/" tmp > tmpp
    mv tmpp tmp
    sed -e "/^ *LOGICAL *, *PUBLIC *:: /s/\(ln_[^= ]*\) *$/\1 = .FALSE./g"    \
	-e "/^ *LOGICAL *, *PUBLIC *:: /s/\(ln_[^= ]*\) *,/\1 = .FALSE.,/g"  tmp > tmpp
    mv tmpp tmp

    # return -1 for all functions
    # h : copy the pattern space into the hold space
    # G : appends the contents of the holding area to the contents of the pattern space
    nb=$( grep -ic "end *function *nf90_" tmp )
    if [ $nb -gt 0 ]
    then
	for nn in $( grep -i "end *function *nf90_" tmp | sed -e "s/.* //" )
	do
	    isint=$( grep -ic "^ *integer.*:: *$nn" tmp )
	    if [ $isint -gt 0 ]
	    then
		sed -e "/^ *[eE][nN][dD] *function *$nn *$/{h;s/^ *[eE][nN][dD] *function *$nn *$/     $nn = 0/;G}" tmp > tmpp
		mv tmpp tmp
	    fi
	    isreal=$( grep -ic "^ *real.*:: *$nn" tmp )
	    if [ $isreal -gt 0 ]
	    then
		sed -e "/^ *[eE][nN][dD] *function *$nn *$/{h;s/^ *[eE][nN][dD] *function *$nn *$/     $nn = 0./;G}" tmp > tmpp
		mv tmpp tmp
	    fi
	    ischar=$( grep -ic "^ *character.*:: *$nn" tmp )
	    if [ $ischar -gt 0 ]
	    then
		sed -e "/^ *[eE][nN][dD] *function *$nn *$/{h;s/^ *[eE][nN][dD] *function *$nn *$/     $nn = 'not used'/;G}" tmp > tmpp
		mv tmpp tmp
	    fi
	done
    fi
    
    if [ "$flist" == "typeSizes.F90" ]
    then
	mv tmp $flist
    else
	mv tmp $( basename $ff .F90 ).h90
    fi

    

    
done

rm -rf netcdf-fortran


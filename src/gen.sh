#!/usr/bin/env bash

# Auto-generate Fortran source from template(s)
#
# When adding a new map type, there are still some manual steps that you need to
# do after running this script:
#
#   - edit map.F90 to `use map_${name}_m`
#   - add map_${name}.F90 to the Makefile
#   - add build rules to the Makefile:
#     * map depends on map_${name}
#     * map_${name} depends on utils
#
# This might not extend to array types, but it's good for primitive scalars at
# least

set -exu

table=()

# Table of str replacements
#
#        name  out_decl                        in_decl            default
table+=('i32' 'integer(kind=4)'               'integer(kind=4)'  '0')
table+=('i64' 'integer(kind=8)'               'integer(kind=8)'  '0')
table+=('str' 'character(len=:), allocatable' 'character(len=*)' '""')

ncols=4
nrows=$(( ${#table[@]} / $ncols ))

for i in $(seq 0 $(( $nrows - 1 )) ) ; do
	echo "$i"

	val_name=${table[$(( $i * $ncols + 0 ))]}
	val_out_=${table[$(( $i * $ncols + 1 ))]}
	val_in__=${table[$(( $i * $ncols + 2 ))]}
	val_dflt=${table[$(( $i * $ncols + 3 ))]}

	ofile="map_${val_name}.F90"
	cp ../template/map.F90 "$ofile"

	sed -i "s/VAL_NAME/$val_name/g" "$ofile"
	sed -i "s/VAL_OUT_/$val_out_/g" "$ofile"
	sed -i "s/VAL_IN__/$val_in__/g" "$ofile"
	sed -i "s/VAL_DFLT/$val_dflt/g" "$ofile"

done


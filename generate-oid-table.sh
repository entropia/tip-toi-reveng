#!/bin/bash

# based on code by @mercedon, this creates a file consisting of 330 OID codes,
# 15 from the range of media codes, the rest from the object code range.
# Printed at 1200 dpi, it comfortably fits on a A4 page.

set -e 

codes="$(seq 42 56) $(seq 4716 5030)"
files=

for i in $codes
do
	echo Creating oid${i}_tile.png
	./tttool oid-code $i
	convert oid-${i}.png -crop 576x576+0+0 +repage oid${i}_tile.png
	convert oid${i}_tile.png -gravity north -stroke none -pointsize 144 -annotate 0 ${i} oid${i}_tile.png
	convert oid${i}_tile.png -shave 2x2 -bordercolor black -compose Copy -border 2 oid${i}_tile.png
	files="$files oid${i}_tile.png"
	rm -f oid-${i}.png
done
echo Creating oid-table.png
montage $files -mode Concatenate -tile 15x oid-table.png
rm -f $files

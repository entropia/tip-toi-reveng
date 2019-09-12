#!/bin/bash

set -o errexit
set -o nounset

LANGUAGES="de_de fr_fr nl_nl it_it ru_ru"

for lang in $LANGUAGES ; do 
   wget -m -nd https://ssl-static.ravensburger.de/db/tiptoi_$lang.xml
   cat tiptoi_$lang.xml | `dirname $0`/download.pl | bash
   #cat tiptoi_$lang.xml | ./download.pl
done

#!/bin/bash

#get all firmwarefiles and gamefiles from static.tiptoi.com

baseurl="http://static.tiptoi.com/db/"
basexmlname="tiptoi_%LANGUAGE%.xml"
languages="de_de fr_fr nl_nl"
xmldir="xml"
upddir="upd"
gmedir="gme"

for language in $languages
do
  IFS=$'\n'
  #create xml-filename for $language
  xmlfile=${basexmlname/"%LANGUAGE%"/$language}

  #download xml-file for $language if newer
  wget -N -P $xmldir $baseurl$xmlfile

  #find firmware-files for $language
  updfiles=`grep -o 'http://.*\.upd' $xmldir/$xmlfile`

  #download firmware-files for $language if newer
  for updfile in $updfiles
  do
    wget -N -P $upddir/$language $updfile
  done

  #find game-files for $language
  gmefiles=`grep -o 'http://.*\.gme' $xmldir/$xmlfile`

  #download game-files for $language if newer
  for gmefile in $gmefiles
  do
    wget -N -P $gmedir/$language $gmefile
  done

done
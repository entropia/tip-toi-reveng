#!/usr/bin/perl

use strict;
use warnings;
use diagnostics;

use XML::Simple;

use Data::Dumper;

my $data = XMLin('-');

    print "mkdir -p \"Firmware/1/".$data->{firmware}->{firmware1}->{version}."\"\n";
    print "pushd \"Firmware/1/".$data->{firmware}->{firmware1}->{version}."\"\n";
    print "wget -m -nd \"".$data->{firmware}->{firmware1}->{path}."\"\n";
    print "popd\n";
    print "mkdir -p \"Firmware/2/".$data->{firmware}->{firmware2}->{version}."\"\n";
    print "pushd \"Firmware/2/".$data->{firmware}->{firmware2}->{version}."\"\n";
    print "wget -m -nd \"".$data->{firmware}->{firmware2}->{path}."\"\n";
    print "popd\n";

    print "mkdir -p \"Software/".$data->{software}->{version}."\"\n";
    print "pushd \"Software/".$data->{software}->{version}."\"\n";
    print "wget -m -nd \"".$data->{software}->{fileWin}."\"\n";
    print "wget -m -nd \"".$data->{software}->{fileMac}."\"\n";
    print "popd\n";

#print Dumper $data;

while (my ($name, $ref) = each(%{$data->{products}->{product}})) {
    my $dir = sprintf("%03d/%05d - %s",$ref->{gameFile}->{id},$ref->{id},$name);
    $dir =~ s/\`//;
    print "mkdir -p \"".$dir."\"\n";
    print "pushd \"".$dir."\"\n";
    print "wget -m -nd \"".$ref->{image}."\"\n";
    print "mkdir -p \"".$ref->{gameFile}->{version}."\"\n";
    print "pushd \"".$ref->{gameFile}->{version}."\"\n";
    print "wget -m -nd \"".$ref->{gameFile}->{path}."\"\n";
    print "popd\n";
    print "popd\n";

}

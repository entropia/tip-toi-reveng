#/bin/env python3
#
# compile and decompile Anyka/Chomptech firmware voice files
# 
# (c) 2015 Ulrich Sibiller

# use print a a function
from __future__ import print_function

import struct
import mmap
import argparse
import sys
import os

def error(*objs):
    print("ERROR:", *objs, file=sys.stderr)

def decompile(infilename, outbasename, force=False, quiet=False):
    offsets = []

    if not quiet:
        print("extracting from %s ..." % infilename)
    # open input file
    infile = open(infilename, 'r+b')
    # memory map input file     
    inmap = mmap.mmap(infile.fileno(), 0)

    # read first offset, use it to calculate
    # the number of contained offsets
    data = infile.read(4) 
    first_offset = struct.unpack('<I', data)[0]
    num = int(first_offset / 4) - 1
    offsets.append(first_offset)    

    # first read all offsets, we read one offset more
    # which points to EOF (= filename)
    for i in range(1, num + 1):
        data = infile.read(4) 
        # treat input as 32 bit value (little endian)
        # and append to offset list 
        offsets.append(struct.unpack('<I', data)[0])

    # last offset is EOF / length of file, 
    for i in range(0, len(offsets) - 1):
        outfilename = '%s_%02.2d.wav' % (outbasename, i)
        if not force:
            if os.path.exists(outfilename):
                error("file %s already exists - aborting" % outfilename)
                sys.exit(1)

        if not quiet:
            print("  writing 0x%8.8x - 0x%8.8x to %s ..." % (offsets[i], offsets[i+1], outfilename))
        inmap.seek(offsets[i])
        data = inmap.read(offsets[i+1] - offsets[i])
        with open(outfilename, 'w+b') as outfile:
            outfile.write(data)
            outfile.close()

    inmap.close()
    infile.close()
    if not quiet:
        print("done.")


def compile(inbasename, outfilename, num, force=False, quiet=False):
    offsets = []
    dataOffset = 4 * (num + 1)

    if not force:
        if os.path.exists(outfilename):
            error("file %s already exists - aborting" % outfilename)
            sys.exit(1)

    # open input file
    if not quiet:
        print("creating %s ..." % outfilename)
    outfile = open(outfilename, 'w+b')

    # seek to first data offset 
    outfile.seek(dataOffset)

    # write all files, one after the other
    # and store their offset
    # currently no padding to even adresses
    # is done - unsure if needed
    for i in range(0, num):
        #infilename = 'Chomp_Voice_%02.2d.wav' % i
        infilename = '%s_%02.2d.wav' % (inbasename, i)
        with open(infilename, 'r+b') as infile:
            data = infile.read()
            infile.close()

        if len(data) == 0:
            error("file size of %s is zero - aborting" % infilename)
            outfile.close()
            os.unlink(outfilename)
            sys.exit(1)

        pos = outfile.tell()
        if not quiet:
            print("  inserting %s at 0x%08.8x ..." % (infilename, pos))
        offsets.append(pos)
        outfile.write(data)

    # last offset is filelength, store that
    offsets.append(outfile.tell())

    # seek  to start of file
    outfile.seek(0)

    if not quiet:
        print("  inserting offsets ...")
    # write all offsets including filelength to outfile 
    for i in offsets:
        data = struct.pack('<I', i)
        outfile.write(data)

    outfile.close()
    if not quiet:
        print("done.")


# configure command line parsing
parser = argparse.ArgumentParser(description='Compile/decompile Anyka/Chomptech firmware voice files')
parser.add_argument('-f', '--force', action='store_true', default=False, help='allow overwriting files')
parser.add_argument('-q', '--quiet', action='store_true', default=False, help='do not output any text except errors')
subparsers = parser.add_subparsers(help='subcommands', dest="subparser_name")

parser_compile = subparsers.add_parser('compile')
parser_compile.add_argument('--numwavs', type=int, default=48, help='number of wav files (default: %(default)d)')
parser_compile.add_argument('inputbasename', type=str, 
                   help='a basename for the input wav files, "_nn.wav" will be appended')
parser_compile.add_argument('binfile', nargs='?', type=str, help='the name of the output binfile (default: <inputbasename>.bin)')

parser_decompile = subparsers.add_parser('decompile')
parser_decompile.add_argument('binfile', nargs='?', type=str, default='Chomp_Voice.bin', help='the name of the input binfile (default: %(default)s)')
parser_decompile.add_argument('outputbasename', nargs='?', type=str, 
                              help='a basename for the output wav files, "_nn.wav" will be appended (default: derived from <binfile>)')
try:
  args = parser.parse_args()
except:
  sys.exit(1)  

if args.subparser_name == "compile":
    if args.binfile:
        out = args.binfile
    else:
        out = "%s.bin" % args.inputbasename
    compile(args.inputbasename, out, args.numwavs, args.force, args.quiet)
elif args.subparser_name == "decompile":
    if args.outputbasename:
        out = args.outputbasename
    else:
        out = args.binfile.split(".bin")[0]
    decompile(args.binfile, out, args.force, args.quiet)
else:
    parser.print_help()


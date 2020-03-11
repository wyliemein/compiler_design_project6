#!/bin/bash

# limit.sh TIME MEM CMD
#   TIME  The amount of CPU time in seconds.
#   MEM   The amount of memory in MB.
#   FSIZE The amount of file size in MB.
#   CMD   The command to be run.


# check arguments
test $# -lt 4 && { echo "$0 requires time, memory, fsize, and command" >&2 ; exit 1 ; }

# retrieve time and memory prarameters
tm=$1 ; shift
mem=$(($1 * 1024)) ; shift
fsize=$(($1 * 1024)) ; shift

ulimit -t$tm -m$mem -f$fsize
"$@"

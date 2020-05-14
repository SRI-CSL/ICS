#!/bin/sh
#
# The MIT License (MIT)
#
# Copyright (c) 2020 SRI International
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#

ICS=$1

#### Configuration #####
MAX_MEMORY=450000
MAX_STACK=40000
MAX_TIME=3600
########################

ulimit -t $MAX_TIME
ulimit -v $MAX_MEMORY
ulimit -s $MAX_STACK

CURR_DIR=`pwd`
DATE=`date`

RESULT_FILE=$CURR_DIR/regression-result.txt

echo "ICS Regression Test $DATE, MAX_MEMORY=$MAX_MEMORY, MAX_STACK=$MAX_STACK, MAX_TIME=$MAX_TIME" > $RESULT_FILE

for name in `ls regression/*.ics`; do
    echo "Executing $name ...";
    echo -n "${name//\.ics/} " >> $RESULT_FILE;
    rm -f /tmp/time.ics
    rm -f /tmp/result.ics
    if time -o /tmp/time.ics $ICS $name 2> /dev/null > /tmp/result.ics; then
	if grep -n -e ":unsat" /tmp/result.ics 2> /dev/null > /dev/null; then
	    RES="UNSAT"
	else
	    if grep -n -e ":sat" /tmp/result.ics 2> /dev/null > /dev/null ; then
		RES="SAT"
	    else
		RES="UNEXPECTED"
            fi
	fi
	TIME=`get-time.sh /tmp/time.ics`
	echo "$1 $RES $TIME" >> $RESULT_FILE
    else
	TIME=`get-time.sh /tmp/time.ics`
	echo "$1 KILLED $TIME" >> $RESULT_FILE
    fi
done

mail ruess <<EOF
Finished ICS regression test
EOF
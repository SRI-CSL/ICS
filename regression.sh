#!/bin/sh

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
		echo "$1 $RES $TIME"
  else
		TIME=`get-time.sh /tmp/time.ics`
		echo "$1 KILLED $TIME"
  fi
done

mail ruess <<EOF
Finished ICS regression test
EOF
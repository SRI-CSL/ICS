#! /bin/sh

if grep -e "user" $1 2> /dev/null > /dev/null; then
		EXEC_TIME=`grep -e "user" $1 | awk '{print $1}'`
		EXEC_TIME=${EXEC_TIME//user/}
		echo $EXEC_TIME
else
		echo "NOT_AVAILABLE"
fi
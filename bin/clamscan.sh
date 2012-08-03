#!/bin/sh -eu

result="`clamscan --no-summary -ri /home/kui`"
line_num="`echo -n "result" | wc -l`"

[ $line_num -ne 0 ] && echo "$result" > $HOME/Desktop/clamscan_result.txt


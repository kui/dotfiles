#!/bin/bash

set -u

# x days old files
DATE_BEFORE=14

find /var/share/tv_recorded -mtime +$DATE_BEFORE -name '*.m2ts' -exec rm -v {} \;

#!/bin/bash

cd /home/chadbraunduin/.emacs.d/backups
for backup in *.*~
do
    orig_file=$backup
    orig_file="${orig_file//!//}"
    orig_file="${orig_file/%[.][~][0-9]*[~]/}"
    orig_file="${orig_file/%[~]/}"
    if [ ! -f $orig_file ]
    then
	echo "$backup"
    fi
done

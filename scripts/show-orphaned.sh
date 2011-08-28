#!/bin/bash

backups_directory="/home/chadbraunduin/.emacs.d/backups"
cd "$backups_directory"
for backup in *.*~
do
    orig_file=$backup
    orig_file="${orig_file//!//}"
    orig_file="${orig_file/%[.][~][0-9]*[~]/}"
    orig_file="${orig_file/%[~]/}"
    if [ ! -f $orig_file ]
    then
			echo "$backups_directory/$backup"
    fi
done

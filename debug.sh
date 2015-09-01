#!/usr/bin/env bash
while true
do
    mono --debug src/splashy/bin/Debug/splashy.exe
    if [ $? -ne 0 ]
    then
        break
    fi
done

#/usr/bin/sh

# script that creates folder `submit` with all files to submit.

mkdir submit &> /dev/null # ignore possible error that folder exists

cp src/login.b submit/
cp src/cpu.vhd submit/
cp xstigl00.png submit/

cd test
make > ../submit/log.txt

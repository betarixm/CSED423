#!/bin/sh

rm pa3_20190084.tar

mkdir pa3;
mkdir pa3/src;

cp -r cool-support pa3/;

cp src/cool-tree.h src/cool-tree.handcode.h src/semant.cc src/semant.h src/Makefile src/*.cl pa3/src/

echo '1' >> pa3/src/README

tar -cvf pa3_20190084.tar pa3;

rm -r pa3;

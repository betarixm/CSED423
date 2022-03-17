#!/bin/sh

rm pa2_20190084.tar

mkdir pa2;
mkdir pa2/src;

cp -r cool-support pa2/;

cp src/cool.y src/cool.flex src/Makefile src/*.cl pa2/src/

echo '2' >> pa2/src/README

tar -cvf pa2_20190084.tar pa2;

rm -r pa2;

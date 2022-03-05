#!/bin/sh

mkdir pa1;
cp -r cool-support pa1/;
cp -r src pa1/;

tar -cvf pa1_20190084.tar pa1;

rm -r pa1;

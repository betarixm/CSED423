#! /bin/bash
for f in $(ls *.out *.refout | sed 's/\.\(\|ref\)out$//' | sort | uniq); do
  diff -u $f.out $f.refout;
done

#!/bin/bash

for i in {1..6}
do
  ./out/compiler/duna ./problems/happy-path/problem$i.duna
	gcc -o ./out/problem$i ./out/duna.c
  echo "Executing problem $i"
  ./out/problem$i
done
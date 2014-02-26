#!/bin/bash

echo "C test:" > Results.txt
for i in {1..5}
do
  rm primec
  { time ./TestC.sh ; } 2>> Results.txt
  echo "" >> Results.txt
done

echo "C test with -O2:" >> Results.txt
for i in {1..5}
do
  rm primec
  { time ./TestC2.sh ; } 2>> Results.txt
  echo "" >> Results.txt
done

echo "Haskell test:" >> Results.txt
for i in {1..5}
do
  rm *.hi *.o primehs
  { time ./TestHs.sh ; } 2>> Results.txt
  echo "" >> Results.txt
done

echo "Haskell test with -O2:" >> Results.txt
for i in {1..5}
do
  rm *.hi *.o primehs
  { time ./TestHs2.sh ; } 2>> Results.txt
  echo "" >> Results.txt
done

echo "E test:" >> Results.txt
for i in {1..5}
do
  { time ./TestE.sh ; } 2>> Results.txt
  echo "" >> Results.txt
done

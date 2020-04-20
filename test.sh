#!/bin/bash
# Projekt: PLG-2-NKA
# Login:   xwilla00
# Autor:   Tomas Willaschek
# Rok:     2020

test_count=14
err_test_count=5

make > /dev/null

echo "Testing valid input"
for i in $(seq 1 $test_count)
do
  printf $i" "
  for j in {1..2}
  do
    # test read file
    ./plg-2-nka -"${j}" test/test"${i}".in > output
    diff output test/test"${i}"-"${j}".out &> /dev/null
    if [ $? -eq 0 ]
    then 
      printf "."
    else
      printf "F"
    fi
    # test read stdin

    cat test/test"${i}".in | ./plg-2-nka -"${j}" > output
    diff output test/test"${i}"-"${j}".out &> /dev/null
    if [ $? -eq 0 ]
    then 
      printf "."
    else
      printf "F"
    fi
  done
  printf "\n"
done

printf "Testing error input\n"
for i in $(seq 1 $err_test_count)
do
  printf $i" "
  for j in {1..2}
  do
    ./plg-2-nka -"${j}" test/err"${i}".in &>/dev/null
    if [ $? -eq 0 ]
    then
      printf "F" 
    else
      printf "."
    fi
  done
  printf "\n"
done

rm output

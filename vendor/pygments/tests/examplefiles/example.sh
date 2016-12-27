#!/bin/bash

printf "%d %s\n" 10 "foo"
printf "%d %s\n" $((10#1)) "bar"

let "m = 10#${1:1:2}"
echo $m

m=$((10#${1:4:3} + 10#${1:1:3}))
echo $m

m=$((10#${1:4:3}))
echo $m

m=$((10#$1))
echo $m

m=$((10#1))
echo $m

m=$((10))
echo $m

#!/bin/sh

mkdir tre_modules
for i in shared; do \
    git clone --depth 1 http://github.com/SvenMichaelKlose/tre-$i.git tre_modules/$i; \
done

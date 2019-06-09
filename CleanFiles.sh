#!/bin/bash

# I had this as one line, but that made it a little difficult to follow
# I wouldn't be surprised if there was a better way to accomplish this

find . -type d -name CMakeFiles -exec rm -rf {} \;
find . -type d -name mod -exec rm -rf {} \;
find . -type d -name bin -exec rm -rf {} \;
find . -type d -name lib -exec rm -rf {} \;
find . -type f -name Makefile -exec rm -rf {} \;
find . -type f -name cmake_install.cmake -exec rm -rf {} \;
find . -type f -name CMakeCache.txt -exec rm -rf {} \;
find . -type f -name hyper.h5 -exec rm -rf {} \;

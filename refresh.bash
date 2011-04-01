#!/bin/bash

. src/config.bash

mkdir -p bin obj

make -f src/top.make bin/lispin bin/server

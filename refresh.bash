#!/bin/bash

. src/config.bash

make -f src/top.make bin/lispin bin/server

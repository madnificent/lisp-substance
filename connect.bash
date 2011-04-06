#!/bin/bash

. src/config.bash

exec bin/server -a $ADDICT_IP_ADDR

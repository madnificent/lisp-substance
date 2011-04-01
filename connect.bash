#!/bin/bash

. src/config.bash

palm-package -o /tmp $ADDICT_DIR
palm-install $ADDICT_IPK
echo will launch $ADDICT in one second
(sleep 1; palm-launch $ADDICT) >/dev/null 2>&1 &
bin/server -a $ADDICT_IP_ADDR

#!/bin/bash

. src/config.bash

palm-package -o /tmp $ADDICT_DIR
palm-install /tmp/$ADDICT_IPK
echo will launch $ADDICT in one second
(sleep 1; palm-launch $ADDICT) >/dev/null 2>&1 &
exec bin/server -a $ADDICT_IP_ADDR

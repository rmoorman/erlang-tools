#!/usr/bin/env sh

currentWorkingDirectory=`dirname $0`
cd $currentWorkingDirectory

erlc -pa ../../ebin -v -o . ws_echo.erl

cat > yaws.conf <<EOF
# Yaws configuration.

cache_refresh_secs = 0

ebin_dir = ../../ebin

<server localhost>
  port = 8080
  listen = 0.0.0.0
EOF

echo "  docroot = `pwd`" >> yaws.conf

cat >> yaws.conf <<EOF
  dir_listings = true
  appmods = </events, websocket_mod>
  <opaque>
    websocket_handler = ws_echo
  </opaque>
</server>
EOF


yaws --conf yaws.conf -i --id ws_echo 


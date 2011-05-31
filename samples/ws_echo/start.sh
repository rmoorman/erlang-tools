#!/usr/bin/env sh

currentWorkingDirectory=`dirname $0`
cd $currentWorkingDirectory

erlc -pa ../../ebin -v -o . ws_echo.erl

cat > yaws.conf <<EOF
# Yaws configuration.

cache_refresh_secs = 0

auth_log = true
ebin_dir = ../../ebin

# TODO: Make log_resolve_hostname true on Linux, false on Windows.
# TODO: Report to the Yaws author the DNS wait, which should not happen.
log_resolve_hostname = false

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


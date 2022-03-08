#!/bin/bash

set -euo pipefail

echo -n "ensuring gdb_codesign cert exists... "
security find-certificate -c gdb_codesign > /dev/null 2>&1
if (( $? != 0 )); then
    echo "gdb_codesign cert not found, run ./macos-setup-codesign.sh first" 1>&2
    exit 1
fi
echo "ok"

echo -n "ensuring gdb_codesign cert is not expired... "
security find-certificate -p -c gdb_codesign | openssl x509 -checkend 0
if (( $? != 0 )); then
    echo "gdb_codesign cert expired." 1>&2
    echo "remove and re-run ./macos-setup-codesign.sh" 1>&2
    exit 1
fi

echo -n "creating gdb entitlement... "
cat <<EOF > /tmp/gdb-entitlement.xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>com.apple.security.cs.debugger</key>
  <true/>
</dict>
</plist>
EOF
echo "done"

echo -n "codesigning gdb... "
codesign --entitlements /tmp/gdb-entitlement.xml -fs gdb_codesign $(which gdb)
echo "done"

echo "restarting taskgated service... "
sudo killall taskgated
echo "done."

echo -n "verifying gdb is codesigned... "
codesign -vv $(which gdb)

echo "gdb is codesigned"

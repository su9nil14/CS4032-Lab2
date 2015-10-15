#!/usr/bin/python

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
python "$DIR/client.py"
python "$DIR/server.py"
rm -rf "$DIR/bin"
mkdir -p "$DIR/bin"
if [ -f "$DIR/client" ]
then
    mv "$DIR/client" "$DIR/bin"
fi
if [ -f "$DIR/server" ]
then
    mv "$DIR/server" "$DIR/bin"
fi

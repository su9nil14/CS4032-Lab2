#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ghc "$DIR/Client.hs"
ghc "$DIR/Server.hs"
rm -rf "$DIR/bin"
mkdir -p "$DIR/bin"
if [ -f "$DIR/Client" ]
then
    mv "$DIR/Client" "$DIR/bin"
fi
if [ -f "$DIR/Server" ]
then
    mv "$DIR/Server" "$DIR/bin"
fi
find "$DIR" -name "*.o" -o -name "*.hi" | xargs -I{} rm {}
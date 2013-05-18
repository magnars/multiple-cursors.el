#!/bin/sh -e
ECUKES=$(find elpa/ecukes-*/ecukes | tail -1)
carton exec "$ECUKES" "$@"

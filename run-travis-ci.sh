#!/bin/sh

cd "$(dirname "$0")"

set_default () {
  eval "
if [ -z \$$1 ]; then
  $1=$2
fi
"
}

set_default EMACS "$(which emacs)"

echo "*** Emacs version ***"
echo "EMACS =" $(which $EMACS)
$EMACS --version
echo

exec ./util/ecukes/ecukes --graphical

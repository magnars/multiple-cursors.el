#!/bin/sh

cd "$(dirname "$0")"

set_default () {
  eval "
if [ -z \$$1 ]; then
  $1=$2
fi
"
}

set_default ECUKES_EMACS "$(which emacs)"

echo "*** Emacs version ***"
echo "ECUKES_EMACS =" $(which $ECUKES_EMACS)
$ECUKES_EMACS --version
echo

exec ./util/ecukes/ecukes --graphical

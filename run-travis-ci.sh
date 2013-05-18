#!/bin/sh -e

cd "$(dirname "$0")"

ECUKES_EMACS=${EMACS:-$(which emacs)}
export ECUKES_EMACS

echo "*** Emacs version ***"
echo "ECUKES_EMACS = $ECUKES_EMACS"
"$ECUKES_EMACS" --version
echo

exec ./run-tests.sh $TAGS

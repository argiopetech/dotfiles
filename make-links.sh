#!/bin/bash

pushd files

find . -type d -print0 | tail -zn +2 | sed -z "s|^\./||" | xargs -0 -i_ mkdir -p ${HOME}/._

for f in `git ls-files`; do
    ln -s $(readlink -f ${f}) ${HOME}/.${f};
done

popd

#!/usr/bin/env bash

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"
cd "$cwd/.."
f=`mktemp -d`
git clone git@github.com:meoblast001/ext2-info.git "$f/ext2-info.git"
cabal haddock
pushd "$f/ext2-info.git"
  git checkout gh-pages && git rm -rf *
popd
mv dist/doc/html/ext2-info/* "$f/ext2-info.git/"
pushd "$f/ext2-info.git"
  git add -A
  git commit -m "Manual docs deploy."
  git push origin gh-pages
popd
rm -rf "$f"

if [ $? == 0 ]; then
  echo "*** Done: https://meoblast001.github.io/ext2-info/"
  exit 0
else
  echo "*** ERROR!!! Fix the above and try again."
  exit 1
fi

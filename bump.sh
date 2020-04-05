#!/usr/bin/env bash

set -e

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
site="$dir/result/bin/site"
siteResult="$dir/site-result"
_site="$dir/_site"

pushd "$dir"

nix-build release.nix # Now we have fresh binary
"$site" clean
"$site" build # Now we have fresh _site
rm -rf "$siteResult"/* # clean everything just in case
cp -r "$_site"/* "$siteResult"

cd "$siteResult"
git add .
git commit -m 'commited by script'
git push

cd "$dir"
git add site-result
git commit -m 'submodule updated'

popd

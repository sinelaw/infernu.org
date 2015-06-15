#!/bin/bash -eu
cabal build
site=./dist/build/site/site
$site clean && PATH=$PATH:node_modules/.bin $site build

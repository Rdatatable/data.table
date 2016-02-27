#!/bin/bash

set -o errexit -o nounset

addToDrat(){
  PKG_REPO=$PWD
  PKG_TARBALL=$(ls -1t *.tar.gz | head -n 1)

  cd ..; mkdir drat; cd drat

  ## Set up Repo parameters
  git init
  git config user.name "addToDrat"
  git config user.email "<>"
  git config --global push.default simple

  ## Get drat repo
  git remote add upstream "https://$GH_TOKEN@github.com/Rdatatable/data.table.git"
  git fetch upstream 2>err.txt
  git checkout gh-pages

  Rscript -e "drat::insertPackage('$PKG_REPO/$PKG_TARBALL', \
    repodir = '.', \
    commit='Travis update: build $TRAVIS_BUILD_NUMBER')"
  git push
  
}

addToDrat

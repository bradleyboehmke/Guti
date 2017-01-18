#!/bin/bash

usage() {
 echo "Create a new Shiny application in the analytics directory."
 echo "A single argument <application name> is required"
}

create-app () {
  if [ "$#" -ne 1 ]; then
    usage 
    return 1
  fi

  git checkout -b $1

  mkdir -p analytics/$1
  cp -r template/* analytics/$1

  sed -i.bak -e 's@~~analyticName~~@'$1'@' analytics/$1/config.R
  rm -f analytics/$1/config.R.bak

  git add analytics/$1
  git commit -m "Initial commit for ${1}"
}

create-app "$@"

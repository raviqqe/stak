#!/bin/sh

set -e

while getopts l: option; do
  case $option in
  l)
    if ! echo $OPTARG | grep ^/ >/dev/null; then
      OPTARG=./$OPTARG
    fi

    libraries="$libraries -l $OPTARG"
    ;;
  esac
done

shift $(expr $OPTIND - 1)

gosh $libraries "$@"

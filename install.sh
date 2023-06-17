#!/bin/sh

CDPATH='' cd -- "$(dirname -- "${0}")" || exit 1

git pull --quiet origin main

rsync --exclude ".git/" \
      --exclude "README.md" \
      --exclude "LICENSE" \
      --exclude "install.sh" \
      -avh --no-perms ./ ~/${0}
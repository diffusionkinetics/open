#!/usr/bin/env bash
NEW_LTS="10.10"

find . -name 'stack.yaml' -exec sed -i -e "s/lts-[0-9]\+\.[0-9]\+$/lts-${NEW_LTS}/" {} \;

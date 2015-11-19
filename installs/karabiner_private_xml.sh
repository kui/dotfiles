#!/bin/bash

set -xue

base="$(cd $(dirname $0); pwd)"

ln -sf "$base/private.xml" "$HOME/Library/Application Support/Karabiner"

#!/bin/sh
set -e
idris2 --build ia.ipkg --codegen node

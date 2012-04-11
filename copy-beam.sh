#!/bin/sh

dir=$1

if [ -d "$dir" ]; then
    find . -name "*.beam" -not -wholename "./beam-*" -exec cp \{\} $dir \;
else
    echo "Usage: $0 <directory>"
fi

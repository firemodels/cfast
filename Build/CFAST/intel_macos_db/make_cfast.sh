#!/bin/bash
set -e

dir=$(pwd)
target=${dir##*/}
clean_cfast=false

while [[ $# -gt 0 ]]; do
    case "$1" in
        --clean-cfast)
            clean_cfast=true
            ;;
        -h|--help)
            echo "Usage: ./make_cfast.sh [--clean-cfast]"
            echo "  --clean-cfast  Remove object and module files before building."
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            exit 1
            ;;
    esac
    shift
done

if [[ "$clean_cfast" == true ]]; then
    echo "Option --clean-cfast is set."
    make -f ../makefile clean
fi

echo "Building $target"
make -f ../makefile "$target"

../../../Utilities/scripts/md5hash.sh  cfast8_macos_db

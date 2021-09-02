#!/bin/bash


RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

function check_installed() {
	if [ -z "$(which ${1})" ] && [ ! -f "/bin/${1}" ]; then
                echo "false"
        else
                echo "true"
	fi
}

echo '-------------- building release binary --------------'

cd $(if [ ! -z "$WORKSPACE" ]; then echo "$WORKSPACE"; else echo "."; fi)

if [ "$(check_installed cabal)" = "true" ]; then
    cabal new-build -O2 --disable-debug-info --enable-executable-stripping --enable-library-stripping --disable-debug-info --disable-library-for-ghci --enable-split-sections
else
    echo -e "${RED}[fatal]${NC}: cabal executable not found, cannot proceed"; exit
fi

if [ "$(check_installed strip)" == "true" ]; then
    strip dist-newstyle/build/x86_64-linux/ghc-*/axolotl-*/x/axl/opt/build/axl/axl
else
    echo -e "${YELLOW}[warn]${NC}: 'strip' not found in PATH, not stripping executable"
fi

cp dist-newstyle/build/x86_64-linux/ghc-*/axolotl-*/x/axl/opt/build/axl/axl axl
echo "${GREEN}[success]${NC}: release binary saved to 'axl'!"


#!/usr/bin/env bash
pushd src/antlr
java -jar ../../lib/antlr.jar -no-listener -visitor Diggleby.g4
popd
rm -rf src/antlr/.antlr

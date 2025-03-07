#!/usr/bin/env bash
javac -d out -cp "out:lib/antlr.jar" src/antlr/*.java
javac -d out -cp "out:lib/antlr.jar" src/ast/*.java
javac -d out -cp "out:lib/antlr.jar" src/*.java

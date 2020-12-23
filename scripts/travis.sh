#!/bin/bash

cd ../exercises

cabal v2-build

cabal v2-exec runhaskell Set1Test.hs

#!/bin/bash
reset
# stack clean --full
# stack haddock
stack build --force-dirty --ghc-options=-fforce-recomp


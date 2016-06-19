# Tile generator for floors :p 

.. should the handyman want random

## Build

    cabal configure && cabal build

## Run w unix colors

    tile-gen 3 3 20 20 color

## Generate SVG

    tile-gen 3 2 25 75 bw | 2svg

## TODO

  -  move summary into sep. bin
  -  move colorize into sep. bin

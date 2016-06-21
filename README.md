# Tile generator for floors :p 

.. should the handyman want random (GNU)

It will generate a floor (rows and cells) of tiles that span either 1 or 2 cells.

![floor sample in svg](https://dynnamitt.github.io/swatchd.svg#2)

Max 10 colors. 

Dbl tiles will sometimes be used at end of row since we have invented THE SAW.

## Build

    cabal configure && cabal build

## Run w unix colors ( ESC[ )

    usage: tile-gen <colors> <span-odds> <floor-w> <floor-h>

    $ ./tile-gen 3 3 20 20

## Generate SVG

    $ ./tile-gen 4 1 24 48 | tee /dev/stderr | ./2svg > my-floor.svg

## TODO

  - moneySaver func (merge small cell into dbl if siblings)
  - summary XSLT (after floor have been optimized MANUALLY in Inkscape :)

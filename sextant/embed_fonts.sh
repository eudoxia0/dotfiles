#!/usr/bin/env bash
# usage: embed_fonts.sh <input.pdf> <output.pdf>

gs -dCompatibilityLevel=1.4 \
   -dPDFSETTINGS=/screen \
   -dCompressFonts=true \
   -dSubsetFonts=true \
   -dNOPAUSE \
   -dBATCH \
   -sDEVICE=pdfwrite \
   -sOutputFile=$2 \
   -c ".setpdfwrite <</NeverEmbed [ ]>> setdistillerparams" \
   -f $1

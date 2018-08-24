#!/usr/bin/env bash
# usage: embed_fonts.sh <input.pdf> <output.pdf>

gs -dNOPAUSE \
   -dBATCH \
   -sDEVICE=pdfwrite \
   -dPDFSETTINGS=/prepress \
   -dEmbedAllFonts=true \
   -sOutputFile="$2" \
   -f "$1"

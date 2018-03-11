#!/bin/sh
NUMBER=$(pdfinfo main.pdf | grep Pages | tr -dc '0-9')
pdftk main.pdf cat 2-$NUMBER output out_rmheader.pdf
pdftk Frontespizio.pdf out_rmheader.pdf cat output DAINVIARE.pdf

#!/bin/bash
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 \
   -dPDFSETTINGS=/ebook \
   -sOutputFile=../FuzzyNumbers-Tutorial_current.pdf \
   FuzzyNumbers-Tutorial.pdf

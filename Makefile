## ------------------------------------------------------------------------ ##
#                                                                            #
#   Makefile for 'vtk-haskell'.                                              #
#                                                                            #
#   Author(s) : Patrick Suggate <patrick.suggate@gmail.com>                  #
#   Date      : 18/04/2022                                                   #
#   Copyright : Patrick Suggate, 2022                                        #
#                                                                            #
## ------------------------------------------------------------------------ ##

all:	doc build

# Source Markdown files:
MD	:= $(wildcard *.md)

# Top-level documents:
PDF	:= $(MD:.md=.pdf)

# Pandoc settings:
FLT	?= --filter=pandoc-include --filter=pandoc-fignos
OPT	?= --number-sections --citeproc
SRC	?= markdown+tex_math_double_backslash

%.pdf: %.md
	@pandoc $(FLT) $(OPT) -f $(SRC) -t latex -V papersize:a4 $< -o $@

.PHONY:	doc build out clean test
doc:	$(PDF)

out:
	@mkdir -p ../out

build:	out
	@stack build

.PHONY:	test
test:
	@stack test

#
#  Cleaning
# -------------------------------------------------------------------------- #
clean:
	@rm -f $(PDF)
	@rm -f ../out/*

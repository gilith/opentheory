###############################################################################
# OPENTHEORY VERILOG MAKEFILE
# Copyright (c) 2013 Joe Leslie-Hurd, distributed under the MIT license
###############################################################################

.SUFFIXES:

###############################################################################
# The default action.
###############################################################################

.PHONY: default
default: iverilog

###############################################################################
# Cleaning temporary files.
###############################################################################

TEMP = \
  a.out \
  *.o

.PHONY: clean
clean:
	@echo
	@echo '+------------------+'
	@echo '| Clean everything |'
	@echo '+------------------+'
	@echo
	rm -f $(TEMP)

###############################################################################
# Building using Icarus Verilog.
###############################################################################

IVERILOG = iverilog

VVP = vvp

TARGETS = \
  compressor2.o

.PHONY: iverilog-info
iverilog-info:
	@echo
	@echo '+-------------------------------------+'
	@echo '| Build and test the verilog examples |'
	@echo '+-------------------------------------+'
	@echo

.PHONY: iverilog
iverilog: iverilog-info $(TARGETS)

%.o: %.v
	$(IVERILOG) -o $@ $<
	$(VVP) $@

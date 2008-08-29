###############################################################################
# OPEN THEORY MAKEFILE
# Copyright (c) 2005-2007 Joe Hurd, distributed under the GNU GPL version 2
###############################################################################

.SUFFIXES:

###############################################################################
# The default action.
###############################################################################

.PHONY: default
default: mosml

###############################################################################
# Cleaning temporary files.
###############################################################################

TEMP = $(MOSML_TARGETS) \
       bin/mosml/*.sml bin/mosml/*.ui bin/mosml/*.uo bin/mosml/a.out \
       $(MLTON_TARGETS) \
       bin/mlton/*.sml bin/mlton/*.mlb

.PHONY: clean
clean:
	@echo
	@echo '********************'
	@echo '* Clean everything *'
	@echo '********************'
	@echo
	rm -f $(TEMP)
	$(MAKE) -C test $@

###############################################################################
# Testing.
###############################################################################

.PHONY: test
test:
	$(MAKE) -C test

###############################################################################
# Source files.
###############################################################################

SRC = \
  src/Useful.sig src/Useful.sml \
  src/Lazy.sig src/Lazy.sml \
  src/Ordered.sig src/Ordered.sml \
  src/Set.sig src/RandomSet.sml src/Set.sml \
  src/ElementSet.sig src/ElementSet.sml \
  src/Map.sig src/RandomMap.sml src/Map.sml \
  src/KeyMap.sig src/KeyMap.sml \
  src/Sharing.sig src/Sharing.sml \
  src/Stream.sig src/Stream.sml \
  src/Parser.sig src/Parser.sml \
  src/MetisName.sig src/MetisName.sml \
  src/Namespace.sig src/Namespace.sml \
  src/Name.sig src/Name.sml \
  src/Type.sig src/Type.sml \
  src/TypeSubst.sig src/TypeSubst.sml \
  src/Var.sig src/Var.sml \
  src/Term.sig src/Term.sml \
  src/TermSubst.sig src/TermSubst.sml \
  src/Sequent.sig src/Sequent.sml \
  src/Thm.sig src/Thm.sml \
  src/Syntax.sig src/Syntax.sml \
  src/Rule.sig src/Rule.sml \
  src/Interpretation.sig src/Interpretation.sml \
  src/Object.sig src/Object.sml \
  src/Article.sig src/Article.sml \
  src/Options.sig src/Options.sml

EXTRA_SRC =

###############################################################################
# The ML preprocessor.
###############################################################################

MLPP = scripts/mlpp

MLPP_OPTS =

###############################################################################
# Building using Moscow ML.
###############################################################################

MOSMLC = mosmlc -toplevel -q

MOSML_SRC = \
  src/Portable.sig src/PortableMosml.sml \
  $(SRC)

MOSML_TARGETS = \
  bin/mosml/behold

include bin/mosml/Makefile.src

.PHONY: mosml-info
mosml-info:
	@echo
	@echo '*****************************************'
	@echo '* Build and test the Moscow ML programs *'
	@echo '*****************************************'
	@echo

.PHONY: mosml
mosml: mosml-info $(MOSML_OBJ) $(MOSML_TARGETS) test

###############################################################################
# Building using MLton.
###############################################################################

MLTON = mlton

MLTON_OPTS = -runtime 'ram-slop 0.4'

MLTON_SRC = \
  src/PP.sig src/PP.sml \
  src/Portable.sig src/PortableMlton.sml \
  $(SRC)

BEHOLD = bin/mlton/behold

MLTON_TARGETS = \
  bin/mlton/selftest \
  $(BEHOLD)

bin/mlton/%.sml: $(MLTON_SRC) src/%.sml
	@$(MLPP) $(MLPP_OPTS) -c mlton $^ > $@

bin/mlton/%.mlb: bin/mlton/%.sml
	echo '$$(SML_LIB)/basis/basis.mlb $$(SML_LIB)/basis/mlton.mlb $(notdir $<)' > $@

bin/mlton/%: bin/mlton/%.mlb
	@echo
	@echo '***************************'
	@echo '* Compile a MLton program *'
	@echo '***************************'
	@echo
	@echo $@
	cd bin/mlton ; $(MLTON) $(MLTON_OPTS) $(notdir $<)
	@echo

.PHONY: mlton-info
mlton-info:
	@echo
	@echo '*************************************'
	@echo '* Build and test the MLton programs *'
	@echo '*************************************'
	@echo

.PHONY: mlton
mlton: mlton-info $(MLTON_TARGETS)
	$(MAKE) -C test mlton

###############################################################################
# Development.
##############################################################################

include Makefile.dev

Makefile.dev:
	echo > $@

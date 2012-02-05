###############################################################################
# OPENTHEORY MAKEFILE
# Copyright (c) 2005 Joe Hurd, distributed under the GNU GPL version 2
###############################################################################

.SUFFIXES:

###############################################################################
# The default action.
###############################################################################

.PHONY: default
default:
	@if type -P mosmlc > /dev/null ; then $(MAKE) mosml ; else if type -P mlton > /dev/null ; then $(MAKE) mlton ; else if type -P poly > /dev/null ; then $(MAKE) polyml ; else echo "ERROR: No ML found on path: install either MLton, Poly/ML or Moscow ML." ; exit 1 ; fi ; fi ; fi

###############################################################################
# Cleaning temporary files.
###############################################################################

TEMP = \
  $(MOSML_TARGETS) \
  bin/mosml/*.sml bin/mosml/*.ui bin/mosml/*.uo bin/mosml/a.out \
  $(MLTON_TARGETS) \
  bin/mlton/*.sml bin/mlton/*.mlb \
  $(POLYML_TARGETS) \
  bin/polyml/*.sml bin/polyml/*.log bin/polyml/*.o

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
  src/Map.sig src/Map.sml \
  src/KeyMap.sig src/KeyMap.sml \
  src/Set.sig src/Set.sml \
  src/ElementSet.sig src/ElementSet.sml \
  src/VertexGraph.sig src/VertexGraph.sml \
  src/Sharing.sig src/Sharing.sml \
  src/Stream.sig src/Stream.sml \
  src/Heap.sig src/Heap.sml \
  src/Print.sig src/Print.sml \
  src/Parse.sig src/Parse.sml \
  src/Html.sig src/Html.sml \
  src/Config.sig src/Config.sml \
  src/Checksum.sig src/Checksum.sml \
  src/Namespace.sig src/Namespace.sml \
  src/Name.sig src/Name.sml \
  src/Show.sig src/Show.sml \
  src/MetisNameArity.sig src/MetisNameArity.sml \
  src/MetisTerm.sig src/MetisTerm.sml \
  src/MetisSubst.sig src/MetisSubst.sml \
  src/MetisAtom.sig src/MetisAtom.sml \
  src/MetisFormula.sig src/MetisFormula.sml \
  src/MetisLiteral.sig src/MetisLiteral.sml \
  src/MetisThm.sig src/MetisThm.sml \
  src/MetisProof.sig src/MetisProof.sml \
  src/MetisRule.sig src/MetisRule.sml \
  src/MetisNormalize.sig src/MetisNormalize.sml \
  src/MetisModel.sig src/MetisModel.sml \
  src/MetisProblem.sig src/MetisProblem.sml \
  src/MetisTermNet.sig src/MetisTermNet.sml \
  src/MetisAtomNet.sig src/MetisAtomNet.sml \
  src/MetisLiteralNet.sig src/MetisLiteralNet.sml \
  src/MetisSubsume.sig src/MetisSubsume.sml \
  src/MetisKnuthBendixOrder.sig src/MetisKnuthBendixOrder.sml \
  src/MetisRewrite.sig src/MetisRewrite.sml \
  src/MetisUnits.sig src/MetisUnits.sml \
  src/MetisClause.sig src/MetisClause.sml \
  src/MetisActive.sig src/MetisActive.sml \
  src/MetisWaiting.sig src/MetisWaiting.sml \
  src/MetisResolution.sig src/MetisResolution.sml \
  src/MetisTptp.sig src/MetisTptp.sml \
  src/TypeTerm.sig src/TypeTerm.sml \
  src/TypeOp.sig src/TypeOp.sml \
  src/Type.sig src/Type.sml \
  src/TypeRewrite.sig src/TypeRewrite.sml \
  src/TypeSubst.sig src/TypeSubst.sml \
  src/Var.sig src/Var.sml \
  src/Const.sig src/Const.sml \
  src/Term.sig src/Term.sml \
  src/TermRewrite.sig src/TermRewrite.sml \
  src/TermSearch.sig src/TermSearch.sml \
  src/TermSubst.sig src/TermSubst.sml \
  src/Sequent.sig src/Sequent.sml \
  src/Thm.sig src/Thm.sml \
  src/Symbol.sig src/Symbol.sml \
  src/SymbolTable.sig src/SymbolTable.sml \
  src/Rule.sig src/Rule.sml \
  src/Thms.sig src/Thms.sml \
  src/Sequents.sig src/Sequents.sml \
  src/Summary.sig src/Summary.sml \
  src/Interpretation.sig src/Interpretation.sml \
  src/Command.sig src/Command.sml \
  src/Inference.sig src/Inference.sml \
  src/ObjectData.sig src/ObjectData.sml \
  src/Object.sig src/Object.sml \
  src/ObjectStore.sig src/ObjectStore.sml \
  src/ObjectSymbol.sig src/ObjectSymbol.sml \
  src/ObjectRewrite.sig src/ObjectRewrite.sml \
  src/ObjectUnwanted.sig src/ObjectUnwanted.sml \
  src/ObjectThm.sig src/ObjectThm.sml \
  src/ObjectExport.sig src/ObjectExport.sml \
  src/ObjectThms.sig src/ObjectThms.sml \
  src/ObjectDict.sig src/ObjectDict.sml \
  src/ObjectStack.sig src/ObjectStack.sml \
  src/ObjectRead.sig src/ObjectRead.sml \
  src/ObjectWrite.sig src/ObjectWrite.sml \
  src/Article.sig src/Article.sml \
  src/PackageName.sig src/PackageName.sml \
  src/PackageVersion.sig src/PackageVersion.sml \
  src/PackageNameVersion.sig src/PackageNameVersion.sml \
  src/PackageAuthor.sig src/PackageAuthor.sml \
  src/PackageSummary.sig src/PackageSummary.sml \
  src/PackageTheory.sig src/PackageTheory.sml \
  src/PackageExtra.sig src/PackageExtra.sml \
  src/PackageTag.sig src/PackageTag.sml \
  src/Package.sig src/Package.sml \
  src/DirectorySystem.sig src/DirectorySystem.sml \
  src/PackageTarball.sig src/PackageTarball.sml \
  src/PackageTheorems.sig src/PackageTheorems.sml \
  src/PackageDocument.sig src/PackageDocument.sml \
  src/PackageInfo.sig src/PackageInfo.sml \
  src/PackageDependency.sig src/PackageDependency.sml \
  src/PackageFinder.sig src/PackageFinder.sml \
  src/Theory.sig src/Theory.sml \
  src/TheoryName.sig src/TheoryName.sml \
  src/TheoryGraph.sig src/TheoryGraph.sml \
  src/PackageDag.sig src/PackageDag.sml \
  src/DirectoryChecksums.sig src/DirectoryChecksums.sml \
  src/DirectoryPath.sig src/DirectoryPath.sml \
  src/DirectoryRepo.sig src/DirectoryRepo.sml \
  src/DirectoryPackages.sig src/DirectoryPackages.sml \
  src/DirectoryError.sig src/DirectoryError.sml \
  src/DirectoryConfig.sig src/DirectoryConfig.sml \
  src/Directory.sig src/Directory.sml \
  src/DirectoryQuery.sig src/DirectoryQuery.sml \
  src/Syntax.sig src/Syntax.sml \
  src/Haskell.sig src/Haskell.sml \
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
  bin/mosml/opentheory

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
  src/Portable.sig src/PortableMlton.sml \
  $(SRC)

OPENTHEORY = bin/mlton/opentheory

MLTON_TARGETS = \
  bin/mlton/selftest \
  $(OPENTHEORY)

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
# Building using Poly/ML.
###############################################################################

POLYML = poly

POLYML_OPTS =

ifeq ($(shell uname), Darwin)
  POLYML_LINK_OPTS = -lpolymain -lpolyml -segprot POLY rwx rwx
else
  POLYML_LINK_OPTS = -lpolymain -lpolyml
endif

POLYML_SRC = \
  src/Random.sig src/Random.sml \
  src/Portable.sig src/PortablePolyml.sml \
  $(SRC)

POLYML_TARGETS = \
  bin/polyml/selftest \
  bin/polyml/opentheory

bin/polyml/%.sml: src/%.sml $(POLYML_SRC)
	@$(MLPP) $(MLPP_OPTS) -c polyml $(POLYML_SRC) > $@
	@echo 'fun main () = let' >> $@
	@$(MLPP) $(MLPP_OPTS) -c polyml $< >> $@
	@echo "in () end; PolyML.export(\"$(basename $(notdir $<))\", main);" >> $@

bin/polyml/%.o: bin/polyml/%.sml
	cd bin/polyml ; echo "use \"$(notdir $<)\";" | $(POLYML) $(POLYML_OPTS) > $(basename $(notdir $<)).log
	@if test $@ -nt $< ; then echo 'compiled $@' ; else cat bin/polyml/$(basename $(notdir $<)).log ; exit 1 ; fi

bin/polyml/%: bin/polyml/%.o
	@echo
	@echo '*****************************'
	@echo '* Compile a Poly/ML program *'
	@echo '*****************************'
	@echo
	@echo $@
	cd bin/polyml && $(CC) -o $(notdir $@) $(notdir $<) $(POLYML_LINK_OPTS)
	@echo

.PHONY: polyml-info
polyml-info:
	@echo
	@echo '***************************************'
	@echo '* Build and test the Poly/ML programs *'
	@echo '***************************************'
	@echo

.PHONY: polyml
polyml: polyml-info $(POLYML_TARGETS)
	$(MAKE) -C test polyml

###############################################################################
# Development.
##############################################################################

include Makefile.dev

Makefile.dev:
	echo > $@

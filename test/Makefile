###############################################################################
# OPENTHEORY TEST MAKEFILE
# Copyright (c) 2005 Joe Leslie-Hurd, distributed under the MIT license
###############################################################################

TOOL_TEST = ../scripts/tool-test

POSTPROCESS = perl -ne 'BEGIN { my %checksums = {}; } my $$line = $$_; chomp $$line; if ($$line =~ /^User: /) { $$line = "[This line of the test output contained timings]"; } elsif ($$line =~ /^(.*)([0-9a-f]{40})(.*)$$/) { my $$prefix = $$1; my $$checksum = $$2; my $$suffix = $$3; my $$index; if (defined($$checksums{$$checksum})) { $$index = $$checksums{$$checksum}; } else { $$index = scalar(keys %checksums); $$checksums{$$checksum} = $$index; } $$line = "$$prefix" . "CHECKSUM[$$index]" . "$$suffix"; } print STDOUT "$$line\n";'

REFERENCE_FILE = result.ok

###############################################################################
# The default action
###############################################################################

.PHONY: default
default: mosml

###############################################################################
# Cleaning up temporary files
###############################################################################

TMPFILE1 = /tmp/golden-reference
TMPFILE2 = /tmp/test-output

TEMP = \
  $(TMPFILE1) $(TMPFILE2) mosml-result mlton-result polyml-result \
  articles/bool-and-def.art summaries/bool-and-def.sum \
  articles/bool-and-thm.art summaries/bool-and-thm.sum \
  articles/bool-and-thm-new.art summaries/bool-and-thm-new.sum \
  articles/bool-forall-def.art summaries/bool-forall-def.sum \
  articles/bool-forall-thm.art summaries/bool-forall-thm.sum \
  articles/bool-implies-def.art summaries/bool-implies-def.sum \
  articles/bool-implies-thm.art summaries/bool-implies-thm.sum \
  articles/bool-implies-thm-new.art summaries/bool-implies-thm-new.sum \
  articles/bool-true-def.art summaries/bool-true-def.sum \
  articles/bool-true-thm.art summaries/bool-true-thm.sum \
  articles/empty.art summaries/empty.sum \
  articles/example1.art summaries/example1.sum \
  articles/example2.art summaries/example2.sum \
  articles/example3.art summaries/example3.sum \
  articles/example4.art summaries/example4.sum \
  articles/example5.art summaries/example5.sum \
  articles/example6.art summaries/example6.sum \
  articles/example7.art summaries/example7.sum \
  articles/example8.art summaries/example8.sum \
  articles/example9.art summaries/example9.sum \
  articles/example10.art summaries/example10.sum

.PHONY: clean
clean:
	rm -f $(TEMP)

###############################################################################
# The Moscow ML self-test
###############################################################################

MOSML = mosml -quietdec -I ../bin/mosml

.PHONY: mosml
mosml: mosml-result mosml-diff

.PHONY: diff
mosml-diff:
	@$(POSTPROCESS) < $(REFERENCE_FILE) > $(TMPFILE1)
	@$(POSTPROCESS) < mosml-result > $(TMPFILE2)
	@diff -c $(TMPFILE1) $(TMPFILE2)
	@rm -f $(TMPFILE1) $(TMPFILE2)

mosml-result: test.sml ../bin/mosml/Options.uo ../bin/mosml/opentheory $(TOOL_TEST) $(REFERENCE_FILE)
	rm -f $@
	echo "quit();" | $(MOSML) $< 2>&1 | tee $@
	$(TOOL_TEST) -d ../bin/mosml 2>&1 | tee -a $@

###############################################################################
# The MLton self-test
###############################################################################

.PHONY: mlton
mlton: mlton-result mlton-diff

.PHONY: mlton-diff
mlton-diff:
	@$(POSTPROCESS) < $(REFERENCE_FILE) > $(TMPFILE1)
	@$(POSTPROCESS) < mlton-result > $(TMPFILE2)
	@diff -c $(TMPFILE1) $(TMPFILE2)
	@rm -f $(TMPFILE1) $(TMPFILE2)

mlton-result: ../bin/mlton/selftest ../bin/mlton/opentheory $(TOOL_TEST) $(REFERENCE_FILE)
	rm -f $@
	$< 2>&1 | tee $@
	$(TOOL_TEST) -d ../bin/mlton 2>&1 | tee -a $@

###############################################################################
# The Poly/ML self-test
###############################################################################

.PHONY: polyml
polyml: polyml-result polyml-diff

.PHONY: polyml-diff
polyml-diff:
	@$(POSTPROCESS) < $(REFERENCE_FILE) > $(TMPFILE1)
	@$(POSTPROCESS) < polyml-result > $(TMPFILE2)
	@diff -c $(TMPFILE1) $(TMPFILE2)
	@rm -f $(TMPFILE1) $(TMPFILE2)

polyml-result: ../bin/polyml/selftest ../bin/polyml/opentheory $(TOOL_TEST) $(REFERENCE_FILE)
	rm -f $@
	$< 2>&1 | tee $@
	$(TOOL_TEST) -d ../bin/polyml 2>&1 | tee -a $@

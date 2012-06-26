###############################################################################
# OPENTHEORY TEST MAKEFILE
# Copyright (c) 2005 Joe Hurd, distributed under the MIT license
###############################################################################

IGNORE = '^User: '

REFERENCE_FILE = result.ok

###############################################################################
# The default action
###############################################################################

.PHONY: default
default: mosml

###############################################################################
# Cleaning up temporary files
###############################################################################

TMPFILE1 = /tmp/test-make-file1
TMPFILE2 = /tmp/test-make-file2

TEMP = \
  $(TMPFILE1) $(TMPFILE2) mosml-result mlton-result polyml-result \
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
  articles/example9.art summaries/example9.sum

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
	@grep -v $(IGNORE) < $(REFERENCE_FILE) > $(TMPFILE1)
	@grep -v $(IGNORE) < mosml-result > $(TMPFILE2)
	@diff -c $(TMPFILE1) $(TMPFILE2)
	@rm -f $(TMPFILE1) $(TMPFILE2)

mosml-result: test.sml ../bin/mosml/Options.uo $(REFERENCE_FILE)
	rm -f $@
	echo "quit();" | $(MOSML) $< 2>&1 | tee $@

###############################################################################
# The MLton self-test
###############################################################################

.PHONY: mlton
mlton: mlton-result mlton-diff

.PHONY: mlton-diff
mlton-diff:
	@grep -v $(IGNORE) < $(REFERENCE_FILE) > $(TMPFILE1)
	@grep -v $(IGNORE) < mlton-result > $(TMPFILE2)
	@diff -c $(TMPFILE1) $(TMPFILE2)
	@rm -f $(TMPFILE1) $(TMPFILE2)

mlton-result: ../bin/mlton/selftest $(REFERENCE_FILE)
	rm -f $@
	$< 2>&1 | tee $@

###############################################################################
# The Poly/ML self-test
###############################################################################

.PHONY: polyml
polyml: polyml-result polyml-diff

.PHONY: polyml-diff
polyml-diff:
	@grep -v $(IGNORE) < $(REFERENCE_FILE) > $(TMPFILE1)
	@grep -v $(IGNORE) < polyml-result > $(TMPFILE2)
	@diff -c $(TMPFILE1) $(TMPFILE2)
	@rm -f $(TMPFILE1) $(TMPFILE2)

polyml-result: ../bin/polyml/selftest $(REFERENCE_FILE)
	rm -f $@
	$< 2>&1 | tee $@

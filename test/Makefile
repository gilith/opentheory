###############################################################################
# OPENTHEORY TEST MAKEFILE
# Copyright (c) 2005 Joe Hurd, distributed under the GNU GPL version 2
###############################################################################

IGNORE = '^User: '

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
  articles/bool-def-true.art articles/bool-def-true.sum \
  articles/bool-int-true.art articles/bool-int-true.sum \
  articles/bool-true-aux.art articles/bool-true-aux.sum \
  articles/empty.art articles/empty.sum \
  articles/example1.art articles/example1.sum \
  articles/example2.art articles/example2.sum \
  articles/example3.art articles/example3.sum \
  articles/example4.art articles/example4.sum

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
	@grep -v $(IGNORE) <result.ok >$(TMPFILE1)
	@grep -v $(IGNORE) <mosml-result >$(TMPFILE2)
	@diff -c $(TMPFILE1) $(TMPFILE2)
	@rm -f $(TMPFILE1) $(TMPFILE2)

mosml-result: test.sml ../bin/mosml/Options.uo
	rm -f $@
	echo "quit();" | $(MOSML) $< 2>&1 | tee $@

###############################################################################
# The MLton self-test
###############################################################################

.PHONY: mlton
mlton: mlton-result mlton-diff

.PHONY: mlton-diff
mlton-diff:
	@grep -v $(IGNORE) <result.ok >$(TMPFILE1)
	@grep -v $(IGNORE) <mlton-result >$(TMPFILE2)
	@diff -c $(TMPFILE1) $(TMPFILE2)
	@rm -f $(TMPFILE1) $(TMPFILE2)

mlton-result: ../bin/mlton/selftest
	rm -f $@
	$< 2>&1 | tee $@

###############################################################################
# The Poly/ML self-test
###############################################################################

.PHONY: polyml
polyml: polyml-result polyml-diff

.PHONY: polyml-diff
polyml-diff:
	@grep -v $(IGNORE) <result.ok >$(TMPFILE1)
	@grep -v $(IGNORE) <polyml-result >$(TMPFILE2)
	@diff -c $(TMPFILE1) $(TMPFILE2)
	@rm -f $(TMPFILE1) $(TMPFILE2)

polyml-result: ../bin/polyml/selftest
	rm -f $@
	$< 2>&1 | tee $@

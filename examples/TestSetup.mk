EXAMPLES_DIR ?= ./examples
setup-twelf:
	gh repo clone standardml/twelf $(EXAMPLES_DIR)/twelf 
setup-smlnj:
	gh repo clone smlnj/smlnj $(EXAMPLES_DIR)/smlnj 
untrack: 
	rm -rf $(EXAMPLES_DIR)/twelf/.git 
	rm -rf $(EXAMPLES_DIR)/smlnj/.git
setup-tests: setup-twelf setup-smlnj untrack
clean-tests:
	rm -rf $(EXAMPLES_DIR)/twelf
	rm -rf $(EXAMPLES_DIR)/smlnj
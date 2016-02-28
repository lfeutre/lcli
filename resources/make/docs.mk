.PHONY: docs proj-docs publish-docs clean-docs

DOCS_DIR = $(ROOT_DIR)/docs
CURRENT_VERSION = current
DOCS_PROD_DIR = $(DOCS_DIR)
CURRENT_PROD_DIR = $(DOCS_PROD_DIR)/$(CURRENT_VERSION)

docs: clean-docs build proj-docs

publish-docs: $(DOCS_PROD_DIR) docs setup-temp-repo
	@echo "\nPublishing docs ...\n"
	@cd $(DOCS_PROD_DIR) && git push -f $(REPO) master:gh-pages
	@make teardown-temp-repo

# The options for generating the project documentation are saved in
# configuration files. See the project rebar.config file for more info.
proj-docs:
	@echo "\nBuilding project docs ...\n"
	@rm -rf $(CURRENT_PROD_DIR)
	@mkdir -p $(DOCS_PROD_DIR)
	@rebar3 lfe lodox

DOCS_GIT_HACK = $(DOCS_DIR)/.git

$(DOCS_PROD_DIR):
	@mkdir -p $(DOCS_PROD_DIR)

$(DOCS_GIT_HACK):
	@ln -s $(ROOT_DIR)/.git $(DOCS_GIT_HACK)

setup-temp-repo:
	@echo "\nSetting up temporary git repos for gh-pages ...\n"
	@cd $(DOCS_PROD_DIR) &&  \
	git init && \
	git add * > /dev/null && \
	git commit -a -m "Generated content." > /dev/null

teardown-temp-repo:
	@echo "\nTearing down temporary gh-pages repos ..."
	@rm -rf $(DOCS_PROD_DIR)/.git

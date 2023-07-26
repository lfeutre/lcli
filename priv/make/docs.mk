.PHONY: docs proj-docs publish-docs clean-docs

DOCS_DIR = $(ROOT_DIR)/priv/mdbook
DOCS_BUILD_DIR = $(ROOT_DIR)/docs
LOCAL_DOCS_HOST = localhost
LOCAL_DOCS_PORT = 5099

docs: docs-clean
	@echo "\nBuilding docs ...\n"
	@cd $(DOCS_DIR) && mdbook build -d $(DOCS_BUILD_DIR)

docs-open: docs-clean
	@echo "\nBuilding docs ...\n"
	@cd $(DOCS_DIR) && mdbook build -d $(DOCS_BUILD_DIR) -o

docs-clean:
	@echo "\nCleaning build directories ..."
	@rm -rf $(DOCS_BUILD_DIR)

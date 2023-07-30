LFE = ./_build/default/lib/lfe/bin/lfe
EX_DIR = ./examples

PHONY: examples

examples: clean build fake-db greeter lcli-app-formatting

fake-db:
	test \
	"$(shell $(LFE) $(EX_DIR)/fake-db.lfe --help)" \
	== "$(shell cat priv/testing/fake-db-help.txt)"

	test \
	"$(shell $(LFE) $(EX_DIR)/fake-db.lfe -x --port 5099 --dbname webapp -o output.dump arg1 arg2)" \
	== "$(shell cat priv/testing/fake-db.txt)"

greeter:
	test \
	'$(shell $(LFE) $(EX_DIR)/greeter.lfe)' \
	== 'Hello, World!'

	test \
	'$(shell $(LFE) $(EX_DIR)/greeter.lfe -g "Awwww, ")' \
	== 'Awwww, World!'

	test \
	'$(shell $(LFE) $(EX_DIR)/greeter.lfe -e "Mr. Bill!")' \
	== 'Hello, Mr. Bill!'

	test \
	'$(shell $(LFE) $(EX_DIR)/greeter.lfe -g "Awwww, " -e "Nuts!")' \
	== 'Awwww, Nuts!'

	test \
	'$(shell $(LFE) $(EX_DIR)/greeter.lfe --greeting "On, no! " -e "Nuts!")' \
	== 'On, no! Nuts!'

	test \
	'$(shell $(LFE) $(EX_DIR)/greeter.lfe --greeting "On, no! " --greetee "Mr. Bill!!")' \
	== 'On, no! Mr. Bill!!'

lcli-app-formatting:
	test \
	"$(shell $(LFE) $(EX_DIR)/lcli-app-formatting.lfe)" \
	== "$(shell cat priv/testing/lcli-app-formatting.txt)"

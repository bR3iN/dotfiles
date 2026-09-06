CODELLDB_VERSION ?= 1.11.5

CODELLDB_URL = https://github.com/vadimcn/codelldb/releases/download/v$(CODELLDB_VERSION)/codelldb-linux-x64.vsix

# The `.vsix` is a zip archive holding the extension itself in an `extension/`
# subdirectory, which is what we unpack.
$(CODELLDB_DIR):
	@echo "Installing codelldb v$(CODELLDB_VERSION)"
	@tmp="$$(mktemp -d)"; trap 'rm -rf "$$tmp"' EXIT; cd "$$tmp" && \
	$(call download,$(CODELLDB_URL),codelldb.vsix) && \
	unzip -q codelldb.vsix 'extension/*' && \
	mkdir -p "$(dir $@)" && \
	mv extension "$@"

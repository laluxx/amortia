GREEN = \033[1;32m
RESET = \033[0m
BOLD = \033[1m

GHC = ghc
GHCFLAGS = -dynamic

SOURCES = Parser.hs Main.hs
MODULES = Parser Main

TARGET = amortia

OBJS = $(MODULES:%=%.o)
HIS = $(MODULES:%=%.hi)

.PHONY: all
all: $(TARGET)

# Development build (no optimization, faster compilation)
$(TARGET): $(SOURCES)
	@echo "🔨 Building $(TARGET) (development mode)..."
	$(GHC) $(GHCFLAGS) $(SOURCES) -o $(TARGET)
	@printf "$(GREEN)✓$(RESET) Build complete: $(TARGET)\n"

# Release build (optimized)
.PHONY: release
release: GHCFLAGS = -dynamic -O2
release: clean
	@echo "🔨 Building $(TARGET) (release mode)..."
	@$(GHC) $(GHCFLAGS) $(SOURCES) -o $(TARGET)
	@printf "$(GREEN)✓$(RESET) Build complete: $(TARGET)\n"

.PHONY: install
install: $(TARGET)
	@echo "📦 Installing $(TARGET) to /bin..."
	@sudo cp $(TARGET) /bin/
	@printf "$(GREEN)✓$(RESET) Installed successfully\n"

.PHONY: clean
clean:
	@printf "🧹 Cleaning build artifacts... "
	@rm -f $(TARGET) $(OBJS) $(HIS)
	@printf "$(GREEN)$(BOLD)DONE$(RESET)\n"

.PHONY: test
test: $(TARGET)
	@echo "🧪 Testing with src.amor..."
	./$(TARGET) json src.amor test-output.json
	@printf "$(GREEN)✓$(RESET) Test complete - check test-output.json\n"

.PHONY: help
help:
	@echo "Amortia Compiler - Makefile targets:"
	@echo "  make          - Build the project (development, fast)"
	@echo "  make release  - Build with optimizations"
	@echo "  make clean    - Remove build artifacts"
	@echo "  make install  - Install to /bin (requires sudo)"
	@echo "  make test     - Run parser on src.amor"
	@echo "  make help     - Show this help message"

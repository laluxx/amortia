GHC = ghc
GHCFLAGS = -dynamic

SOURCES = Parser.hs Main.hs
MODULES = Parser Main

TARGET = amortia

OBJS = $(MODULES:%=%.o)
HIS = $(MODULES:%=%.hi)

# Default target - development build
.PHONY: all
all: $(TARGET)

# Development build (no optimization, faster compilation)
$(TARGET): $(SOURCES)
	@echo "🔨 Building $(TARGET) (development mode)..."
	$(GHC) $(GHCFLAGS) $(SOURCES) -o $(TARGET)
	@echo "✓ Build complete: $(TARGET)"

# Release build (optimized)
.PHONY: release
release: GHCFLAGS = -dynamic -O2
release: clean
	@echo "🔨 Building $(TARGET) (release mode)..."
	@$(GHC) $(GHCFLAGS) $(SOURCES) -o $(TARGET)
	@echo "✓ Build complete: $(TARGET)"

# Install target
.PHONY: install
install: $(TARGET)
	@echo "📦 Installing $(TARGET) to /bin..."
	@sudo cp $(TARGET) /bin/
	@echo "✓ Installed successfully"

# Clean build artifacts
.PHONY: clean
clean:
	@printf "🧹 Cleaning build artifacts... "
	@rm -f $(TARGET) $(OBJS) $(HIS)
	@echo "DONE"

# Run tests on example file
.PHONY: test
test: $(TARGET)
	@echo "🧪 Testing with src.amor..."
	./$(TARGET) --json src.amor test-output.json
	@echo "✓ Test complete - check test-output.json"

# Show help
.PHONY: help
help:
	@echo "Amortia Parser & Visualizer - Makefile targets:"
	@echo ""
	@echo "  make          - Build the project (development, fast)"
	@echo "  make release  - Build with optimizations"
	@echo "  make clean    - Remove build artifacts"
	@echo "  make install  - Install to /bin (requires sudo)"
	@echo "  make test     - Run parser on src.amor"
	@echo "  make help     - Show this help message"
	@echo ""
	@echo "Usage examples:"
	@echo "  ./$(TARGET) --watch src.amor"
	@echo "  ./$(TARGET) --visualize src.amor"
	@echo "  ./$(TARGET) --json src.amor output.json"

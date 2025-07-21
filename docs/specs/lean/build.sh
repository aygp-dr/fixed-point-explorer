#!/bin/bash
#!/bin/bash
# Build and run tests

echo "Building Fixed Point Explorer Lean specifications..."

# Build with Lake
../../tools/formal-methods/lean/bin/lake build

# Run tests
echo -e "\nRunning tests..."
../../tools/formal-methods/lean/bin/lake exe tests

# Run individual file checks
echo -e "\nChecking individual files..."
for file in *.lean; do
  if [[ "$file" != "lakefile.lean" ]]; then
    echo "Checking $file..."
    ../../tools/formal-methods/lean/bin/lean "$file" --quiet || echo "  Issues found in $file"
  fi
done

echo -e "\nBuild complete!"

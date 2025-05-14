#!/bin/bash

set -e

input_file="README.md"
output_file="/tmp/README_1.md"

# Make sure input file exists
if [ ! -f "$input_file" ]; then
  echo "Error: Input file '$input_file' not found."
  exit 1
fi

# Clear or create the output file
echo "" > "$output_file"

# Read input file line by line
while IFS= read -r line; do
  echo "$line" >> "$output_file"
  if [ "$line" = "<!-- FROM APP -->" ]; then
    break
  fi
done < "$input_file"

echo "" >> "$output_file"
echo "" >> "$output_file"

echo "" > "/tmp/README_2.md"
./run.sh helpMd "/tmp/README_2.md"

# Append the content of the second file to the output file
cat /tmp/README_2.md >> "$output_file"

cp "$output_file" README.md

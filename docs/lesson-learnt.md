# Lessons Learned: TOML Parser Development

## Understanding the TOML Specification

- **Hierarchical paths vs. literal dotted keys**: There's a critical difference between `dog.tatter.man` (a hierarchical path to nested tables) and `"dog.tatter.man"` (a single key with literal dots)
- **Proper quoting rules**: Keys need quoting when they contain characters outside the basic set (letters, numbers, underscores, hyphens)
- **Table representation**: The TOML format has specific ways of representing tables, inline tables, and arrays of tables
- **Dotted keys in paths**: Understanding how to handle dots in keys vs dots in paths is essential

## Implementation Techniques

- **Path component handling**: We need to track and properly quote path components during serialization
- **Table structure preservation**: When parsing hierarchical tables, we must maintain the nested structure
- **Smart quoting logic**: The `NeedsQuoting` function provides a reliable way to determine when keys need quotes
- **Serialization challenges**: Converting in-memory data structures to properly formatted TOML requires careful attention to detail

## Testing Best Practices

- **Split complex tests**: Breaking test cases into smaller, focused units makes debugging easier
- **Test specific behaviors**: Having separate tests for hierarchical paths and literal dotted keys clarifies the expected behavior
- **Avoid assumptions**: Don't assume features (like dot notation access) exist if they aren't explicitly documented
- **Test round-trip behavior**: Verify that serializing and then parsing again preserves the original structure
- **Progressive verification**: Test each level of a complex structure separately

## Debugging Strategies

- **Isolate the problem**: When a test fails, focus on what specific assertion is failing
- **Understand root causes**: In our case, the issue was due to confusion between hierarchical paths and literal dotted keys
- **Verify assumptions**: Don't assume how something works; test it explicitly
- **Use proper memory management**: Ensure objects are properly created and freed to avoid memory leaks

## Object Pascal Specifics

- **Memory management**: Properly free objects with try-finally blocks
- **Avoid duplicate keys**: The TTOMLTable doesn't allow adding duplicate keys
- **TStringBuilder efficiency**: Using TStringBuilder for string concatenation is more efficient than string operators
- **TStringList for paths**: Using TStringList for path components enables easy management of path hierarchies

## Importance of Specifications

- **Follow the spec closely**: The TOML specification provides exact rules for formatting and parsing
- **Edge cases matter**: Pay special attention to edge cases like quoting rules and nested structures
- **Test against spec examples**: Using examples from the official spec as test cases ensures compliance

These lessons highlight the importance of careful design, thorough testing, and attention to detail when implementing parsers and serializers for structured data formats like TOML.
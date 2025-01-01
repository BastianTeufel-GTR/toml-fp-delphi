# TOML Parser for Free Pascal

A TOML v1.0.0 parser and serializer implementation for Free Pascal (FPC). This library provides a type-safe and easy-to-use interface for working with TOML files in Free Pascal applications.

## Features

- Full support for TOML v1.0.0 specification
- Type-safe parsing and serialization
- Easy-to-use API
- No external dependencies
- Comprehensive error handling
- Support for all TOML data types:
  - Strings (basic and multi-line)
  - Integers
  - Floats
  - Booleans
  - Dates and Times
  - Arrays
  - Tables (including inline tables)

## Requirements

- Free Pascal Compiler (FPC) 3.2.2 or later

## Installation

1. Clone this repository or download the source files
2. Add the `src` directory to your project's search path
3. Add `TOML` to your unit's uses clause

## Usage

### Parsing TOML

```pascal
uses
  TOML;

var
  TOMLData: TTOMLTable;
begin
  // Parse from string
  TOMLData := ParseTOML('key = "value"');
  try
    // Use the data...
  finally
    TOMLData.Free;
  end;

  // Parse from file
  TOMLData := ParseTOMLFromFile('config.toml');
  try
    // Use the data...
  finally
    TOMLData.Free;
  end;
end;
```

### Creating TOML Data

```pascal
uses
  TOML;

var
  Table: TTOMLTable;
  Array: TTOMLArray;
begin
  Table := TOMLTable;
  try
    // Add simple values
    Table.Add('string', TOMLString('Hello, World!'));
    Table.Add('integer', TOMLInteger(42));
    Table.Add('float', TOMLFloat(3.14));
    Table.Add('boolean', TOMLBoolean(True));
    Table.Add('date', TOMLDateTime(Now));

    // Create and add an array
    Array := TOMLArray;
    Array.Add(TOMLInteger(1));
    Array.Add(TOMLInteger(2));
    Array.Add(TOMLInteger(3));
    Table.Add('numbers', Array);

    // Create nested table
    var NestedTable := TOMLTable;
    NestedTable.Add('nested_key', TOMLString('nested_value'));
    Table.Add('nested', NestedTable);

    // Serialize to string
    WriteLn(SerializeTOML(Table));

    // Save to file
    SerializeTOMLToFile(Table, 'output.toml');
  finally
    Table.Free;
  end;
end;
```

### Reading TOML Values

```pascal
uses
  TOML;

var
  TOMLData: TTOMLTable;
  Value: TTOMLValue;
begin
  TOMLData := ParseTOMLFromFile('config.toml');
  try
    // Reading values with type checking
    if TOMLData.TryGetValue('string_key', Value) then
      WriteLn('String value: ', Value.AsString);

    if TOMLData.TryGetValue('int_key', Value) then
      WriteLn('Integer value: ', Value.AsInteger);

    if TOMLData.TryGetValue('float_key', Value) then
      WriteLn('Float value: ', Value.AsFloat);

    if TOMLData.TryGetValue('bool_key', Value) then
      WriteLn('Boolean value: ', Value.AsBoolean);

    if TOMLData.TryGetValue('date_key', Value) then
      WriteLn('Date value: ', DateTimeToStr(Value.AsDateTime));

    // Reading arrays
    if TOMLData.TryGetValue('array_key', Value) then
    begin
      var Array := Value.AsArray;
      for var i := 0 to Array.Count - 1 do
        WriteLn('Array item ', i, ': ', Array.GetItem(i).AsString);
    end;

    // Reading nested tables
    if TOMLData.TryGetValue('table_key', Value) then
    begin
      var Table := Value.AsTable;
      if Table.TryGetValue('nested_key', Value) then
        WriteLn('Nested value: ', Value.AsString);
    end;
  finally
    TOMLData.Free;
  end;
end;
```

## Error Handling

The library uses exception classes for error handling:

- `ETOMLException`: Base exception class for all TOML-related errors
- `ETOMLParserException`: Raised when parsing errors occur
- `ETOMLSerializerException`: Raised when serialization errors occur

Example:

```pascal
uses
  TOML;

var
  TOMLData: TTOMLTable;
begin
  try
    TOMLData := ParseTOML('invalid = toml] content');
    try
      // Use the data...
    finally
      TOMLData.Free;
    end;
  except
    on E: ETOMLParserException do
      WriteLn('Parsing error: ', E.Message);
    on E: ETOMLException do
      WriteLn('TOML error: ', E.Message);
  end;
end;
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This library is released under the MIT License. See the LICENSE file for details. 
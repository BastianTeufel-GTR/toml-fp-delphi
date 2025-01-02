# TOML Parser for Free Pascal

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Free Pascal](https://img.shields.io/badge/Free%20Pascal-3.2.2-blue.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-3.6-orange.svg)](https://www.lazarus-ide.org/)
[![TOML](https://img.shields.io/badge/TOML-1.0.0-green.svg)](https://toml.io/)

A robust [TOML (Tom's Obvious, Minimal Language)](https://toml.io/) parser and serializer for Free Pascal, fully compliant with TOML v1.0.0 specification.

## Overview

TOML-FP provides a complete solution for working with TOML configuration files in Free Pascal applications. Whether you need to read configuration files or generate them, TOML-FP offers an intuitive API with comprehensive type support and robust error handling.

## Features

- **Full TOML v1.0.0 Compliance:** Supports all TOML data types and structures
- **Type-Safe API:** Strong typing with compile-time checks
- **Memory Management:** Automatic cleanup with proper object lifecycle management
- **Error Handling:** Detailed error messages and exception handling
- **Serialization:** Convert Pascal objects to TOML and back
- **Documentation:** Comprehensive examples and API documentation
- **Test Suite:** Comprehensive test suite (53 items)

## Requirements

- Free Pascal Compiler 3.2.2 or later
- Lazarus IDE 3.6 (needed for running the test runner; optional, for development)

## Installation

### Requirements
- Free Pascal Compiler 3.2.2 or later
- Lazarus IDE 3.6 (optional, for development)

### Steps
1. Clone the repository:
   
   ```bash
   git clone https://github.com/yourusername/toml-fp.git
   ```

2. Add to your project:
   - Copy the `src` directory to your project
   - Add units to your project's search path
   - Add to your uses clause:
     ```pascal
     uses
       TOML, TOML.Types, TOML.Parser, TOML.Serializer;
     ```

## Quick Start

### Basic Usage

```pascal
program BasicParseTOML;

{$mode objfpc}{$H+}{$J-}

uses
  TOML;

var
  Config: TTOMLTable;
  RevisionValue, ProjectValue, ProjectName: TTOMLValue;
  ProjectTable: TTOMLTable;

begin
  // Parse TOML from file
  Config := ParseTOMLFromFile('config.toml');
  try

    // Access a string value
    if (Config.TryGetValue('revision', RevisionValue)) then
      WriteLn('The value of ''revision'' is ', RevisionValue.AsString);

    // Access nested values safely
    if Config.TryGetValue('project', ProjectValue) and
      (ProjectValue is TTOMLTable) then
    begin
      ProjectTable := TTOMLTable(ProjectValue);
      if ProjectTable.TryGetValue('name', ProjectName) then
        WriteLn('Project Name: ', ProjectName.AsString)
      else
        WriteLn('Project name not found');
    end
    else
      WriteLn('Project configuration not found');
  finally
    Config.Free;
  end;
end.
```

Example `config.toml`:

```toml
# config.toml

revision = "1.2.1af"

[project]
name = "My Amazing Project"
version = "1.0.0"
```


### Writing TOML Files
```pascal
program BasicSerializeTOML;

{$mode objfpc}{$H+}{$J-}

uses
  TOML;

var
  Config: TTOMLTable;
  Database: TTOMLTable;
begin
  Config := TOMLTable;
  try
    Database := TOMLTable;
    Database.Add('host', TOMLString('localhost'));
    Database.Add('port', TOMLInteger(5432));
    Config.Add('database', Database);

    if SerializeTOMLToFile(Config, 'config.toml') then
      WriteLn('Configuration saved successfully')
    else
      WriteLn('Error saving configuration');
  finally
    Config.Free;
  end;
end.
```

## 📚 Documentation

### Common Use Cases

#### Working with Arrays
```pascal
var
  Config: TTOMLTable;
  Tags: TTOMLArray;
begin
  Config := TOMLTable;
  try
    Tags := TOMLArray;
    Tags.Add(TOMLString('pascal'));
    Tags.Add(TOMLString('toml'));
    Config.Add('tags', Tags);
    
    WriteLn(SerializeTOML(Config));
  finally
    Config.Free;
  end;
end.
```

#### Nested Tables
```pascal
var
  Config: TTOMLTable;
  Database: TTOMLTable;
begin
  Config := TOMLTable;
  try
    Database := TOMLTable;
    Database.Add('host', TOMLString('localhost'));
    Database.Add('port', TOMLInteger(5432));
    Config.Add('database', Database);
    
    WriteLn(SerializeTOML(Config));
  finally
    Config.Free;
  end;
end.
```

#### Serializing Complex Structures
```pascal
program BasicSerializeTOML;

{$mode objfpc}{$H+}{$J-}

uses
  TOML, SysUtils;

var
  Config, ServerConfig: TTOMLTable;
  Ports: TTOMLArray;
  SerializedTOML: string;
begin
  Config := TOMLTable;
  try
    // Create a nested table
    ServerConfig := TOMLTable;
    ServerConfig.Add('host', TOMLString('127.0.0.1'));
    ServerConfig.Add('enabled', TOMLBoolean(True));

    // Create and populate an array
    Ports := TOMLArray;
    Ports.Add(TOMLInteger(80));
    Ports.Add(TOMLInteger(443));
    ServerConfig.Add('ports', Ports);

    // Add the server config to main config
    Config.Add('server', ServerConfig);

    // Add some basic metadata
    Config.Add('version', TOMLFloat(1.0));
    Config.Add('last_updated', TOMLDateTime(Now));

    // Serialize to TOML format
    SerializedTOML := SerializeTOML(Config);
    WriteLn('Generated TOML:');
    WriteLn(SerializedTOML);

    // Save to file
    if SerializeTOMLToFile(Config, 'config.toml') then
      WriteLn('Successfully saved to file')
    else
      WriteLn('Error saving to file');

  finally
    Config.Free;
  end;
end.
```

This will generate TOML like:
```toml
version = 1.0
last_updated = 2024-03-20T15:30:45Z

[server]
host = "127.0.0.1"
enabled = true
ports = [ 80, 443 ]
```

Note: All values are properly type-checked and memory-managed. The library ensures that:
- Each value has the correct TOML type
- Arrays maintain type consistency
- All objects are properly freed
- Type conversions are validated

## API Reference

### Types

- `TTOMLValue` - Base type for all TOML values
- `TTOMLTable` - Represents a TOML table
- `TTOMLArray` - Represents a TOML array
- `TTOMLString` - Represents a TOML string value
- `TTOMLInteger` - Represents a TOML integer value
- `TTOMLFloat` - Represents a TOML float value
- `TTOMLBoolean` - Represents a TOML boolean value
- `TTOMLDateTime` - Represents a TOML datetime value

### Helper Functions for Creating TOML Values

- `TOMLString`

```pascal
  function TOMLString(const AValue: string): TTOMLString;
```
Creates a TOML string value.

- `TOMLInteger`

```pascal
  function TOMLInteger(const AValue: Int64): TTOMLInteger;
```
Creates a TOML integer value.

- `TOMLFloat`

```pascal
  function TOMLFloat(const AValue: Double): TTOMLFloat;
```
Creates a TOML float value.

- `TOMLBoolean` 

```pascal
    function TOMLBoolean(const AValue: Boolean): TTOMLBoolean;
```
Creates a TOML boolean value.

- `TOMLDateTime`

```pascal
    function TOMLDateTime(const AValue: TDateTime): TTOMLDateTime;
```
Creates a TOML datetime value.

- `TOMLArray` 

```pascal
  function TOMLArray: TTOMLArray;
```
Creates a TOML array.

- `TOMLTable`

```pascal
  function TOMLTable: TTOMLTable;
```
Creates a TOML table.

### Parsing Functions

- `ParseTOML`

```pascal
  function ParseTOML(const ATOML: string): TTOMLTable;
```
Parses a TOML-formatted string into a `TTOMLTable` object.

```pascal
  function ParseTOML(const ATOML: string): TTOMLTable;
  begin
    Result := TOML.Parser.ParseTOMLString(ATOML);
  end;
```

- `ParseTOMLFromFile`

```pascal
  function ParseTOMLFromFile(const AFileName: string): TTOMLTable;
```
Parses a TOML file into a `TTOMLTable` object.

```pascal
  function ParseTOMLFromFile(const AFileName: string): TTOMLTable;
  var
    FileStream: TFileStream;
    StringStream: TStringStream;
  begin
    FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      StringStream := TStringStream.Create('');
      try
        StringStream.CopyFrom(FileStream, 0);
        Result := ParseTOMLString(StringStream.DataString);
      finally
        StringStream.Free;
      end;
    finally
      FileStream.Free;
    end;
  end;
```

### Serialization Functions
- `SerializeTOML`

```pascal
  function SerializeTOML(const AValue: TTOMLValue): string;
```
Serializes a `TTOMLValue` into a TOML-formatted string.

```pascal
  function SerializeTOML(const AValue: TTOMLValue): string;
  begin
    Result := TOML.Serializer.SerializeTOML(AValue);
  end;
```

- `SerializeTOMLToFile`

```pascal
  function SerializeTOMLToFile(const AValue: TTOMLValue; const AFileName: string): Boolean;
```
Serializes a `TTOMLValue` and saves it to a file.

```pascal
  function SerializeTOMLToFile(const AValue: TTOMLValue; const AFileName: string): Boolean;
  begin
    Result := TOML.Serializer.SerializeTOMLToFile(AValue, AFileName);
  end;
```

## 🧪 Testing

The library includes a comprehensive test suite (53 items). 

To run the tests:

1. Open `tests/TestRunner.lpi` in Lazarus
2. Build and run the project

### Sample Test Output

```bash
$ ./TestRunner.exe -a --format=plain
 Time:00.006 N:53 E:0 F:0 I:0
  TTOMLTestCase Time:00.006 N:53 E:0 F:0 I:0
    Test01_StringValue
    Test02_IntegerValue
    ...
    Test70_ComplexKeys

Number of run tests: 53
Number of errors:    0
Number of failures:  0

Heap dump by heaptrc unit of path\to\TestRunner.exe
2991 memory blocks allocated : 195551/208600
2991 memory blocks freed     : 195551/208600
0 unfreed memory blocks : 0
True heap size : 294912 (256 used in System startup)
True free heap : 294656
``` 

## Examples

Check out the `examples` directory and the test cases, `tests/TestCaseTOML.pas`, for more detailed usage examples:

- Basic Configuration Reading
- Writing Complex Data Structures
- Error Handling Patterns
- Integration Examples

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

Please ensure:
- Code follows the project's style guidelines
- All tests pass
- New features include appropriate tests
- Documentation is updated

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## Acknowledgments

- TOML specification creators and maintainers
- Free Pascal and Lazarus communities
- All contributors to this project




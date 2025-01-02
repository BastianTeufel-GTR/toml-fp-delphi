# TOML Parser for Free Pascal

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Free Pascal](https://img.shields.io/badge/Free%20Pascal-3.2.2-blue.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-3.6-orange.svg)](https://www.lazarus-ide.org/)

TOML-FP is a robust and efficient [TOML (Tom's Obvious, Minimal Language)](https://toml.io/) parser and serializer written in Free Pascal. It adheres to the TOML v1.0.0 specification, providing comprehensive support for various data types, arrays, tables, and more.

## 🚀 Features

- Comprehensive Type Support: Handles strings, integers, floats, booleans, datetime, arrays, and tables.
- Array and Table Handling: Supports mixed-type arrays, nested tables, and array of tables.
- Serialization: Easily convert TOML data structures back into TOML-formatted strings or files.
- Error Handling: Provides detailed exceptions for parsing and serialization errors.
- Memory Management: Efficiently manages memory with no leaks, ensuring optimal performance.
- Compliant with TOML v1.0.0: Fully adheres to the TOML specification for compatibility and reliability.

## 📋 Requirements

- Free Pascal Compiler 3.2.2 or later
- Lazarus IDE 3.6 (needed for running the test runner; optional, for development)

## ⚡ Quick Start

### Installation

1. Clone the Repository:

```bash
   git clone https://github.com/ikelaiah/toml-fp.git
```

2. Add the following units to your project path:
- TOML.pas
- TOML.Types.pas
- TOML.Parser.pas
- TOML.Serializer.pas

3. Add the following units to your project uses clause:

```pascal
uses
  TOML, TOML.Types, TOML.Parser, TOML.Serializer;
```

4. Configure Compiler Settings:

Ensure your Free Pascal Compiler (FPC) is set to mode objfpc with H+ string handling by including the following directives at the top of your source files:

```pascal
{$mode objfpc}{$H+}{$J-}
```

### Basic Usage

```pascal
program BasicParseTOML;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, TOML;

var
  Config: TTOMLTable;
  ProjectValue: TTOMLValue;
  ProjectTable: TTOMLTable;
  ProjectName: TTOMLValue;
begin
  // Parse TOML from file
  Config := ParseTOMLFromFile('config.toml');
  try
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

Example config.toml:
```toml
[project]
name = "My Amazing Project"
version = "1.0.0"
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
program SerializationExample;
{$mode objfpc}{$H+}{$J-}

uses
  TOML, SysUtils;

var
  Config, ServerConfig: TTOMLTable;
  Ports: TTOMLArray;
  TOMLString: string;
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
    TOMLString := SerializeTOML(Config);
    WriteLn('Generated TOML:');
    WriteLn(TOMLString);
    
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
### Helper Functions for Creating TOML Values
- TOMLString

```pascal
  function TOMLString(const AValue: string): TTOMLString;
```
Creates a TOML string value.

- TOMLInteger

```pascal
  function TOMLInteger(const AValue: Int64): TTOMLInteger;
```
Creates a TOML integer value.

- TOMLFloat

```pascal
  function TOMLFloat(const AValue: Double): TTOMLFloat;
```
Creates a TOML float value.

- TOMLBoolean 

```pascal
    function TOMLBoolean(const AValue: Boolean): TTOMLBoolean;
```
Creates a TOML boolean value.

- TOMLDateTime

```pascal
    function TOMLDateTime(const AValue: TDateTime): TTOMLDateTime;
```
Creates a TOML datetime value.

- TOMLArray 

```pascal
  function TOMLArray: TTOMLArray;
```
Creates a TOML array.

- TOMLTable

```pascal
  function TOMLTable: TTOMLTable;
```
Creates a TOML table.

### Parsing Functions
- ParseTOML

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

- ParseTOMLFromFile

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
- SerializeTOML

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

- SerializeTOMLToFile

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
3. Or from command line: `fpc tests/TestRunner.lpr && ./TestRunner`

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

## 🔍 Examples

Check out the `examples` directory and the test cases, `tests/TestCaseTOML.pas`, for more detailed usage examples:

- Basic Configuration Reading
- Writing Complex Data Structures
- Error Handling Patterns
- Integration Examples

## 📫 Support

- Create an issue for bug reports or feature requests
- Star the repository if you find it useful
- Follow the project for updates

## 🤝 Contributing

Contributions are welcome! Here's how you can help:

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## 📝 License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## 🏆 Acknowledgments

- TOML specification creators and maintainers
- Free Pascal and Lazarus communities
- All contributors to this project




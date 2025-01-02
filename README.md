# TOML Parser for Free Pascal

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Free Pascal](https://img.shields.io/badge/Free%20Pascal-3.2.2-blue.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-3.6-orange.svg)](https://www.lazarus-ide.org/)

TOML-FP is a robust and efficient TOML (Tom's Obvious, Minimal Language) parser and serializer written in Free Pascal. It adheres to the TOML v1.0.0 specification, providing comprehensive support for various data types, arrays, tables, and more. Whether you're configuring applications or handling complex data structures, TOML-FP offers a reliable solution for your parsing and serialization needs.

## Features

- Comprehensive Type Support: Handles strings, integers, floats, booleans, datetime, arrays, and tables.
- Array and Table Handling: Supports mixed-type arrays, nested tables, and array of tables.
- Serialization: Easily convert TOML data structures back into TOML-formatted strings or files.
- Error Handling: Provides detailed exceptions for parsing and serialization errors.
- Memory Management: Efficiently manages memory with no leaks, ensuring optimal performance.
- Compliant with TOML v1.0.0: Fully adheres to the TOML specification for compatibility and reliability.

## Installation


1. Clone the Repository:

```bash
   git clone https://github.com/yourusername/toml-fp.git
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

## Quick Start

### Parsing TOML Strings
Parse TOML-formatted strings into TTOMLTable objects, allowing you to access and manipulate the data programmatically.

```pascal
uses
  TOML;

var
  TOMLData: TTOMLTable;
  Value: TTOMLValue;
begin
  TOMLData := ParseTOML('key = "Hello, World!"');
  try
    if TOMLData.TryGetValue('key', Value) then
      WriteLn('Value: ', Value.AsString)
    else
      WriteLn('Key not found.');
  finally
    TOMLData.Free;
  end;
end.
``` 

### Serializing TOML Data
Convert TTOMLTable objects back into TOML-formatted strings or save them to files.

```pascal
uses
  TOML;

var
  Table: TTOMLTable;
  TOMLString: string;
begin
  Table := TOMLTable;
  try
    Table.Add('key', TOMLString('value'));
    TOMLString := SerializeTOML(Table);
    WriteLn(TOMLString);
  finally
    Table.Free;
  end;
end.
```

### Handling Errors
The library uses specific exception classes to handle errors gracefully. It's essential to catch these exceptions to ensure your application can respond appropriately to parsing or serialization issues.

```pascal
uses
  TOML;

var
  TOMLData: TTOMLTable;
begin
  try
    TOMLData := ParseTOML('invalid = toml] content');
    try
      // Use the parsed data...
    finally
      TOMLData.Free;
    end;
  except
    on E: ETOMLParserException do
      WriteLn('Parsing error: ', E.Message);
    on E: ETOMLException do
      WriteLn('TOML error: ', E.Message);
  end;
end.
``` 

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




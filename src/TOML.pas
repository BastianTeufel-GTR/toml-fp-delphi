unit TOML;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, TOML.Types, TOML.Parser, TOML.Serializer;

type
  { Re-export main types }
  TTOMLValue = TOML.Types.TTOMLValue;
  TTOMLString = TOML.Types.TTOMLString;
  TTOMLInteger = TOML.Types.TTOMLInteger;
  TTOMLFloat = TOML.Types.TTOMLFloat;
  TTOMLBoolean = TOML.Types.TTOMLBoolean;
  TTOMLDateTime = TOML.Types.TTOMLDateTime;
  TTOMLArray = TOML.Types.TTOMLArray;
  TTOMLTable = TOML.Types.TTOMLTable;
  
  ETOMLException = TOML.Types.ETOMLException;
  ETOMLParserException = TOML.Types.ETOMLParserException;
  ETOMLSerializerException = TOML.Types.ETOMLSerializerException;

{ Helper functions for creating TOML values }
function TOMLString(const AValue: string): TTOMLString;
function TOMLInteger(const AValue: Int64): TTOMLInteger;
function TOMLFloat(const AValue: Double): TTOMLFloat;
function TOMLBoolean(const AValue: Boolean): TTOMLBoolean;
function TOMLDateTime(const AValue: TDateTime): TTOMLDateTime;
function TOMLArray: TTOMLArray;
function TOMLTable: TTOMLTable;

{ Parsing functions }
function ParseTOML(const ATOML: string): TTOMLTable;
function ParseTOMLFromFile(const AFileName: string): TTOMLTable;

{ Serialization functions }
function SerializeTOML(const AValue: TTOMLValue): string;
function SerializeTOMLToFile(const AValue: TTOMLValue; const AFileName: string): Boolean;

implementation

{ Helper functions }

function TOMLString(const AValue: string): TTOMLString;
begin
  Result := TTOMLString.Create(AValue);
end;

function TOMLInteger(const AValue: Int64): TTOMLInteger;
begin
  Result := TTOMLInteger.Create(AValue);
end;

function TOMLFloat(const AValue: Double): TTOMLFloat;
begin
  Result := TTOMLFloat.Create(AValue);
end;

function TOMLBoolean(const AValue: Boolean): TTOMLBoolean;
begin
  Result := TTOMLBoolean.Create(AValue);
end;

function TOMLDateTime(const AValue: TDateTime): TTOMLDateTime;
begin
  Result := TTOMLDateTime.Create(AValue);
end;

function TOMLArray: TTOMLArray;
begin
  Result := TTOMLArray.Create;
end;

function TOMLTable: TTOMLTable;
begin
  Result := TTOMLTable.Create;
end;

{ Parsing functions }

function ParseTOML(const ATOML: string): TTOMLTable;
begin
  Result := TOML.Parser.ParseTOMLString(ATOML);
end;

function ParseTOMLFromFile(const AFileName: string): TTOMLTable;
begin
  Result := TOML.Parser.ParseTOMLFile(AFileName);
end;

{ Serialization functions }

function SerializeTOML(const AValue: TTOMLValue): string;
begin
  Result := TOML.Serializer.SerializeTOML(AValue);
end;

function SerializeTOMLToFile(const AValue: TTOMLValue; const AFileName: string): Boolean;
begin
  Result := TOML.Serializer.SerializeTOMLToFile(AValue, AFileName);
end;

end. 
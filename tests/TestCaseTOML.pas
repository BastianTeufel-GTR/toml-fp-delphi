unit TestCaseTOML;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, TOML, fpcunit, testregistry;

type
  TTOMLTestCase = class(TTestCase)
  published
    { Basic Types Tests }
    procedure Test01_StringValue;
    procedure Test02_IntegerValue;
    procedure Test03_FloatValue;
    procedure Test04_BooleanTrueValue;
    procedure Test05_BooleanFalseValue;
    procedure Test06_DateTimeValue;
    
    { Array Tests }
    procedure Test10_IntegerArray;
    procedure Test11_StringArray;
    procedure Test12_MixedArrayInteger;
    procedure Test13_MixedArrayString;
    procedure Test14_MixedArrayBoolean;
    procedure Test15_EmptyArray;
    
    { Table Tests }
    procedure Test20_BasicTableString;
    procedure Test21_BasicTableInteger;
    procedure Test22_InlineTableString;
    procedure Test23_InlineTableInteger;
    procedure Test24_EmptyTable;
    procedure Test25_NestedTable;
    
    { Serialization Tests }
    procedure Test30_SerializeString;
    procedure Test31_SerializeInteger;
    procedure Test32_SerializeFloat;
    procedure Test33_SerializeBoolean;
    procedure Test34_SerializeArray;
    procedure Test35_SerializeTable;
    procedure Test36_SerializeNestedTable;
    
    { Error Cases }
    procedure Test40_InvalidInteger;
    procedure Test41_InvalidFloat;
    procedure Test42_InvalidDateTime;
    procedure Test43_InvalidTableKey;
    procedure Test44_DuplicateKey;
  end;

implementation

procedure TTOMLTestCase.Test01_StringValue;
var
  Data: TTOMLTable;
  Value: TTOMLValue;
begin
  Data := ParseTOML('key = "Hello, World!"');
  try
    AssertTrue('String value exists', Data.TryGetValue('key', Value));
    AssertEquals('String value matches', 'Hello, World!', Value.AsString);
  finally
    Data.Free;
  end;
end;

procedure TTOMLTestCase.Test02_IntegerValue;
var
  Data: TTOMLTable;
  Value: TTOMLValue;
begin
  Data := ParseTOML('key = 42');
  try
    AssertTrue('Integer value exists', Data.TryGetValue('key', Value));
    AssertEquals('Integer value matches', 42, Value.AsInteger);
  finally
    Data.Free;
  end;
end;

procedure TTOMLTestCase.Test03_FloatValue;
var
  Data: TTOMLTable;
  Value: TTOMLValue;
begin
  Data := ParseTOML('key = 3.14');
  try
    AssertTrue('Float value exists', Data.TryGetValue('key', Value));
    AssertEquals('Float value matches', 3.14, Value.AsFloat, 0.0001);
  finally
    Data.Free;
  end;
end;

procedure TTOMLTestCase.Test04_BooleanTrueValue;
var
  Data: TTOMLTable;
  Value: TTOMLValue;
begin
  Data := ParseTOML('key = true');
  try
    AssertTrue('Boolean value exists', Data.TryGetValue('key', Value));
    AssertTrue('Boolean value is true', Value.AsBoolean);
  finally
    Data.Free;
  end;
end;

procedure TTOMLTestCase.Test05_BooleanFalseValue;
var
  Data: TTOMLTable;
  Value: TTOMLValue;
begin
  Data := ParseTOML('key = false');
  try
    AssertTrue('Boolean value exists', Data.TryGetValue('key', Value));
    AssertFalse('Boolean value is false', Value.AsBoolean);
  finally
    Data.Free;
  end;
end;

procedure TTOMLTestCase.Test06_DateTimeValue;
var
  Data: TTOMLTable;
  Value: TTOMLValue;
  ExpectedDate: TDateTime;
begin
  Data := ParseTOML('key = 2023-01-01T12:00:00Z');
  try
    AssertTrue('DateTime value exists', Data.TryGetValue('key', Value));
    ExpectedDate := EncodeDate(2023, 1, 1) + EncodeTime(12, 0, 0, 0);
    AssertEquals('DateTime value matches', ExpectedDate, Value.AsDateTime);
  finally
    Data.Free;
  end;
end;

procedure TTOMLTestCase.Test10_IntegerArray;
var
  Data: TTOMLTable;
  Value: TTOMLValue;
begin
  Data := ParseTOML('key = [1, 2, 3]');
  try
    AssertTrue('Array exists', Data.TryGetValue('key', Value));
    AssertEquals('Array has correct size', 3, Value.AsArray.Count);
    AssertEquals('First element matches', 1, Value.AsArray.GetItem(0).AsInteger);
  finally
    Data.Free;
  end;
end;

procedure TTOMLTestCase.Test11_StringArray;
var
  Data: TTOMLTable;
  Value: TTOMLValue;
begin
  Data := ParseTOML('key = ["a", "b", "c"]');
  try
    AssertTrue('Array exists', Data.TryGetValue('key', Value));
    AssertEquals('Array has correct size', 3, Value.AsArray.Count);
    AssertEquals('First element matches', 'a', Value.AsArray.GetItem(0).AsString);
  finally
    Data.Free;
  end;
end;

procedure TTOMLTestCase.Test12_MixedArrayInteger;
var
  Data: TTOMLTable;
  Value: TTOMLValue;
begin
  Data := ParseTOML('key = [1, "two", true]');
  try
    AssertTrue('Array exists', Data.TryGetValue('key', Value));
    AssertEquals('First element is integer', 1, Value.AsArray.GetItem(0).AsInteger);
  finally
    Data.Free;
  end;
end;

procedure TTOMLTestCase.Test13_MixedArrayString;
var
  Data: TTOMLTable;
  Value: TTOMLValue;
begin
  Data := ParseTOML('key = [1, "two", true]');
  try
    AssertTrue('Array exists', Data.TryGetValue('key', Value));
    AssertEquals('Second element is string', 'two', Value.AsArray.GetItem(1).AsString);
  finally
    Data.Free;
  end;
end;

procedure TTOMLTestCase.Test14_MixedArrayBoolean;
var
  Data: TTOMLTable;
  Value: TTOMLValue;
begin
  Data := ParseTOML('key = [1, "two", true]');
  try
    AssertTrue('Array exists', Data.TryGetValue('key', Value));
    AssertTrue('Third element is boolean true', Value.AsArray.GetItem(2).AsBoolean);
  finally
    Data.Free;
  end;
end;

procedure TTOMLTestCase.Test15_EmptyArray;
var
  Data: TTOMLTable;
  Value: TTOMLValue;
begin
  Data := ParseTOML('key = []');
  try
    AssertTrue('Array exists', Data.TryGetValue('key', Value));
    AssertEquals('Array is empty', 0, Value.AsArray.Count);
  finally
    Data.Free;
  end;
end;

procedure TTOMLTestCase.Test20_BasicTableString;
var
  Data: TTOMLTable;
  Value: TTOMLValue;
begin
  Data := ParseTOML('[server]' + LineEnding + 'host = "localhost"');
  try
    AssertTrue('Table exists', Data.TryGetValue('server', Value));
    AssertTrue('Host value exists', Value.AsTable.TryGetValue('host', Value));
    AssertEquals('Host value matches', 'localhost', Value.AsString);
  finally
    Data.Free;
  end;
end;

procedure TTOMLTestCase.Test21_BasicTableInteger;
var
  Data: TTOMLTable;
  Value: TTOMLValue;
begin
  Data := ParseTOML('[server]' + LineEnding + 'port = 8080');
  try
    AssertTrue('Table exists', Data.TryGetValue('server', Value));
    AssertTrue('Port value exists', Value.AsTable.TryGetValue('port', Value));
    AssertEquals('Port value matches', 8080, Value.AsInteger);
  finally
    Data.Free;
  end;
end;

procedure TTOMLTestCase.Test22_InlineTableString;
var
  Data: TTOMLTable;
  Value: TTOMLValue;
begin
  Data := ParseTOML('server = { host = "localhost" }');
  try
    AssertTrue('Table exists', Data.TryGetValue('server', Value));
    AssertTrue('Host value exists', Value.AsTable.TryGetValue('host', Value));
    AssertEquals('Host value matches', 'localhost', Value.AsString);
  finally
    Data.Free;
  end;
end;

procedure TTOMLTestCase.Test23_InlineTableInteger;
var
  Data: TTOMLTable;
  Value: TTOMLValue;
begin
  Data := ParseTOML('server = { port = 8080 }');
  try
    AssertTrue('Table exists', Data.TryGetValue('server', Value));
    AssertTrue('Port value exists', Value.AsTable.TryGetValue('port', Value));
    AssertEquals('Port value matches', 8080, Value.AsInteger);
  finally
    Data.Free;
  end;
end;

procedure TTOMLTestCase.Test24_EmptyTable;
var
  Data: TTOMLTable;
  Value: TTOMLValue;
begin
  Data := ParseTOML('[empty]');
  try
    AssertTrue('Table exists', Data.TryGetValue('empty', Value));
    AssertEquals('Table is empty', 0, Value.AsTable.Items.Count);
  finally
    Data.Free;
  end;
end;

procedure TTOMLTestCase.Test25_NestedTable;
var
  Data: TTOMLTable;
  Value, SubValue: TTOMLValue;
begin
  Data := ParseTOML('[server.config]' + LineEnding + 'enabled = true');
  try
    AssertTrue('Server table exists', Data.TryGetValue('server', Value));
    AssertTrue('Config table exists', Value.AsTable.TryGetValue('config', SubValue));
    AssertTrue('Enabled value exists', SubValue.AsTable.TryGetValue('enabled', Value));
    AssertTrue('Enabled value matches', Value.AsBoolean);
  finally
    Data.Free;
  end;
end;

procedure TTOMLTestCase.Test30_SerializeString;
var
  Table: TTOMLTable;
  TOML: string;
begin
  Table := TOMLTable;
  try
    Table.Add('key', TOMLString('value'));
    TOML := SerializeTOML(Table);
    AssertEquals('Serialized string matches', 'key = "value"' + LineEnding, TOML);
  finally
    Table.Free;
  end;
end;

procedure TTOMLTestCase.Test31_SerializeInteger;
var
  Table: TTOMLTable;
  TOML: string;
begin
  Table := TOMLTable;
  try
    Table.Add('key', TOMLInteger(42));
    TOML := SerializeTOML(Table);
    AssertEquals('Serialized integer matches', 'key = 42' + LineEnding, TOML);
  finally
    Table.Free;
  end;
end;

procedure TTOMLTestCase.Test32_SerializeFloat;
var
  Table: TTOMLTable;
  TOML: string;
begin
  Table := TOMLTable;
  try
    Table.Add('key', TOMLFloat(3.14));
    TOML := SerializeTOML(Table);
    AssertEquals('Serialized float matches', 'key = 3.14' + LineEnding, TOML);
  finally
    Table.Free;
  end;
end;

procedure TTOMLTestCase.Test33_SerializeBoolean;
var
  Table: TTOMLTable;
  TOML: string;
begin
  Table := TOMLTable;
  try
    Table.Add('key', TOMLBoolean(True));
    TOML := SerializeTOML(Table);
    AssertEquals('Serialized boolean matches', 'key = true' + LineEnding, TOML);
  finally
    Table.Free;
  end;
end;

procedure TTOMLTestCase.Test34_SerializeArray;
var
  Table: TTOMLTable;
  ArrValue: TTOMLArray;
  TOML: string;
begin
  Table := TOMLTable;
  try
    ArrValue := TOMLArray;
    ArrValue.Add(TOMLInteger(1));
    ArrValue.Add(TOMLInteger(2));
    Table.Add('key', ArrValue);
    TOML := SerializeTOML(Table);
    AssertEquals('Serialized array matches', 'key = [1, 2]' + LineEnding, TOML);
  finally
    Table.Free;
  end;
end;

procedure TTOMLTestCase.Test35_SerializeTable;
var
  Table: TTOMLTable;
  InnerTable: TTOMLTable;
  TOML: string;
begin
  Table := TOMLTable;
  try
    InnerTable := TOMLTable;
    InnerTable.Add('inner', TOMLString('value'));
    Table.Add('outer', InnerTable);
    TOML := SerializeTOML(Table);
    AssertEquals('Serialized table matches', LineEnding + '[outer]' + LineEnding + 'inner = "value"' + LineEnding, TOML);
  finally
    Table.Free;
  end;
end;

procedure TTOMLTestCase.Test36_SerializeNestedTable;
var
  Table: TTOMLTable;
  InnerTable: TTOMLTable;
  TOML: string;
begin
  Table := TOMLTable;
  try
    InnerTable := TOMLTable;
    InnerTable.Add('key', TOMLString('value'));
    Table.Add('table.nested', InnerTable);
    TOML := SerializeTOML(Table);
    AssertEquals('Serialized nested table matches', LineEnding + '[table.nested]' + LineEnding + 'key = "value"' + LineEnding, TOML);
  finally
    Table.Free;
  end;
end;

procedure TTOMLTestCase.Test40_InvalidInteger;
begin
  try
    ParseTOML('key = 12.34.56');
    Fail('Should raise ETOMLParserException');
  except
    on E: ETOMLParserException do
      ; // Test passes
  end;
end;

procedure TTOMLTestCase.Test41_InvalidFloat;
begin
  try
    ParseTOML('key = 3.14.15');
    Fail('Should raise ETOMLParserException');
  except
    on E: ETOMLParserException do
      ; // Test passes
  end;
end;

procedure TTOMLTestCase.Test42_InvalidDateTime;
begin
  try
    ParseTOML('key = 2023-13-32T25:61:61Z');
    Fail('Should raise ETOMLParserException');
  except
    on E: ETOMLParserException do
      ; // Test passes
  end;
end;

procedure TTOMLTestCase.Test43_InvalidTableKey;
begin
  try
    ParseTOML('[invalid.key!]');
    Fail('Should raise ETOMLParserException');
  except
    on E: ETOMLParserException do
      ; // Test passes
  end;
end;

procedure TTOMLTestCase.Test44_DuplicateKey;
begin
  try
    ParseTOML('key = "first"' + LineEnding + 'key = "second"');
    Fail('Should raise ETOMLParserException');
  except
    on E: ETOMLParserException do
      ; // Test passes
  end;
end;

initialization
  RegisterTest(TTOMLTestCase);
end. 
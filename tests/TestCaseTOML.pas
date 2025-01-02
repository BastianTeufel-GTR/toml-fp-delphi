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
    
    { TOML v1.0.0 Specification Tests }
    procedure Test50_MultilineString;
    procedure Test51_LiteralString;
    procedure Test52_MultilineLiteralString;
    procedure Test53_IntegerWithUnderscores;
    procedure Test54_HexOctBinIntegers;
    procedure Test55_FloatWithUnderscores;
    procedure Test56_LocalDateTime;
    procedure Test57_LocalDate;
    procedure Test58_LocalTime;
    procedure Test59_ArrayOfTables;
    procedure Test60_DottedTableArray;
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

procedure TTOMLTestCase.Test50_MultilineString;
var
  TOML: string;
  Doc: TTOMLTable;
  Value: TTOMLValue;
begin
  TOML := 'str1 = """' + LineEnding +
          'Roses are red' + LineEnding +
          'Violets are blue"""' + LineEnding;
  Doc := ParseTOML(TOML);
  try
    AssertTrue('String value exists', Doc.TryGetValue('str1', Value));
    AssertEquals('Roses are red' + LineEnding + 'Violets are blue', 
      Value.AsString);
  finally
    Doc.Free;
  end;
end;

procedure TTOMLTestCase.Test51_LiteralString;
var
  TOML: string;
  Doc: TTOMLTable;
  Value: TTOMLValue;
begin
  TOML := 'winpath = ''C:\Users\nodejs\templates''' + LineEnding +
          'winpath2 = ''\\ServerX\admin$\system32\''' + LineEnding;
  Doc := ParseTOML(TOML);
  try
    AssertTrue('First path exists', Doc.TryGetValue('winpath', Value));
    AssertEquals('C:\Users\nodejs\templates', Value.AsString);
    AssertTrue('Second path exists', Doc.TryGetValue('winpath2', Value));
    AssertEquals('\\ServerX\admin$\system32\', Value.AsString);
  finally
    Doc.Free;
  end;
end;

procedure TTOMLTestCase.Test52_MultilineLiteralString;
var
  TOML: string;
  Doc: TTOMLTable;
  Value: TTOMLValue;
begin
  TOML := 'regex = ''''''I [dw]on''t need \d{2} apples''''''';
  Doc := ParseTOML(TOML);
  try
    AssertTrue('Regex exists', Doc.TryGetValue('regex', Value));
    AssertEquals('I [dw]on''t need \d{2} apples', Value.AsString);
  finally
    Doc.Free;
  end;
end;

procedure TTOMLTestCase.Test53_IntegerWithUnderscores;
var
  TOML: string;
  Doc: TTOMLTable;
  Value: TTOMLValue;
begin
  TOML := 'int1 = 1_000' + LineEnding +
          'int2 = 5_349_221' + LineEnding +
          'int3 = 1_2_3_4_5' + LineEnding;
  Doc := ParseTOML(TOML);
  try
    AssertTrue('First integer exists', Doc.TryGetValue('int1', Value));
    AssertEquals(1000, Value.AsInteger);
    AssertTrue('Second integer exists', Doc.TryGetValue('int2', Value));
    AssertEquals(5349221, Value.AsInteger);
    AssertTrue('Third integer exists', Doc.TryGetValue('int3', Value));
    AssertEquals(12345, Value.AsInteger);
  finally
    Doc.Free;
  end;
end;

procedure TTOMLTestCase.Test54_HexOctBinIntegers;
var
  TOML: string;
  Doc: TTOMLTable;
  Value: TTOMLValue;
begin
  TOML := 'hex = 0xDEADBEEF' + LineEnding +
          'oct = 0o755' + LineEnding +
          'bin = 0b11010110' + LineEnding;
  Doc := ParseTOML(TOML);
  try
    AssertTrue('Hex exists', Doc.TryGetValue('hex', Value));
    AssertEquals(3735928559, Value.AsInteger);
    AssertTrue('Oct exists', Doc.TryGetValue('oct', Value));
    AssertEquals(493, Value.AsInteger);
    AssertTrue('Bin exists', Doc.TryGetValue('bin', Value));
    AssertEquals(214, Value.AsInteger);
  finally
    Doc.Free;
  end;
end;

procedure TTOMLTestCase.Test55_FloatWithUnderscores;
var
  TOML: string;
  Doc: TTOMLTable;
  Value: TTOMLValue;
begin
  TOML := 'float1 = 1_000.000_001' + LineEnding +
          'float2 = 1e1_0' + LineEnding;
  Doc := ParseTOML(TOML);
  try
    AssertTrue('First float exists', Doc.TryGetValue('float1', Value));
    AssertEquals(1000.000001, Value.AsFloat, 0.000001);
    AssertTrue('Second float exists', Doc.TryGetValue('float2', Value));
    AssertEquals(1e10, Value.AsFloat, 0.0);
  finally
    Doc.Free;
  end;
end;

procedure TTOMLTestCase.Test56_LocalDateTime;
var
  TOML: string;
  Doc: TTOMLTable;
  Value: TTOMLValue;
begin
  TOML := 'ldt1 = 1979-05-27T07:32:00' + LineEnding +
          'ldt2 = 1979-05-27T00:32:00.999999' + LineEnding;
  Doc := ParseTOML(TOML);
  try
    AssertTrue('First datetime exists', Doc.TryGetValue('ldt1', Value));
    AssertEquals('1979-05-27T07:32:00', 
      FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Value.AsDateTime));
    AssertTrue('Second datetime exists', Doc.TryGetValue('ldt2', Value));
    AssertEquals('1979-05-27T00:32:00.999', 
      FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', Value.AsDateTime));
  finally
    Doc.Free;
  end;
end;

procedure TTOMLTestCase.Test57_LocalDate;
var
  TOML: string;
  Doc: TTOMLTable;
  Value: TTOMLValue;
begin
  TOML := 'date1 = 1979-05-27' + LineEnding;
  Doc := ParseTOML(TOML);
  try
    AssertTrue('Date exists', Doc.TryGetValue('date1', Value));
    AssertEquals('1979-05-27', FormatDateTime('yyyy-mm-dd', Value.AsDateTime));
  finally
    Doc.Free;
  end;
end;

procedure TTOMLTestCase.Test58_LocalTime;
var
  TOML: string;
  Doc: TTOMLTable;
  Value: TTOMLValue;
  ExpectedTime: TDateTime;
begin
  TOML := 'time1 = 07:32:00' + LineEnding +
          'time2 = 00:32:00.999' + LineEnding;
  Doc := ParseTOML(TOML);
  try
    AssertTrue('First time exists', Doc.TryGetValue('time1', Value));
    ExpectedTime := EncodeTime(7, 32, 0, 0);
    AssertEquals('First time matches', ExpectedTime, Frac(Value.AsDateTime));
    
    AssertTrue('Second time exists', Doc.TryGetValue('time2', Value));
    ExpectedTime := EncodeTime(0, 32, 0, 999);
    AssertEquals('Second time matches', ExpectedTime, Frac(Value.AsDateTime));
  finally
    Doc.Free;
  end;
end;

procedure TTOMLTestCase.Test59_ArrayOfTables;
var
  TOML: string;
  Doc: TTOMLTable;
  Value: TTOMLValue;
  Products: TTOMLArray;
  ProductTable: TTOMLTable;
begin
  TOML := '[[products]]' + LineEnding +
          'name = "Hammer"' + LineEnding +
          'sku = 738594937' + LineEnding +
          '' + LineEnding +
          '[[products]]' + LineEnding +
          'name = "Nail"' + LineEnding +
          'sku = 284758393' + LineEnding +
          'color = "gray"' + LineEnding;
  Doc := ParseTOML(TOML);
  try
    AssertTrue('Products array exists', Doc.TryGetValue('products', Value));
    Products := Value.AsArray;
    AssertEquals(2, Products.Count);
    
    ProductTable := Products.Items[0].AsTable;
    AssertTrue('First product name exists', ProductTable.TryGetValue('name', Value));
    AssertEquals('Hammer', Value.AsString);
    AssertTrue('First product sku exists', ProductTable.TryGetValue('sku', Value));
    AssertEquals(738594937, Value.AsInteger);
    
    ProductTable := Products.Items[1].AsTable;
    AssertTrue('Second product name exists', ProductTable.TryGetValue('name', Value));
    AssertEquals('Nail', Value.AsString);
    AssertTrue('Second product sku exists', ProductTable.TryGetValue('sku', Value));
    AssertEquals(284758393, Value.AsInteger);
    AssertTrue('Second product color exists', ProductTable.TryGetValue('color', Value));
    AssertEquals('gray', Value.AsString);
  finally
    Doc.Free;
  end;
end;

procedure TTOMLTestCase.Test60_DottedTableArray;
var
  TOML: string;
  Doc: TTOMLTable;
  Value, SubValue: TTOMLValue;
  Fruits: TTOMLArray;
  FruitTable: TTOMLTable;
  Varieties: TTOMLArray;
begin
  TOML := '[[fruits]]' + LineEnding +
          'name = "apple"' + LineEnding +
          'physical = { color = "red", shape = "round" }' + LineEnding +
          '' + LineEnding +
          '[[fruits.varieties]]' + LineEnding +
          'name = "red delicious"' + LineEnding +
          '' + LineEnding +
          '[[fruits.varieties]]' + LineEnding +
          'name = "granny smith"' + LineEnding;
  Doc := ParseTOML(TOML);
  try
    AssertTrue('Fruits array exists', Doc.TryGetValue('fruits', Value));
    Fruits := Value.AsArray;
    AssertEquals(1, Fruits.Count);
    
    FruitTable := Fruits.Items[0].AsTable;
    AssertTrue('Fruit name exists', FruitTable.TryGetValue('name', Value));
    AssertEquals('apple', Value.AsString);
    
    AssertTrue('Physical table exists', FruitTable.TryGetValue('physical', Value));
    AssertTrue('Physical color exists', Value.AsTable.TryGetValue('color', SubValue));
    AssertEquals('red', SubValue.AsString);
    AssertTrue('Physical shape exists', Value.AsTable.TryGetValue('shape', SubValue));
    AssertEquals('round', SubValue.AsString);
    
    AssertTrue('Varieties array exists', FruitTable.TryGetValue('varieties', Value));
    Varieties := Value.AsArray;
    AssertEquals(2, Varieties.Count);
    
    AssertTrue('First variety name exists', Varieties.Items[0].AsTable.TryGetValue('name', Value));
    AssertEquals('red delicious', Value.AsString);
    AssertTrue('Second variety name exists', Varieties.Items[1].AsTable.TryGetValue('name', Value));
    AssertEquals('granny smith', Value.AsString);
  finally
    Doc.Free;
  end;
end;

initialization
  RegisterTest(TTOMLTestCase);
end. 
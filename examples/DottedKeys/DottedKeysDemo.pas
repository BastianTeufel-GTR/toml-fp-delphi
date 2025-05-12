program DottedKeysDemo;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils, TOML;

var
  Doc, FruitTable, AppleTable, DogTable, TaterManTable: TTOMLTable;
  SerializedTOML: string;
  Value: TTOMLValue;

begin
  Doc := TOMLTable;
  try
    // Example 1: Hierarchical nested tables
    // This creates a real hierarchical structure with tables and subtables
    WriteLn('Example 1: Hierarchical Nested Tables');
    WriteLn('-------------------------------------');
    
    // Create nested structure: fruit.banana
    AppleTable := TOMLTable;
    AppleTable.Add('color', TOMLString('yellow'));
    
    FruitTable := TOMLTable;
    FruitTable.Add('banana', AppleTable);
    
    Doc.Add('fruit', FruitTable);
    SerializedTOML := SerializeTOML(Doc);
    WriteLn(SerializedTOML);
    
    // Clear for next example
    Doc.Free;
    Doc := TOMLTable;
    
    // Example 2: Table with literal dotted key
    // This creates a table with a single key that contains dots
    WriteLn;
    WriteLn('Example 2: Table with Literal Dotted Key');
    WriteLn('--------------------------------------');
    
    AppleTable := TOMLTable;
    AppleTable.Add('color', TOMLString('red'));
    
    // 'fruit.apple' is a single key with a dot, not a hierarchical path
    Doc.Add('fruit.apple', AppleTable);
    SerializedTOML := SerializeTOML(Doc);
    WriteLn(SerializedTOML);
    
    // Example 3: Parsing a table with literal dotted key
    WriteLn;
    WriteLn('Example 3: Parsing a Table with Literal Dotted Key');
    WriteLn('----------------------------------------------');
    WriteLn('Sample TOML content:');
    WriteLn('["fruit.apple"]');
    WriteLn('color = "green"');
    WriteLn;
    
    // Parse TOML with a literal dotted key
    Doc.Free;
    Doc := ParseTOML('["fruit.apple"]' + LineEnding + 'color = "green"');
    
    WriteLn('Accessing the table with Doc.TryGetValue("fruit.apple", Value):');
    if Doc.TryGetValue('fruit.apple', Value) then
    begin
      WriteLn('- Table found with literal key "fruit.apple"');

      if Value.AsTable.TryGetValue('color', Value) then
        WriteLn('- Color: ', Value.AsString)
      else
        WriteLn('- Color: not found');
    end
    else
      WriteLn('- Table not found');
    
    // Example 4: Nested table with dotted key component
    WriteLn;
    WriteLn('Example 4: Nested Table with Dotted Key Component');
    WriteLn('----------------------------------------------');
    WriteLn('This demonstrates a table path where one segment contains dots');
    WriteLn('TOML Spec: dog."tater.man" should be serialized as [dog."tater.man"]');
    WriteLn;
    
    Doc.Free;
    Doc := TOMLTable;
    DogTable := TOMLTable;
    
    // Create a table with key "tater.man" under the "dog" table
    // This creates a nested structure where a component has a dot in its name
    TaterManTable := TOMLTable;
    TaterManTable.Add('type', TOMLString('pug')); // Add some content
    DogTable.Add('tater.man', TaterManTable);  // This key has a dot and should be quoted
    Doc.Add('dog', DogTable);
    
    SerializedTOML := SerializeTOML(Doc);
    WriteLn('Serialized Output:');
    WriteLn(SerializedTOML);
    
    // Parse it back to verify
    Doc.Free;
    Doc := ParseTOML(SerializedTOML);
    WriteLn('Parsing back and verifying:');
    
    // Replace AssertTrue with if-then checks
    if Doc.TryGetValue('dog', Value) then
      WriteLn('- DogTable exists')
    else
      WriteLn('- ERROR: DogTable not found!');
      
    if Value.AsTable.TryGetValue('tater.man', Value) then
      WriteLn('- tater.man table exists')
    else  
      WriteLn('- ERROR: tater.man table not found!');
      
    WriteLn('- Successfully parsed nested table with dotted key component');
    
  finally
    Doc.Free;
  end;
  
  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.

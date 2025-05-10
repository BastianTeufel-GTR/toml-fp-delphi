program NestedTablesDemo;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils, TOML.Types, TOML.Serializer;

var
  RootTable, FruitTable, BananaTable, AppleTable: TTOMLTable;
  TOML: string;

begin
  // Create nested tables structure
  RootTable := TTOMLTable.Create;
  try
    // Create banana subtable
    BananaTable := TTOMLTable.Create;
    BananaTable.Add('color', TTOMLString.Create('yellow'));
    BananaTable.Add('ripeness', TTOMLString.Create('ripe'));
    
    // Create apple subtable
    AppleTable := TTOMLTable.Create;
    AppleTable.Add('color', TTOMLString.Create('red'));
    AppleTable.Add('variety', TTOMLString.Create('gala'));
    
    // Add both to fruit table
    FruitTable := TTOMLTable.Create;
    FruitTable.Add('banana', BananaTable);
    FruitTable.Add('apple', AppleTable);
    
    // Add fruit table to root
    RootTable.Add('fruit', FruitTable);
    
    // Add some root-level values too
    RootTable.Add('title', TTOMLString.Create('Fruit Catalog'));
    RootTable.Add('version', TTOMLInteger.Create(2));
    
    // Serialize and display
    TOML := SerializeTOML(RootTable);
    Writeln('TOML Output:');
    Writeln('------------');
    Writeln(TOML);
    Writeln('------------');
    
  finally
    RootTable.Free;
  end;

  // Pause console
  ReadLn;

end.

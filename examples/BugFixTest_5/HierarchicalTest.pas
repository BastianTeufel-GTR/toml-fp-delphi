program HierarchicalTest;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils, TOML.Types, TOML.Serializer;

var
  t1, t2, t3: TTOMLTable;
  s1, s2: string;
  Serializer: TTOMLSerializer;

begin
  // Create nested structure
  t1 := TTOMLTable.Create;
  t2 := TTOMLTable.Create;
  t3 := TTOMLTable.Create;
  
  try
    // Build the hierarchical structure
    t1.Add('color', TTOMLString.Create('yellow'));
    t2.Add('banana', t1);
    t3.Add('fruit', t2);
    
    // Method 1: Using TTOMLSerializer directly
    begin
      Serializer := TTOMLSerializer.Create;
      try
        s1 := Serializer.Serialize(t3);
      finally
        Serializer.Free;
      end;
    end;
    
    // Method 2: Using the helper function (recommended approach)
    s2 := SerializeTOML(t3);
    
    // Show both results are identical
    Writeln('Method 1 (Direct serializer):');
    Writeln('----------------------------');
    Writeln(s1);
    Writeln;
    Writeln('Method 2 (Helper function):');
    Writeln('----------------------------');
    Writeln(s2);
    
    // Validate output format
    if Pos('[fruit.banana]', s1) > 0 then
      Writeln('SUCCESS: Hierarchical table format is correct')
    else
      Writeln('ERROR: Expected [fruit.banana] format but got something else');
      
  finally
    // Proper cleanup - t3 owns t2 which owns t1, so just free t3
    t3.Free;
  end;

  //Pause console
  ReadLn;

end.

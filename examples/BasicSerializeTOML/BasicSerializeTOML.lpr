program BasicSerializeTOML;

{$mode objfpc}{$H+}{$J-}

{ 
  This example demonstrates how to create and save a TOML configuration file
  using the TOML library. It shows:
  - Creating nested TOML tables
  - Adding string and integer values
  - Serializing to a file 
  
  Visual representation of the TOML data structure:
  Config (TTOMLTable)
  │
  ├── database (TTOMLTable)
      ├── host = "localhost" (TTOMLString)
      └── port = 5432 (TTOMLInteger)  
  
}

uses
  TOML;

var
  Config: TTOMLTable;    // Root TOML table
  Database: TTOMLTable;  // Nested table for database settings

begin
  // Create root TOML table
  Config := TOMLTable;
  try
    // Create nested database table
    Database := TOMLTable;
    
    // Add database connection settings
    Database.Add('host', TOMLString('localhost'));  // Host as string
    Database.Add('port', TOMLInteger(5432));        // Port as integer
    
    // Add database table to root config
    Config.Add('database', Database);

    // Save TOML to file
    if SerializeTOMLToFile(Config, 'config.toml') then
      WriteLn('Configuration saved successfully')
    else
      WriteLn('Error saving configuration');
  finally
    // Free the TOML table to prevent memory leaks (frees database TOMLTable as well)
    Config.Free;
  end;
end.

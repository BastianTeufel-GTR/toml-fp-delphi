program ComplexSerializeTOML;

{$mode objfpc}{$H+}{$J-}

{ 
  This example demonstrates advanced TOML serialization features including:
  - Creating nested tables
  - Creating and populating arrays
  - Using different TOML data types (string, boolean, integer, float, datetime)
  - Serializing to both string and file
  
  The resulting TOML structure will look like:
  
  version = 1.0
  last_updated = <current-datetime>
  
  [server]
  host = "127.0.0.1"
  enabled = true
  ports = [ 80, 443 ]
}

uses
  TOML, SysUtils;

var
  Config, ServerConfig: TTOMLTable;    // TOML tables for configuration
  Ports: TTOMLArray;                   // Array to hold port numbers
  SerializedTOML: string;             // Will hold the generated TOML text
begin
  // Create root TOML table
  Config := TOMLTable;
  try
    // Create a nested table for server configuration
    ServerConfig := TOMLTable;
    ServerConfig.Add('host', TOMLString('127.0.0.1'));     // Server hostname
    ServerConfig.Add('enabled', TOMLBoolean(True));        // Server enabled flag

    // Create and populate an array of port numbers
    Ports := TOMLArray;
    Ports.Add(TOMLInteger(80));    // HTTP port
    Ports.Add(TOMLInteger(443));   // HTTPS port
    ServerConfig.Add('ports', Ports);

    // Add the server configuration table to main config
    Config.Add('server', ServerConfig);

    // Add top-level metadata
    Config.Add('version', TOMLFloat(1.0));                 // Software version
    Config.Add('last_updated', TOMLDateTime(Now));         // Current timestamp

    // Convert the TOML structure to a string
    SerializedTOML := SerializeTOML(Config);
    WriteLn('Generated TOML:');
    WriteLn(SerializedTOML);

    // Save the TOML structure to a file
    // SerializeTOMLToFile returns boolean indicating success/failure
    if SerializeTOMLToFile(Config, 'config.toml') then
      WriteLn('Successfully saved to file')
    else
      WriteLn('Error saving to file');

  finally
    // Free the root table (this also frees all nested tables and values)
    Config.Free;
  end;
end.

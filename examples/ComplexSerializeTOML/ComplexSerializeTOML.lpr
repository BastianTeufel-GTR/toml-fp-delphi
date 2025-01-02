program ComplexSerializeTOML;

{$mode objfpc}{$H+}{$J-}

uses
  TOML, SysUtils;

var
  Config, ServerConfig: TTOMLTable;
  Ports: TTOMLArray;
  SerializedTOML: string;
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
    SerializedTOML := SerializeTOML(Config);
    WriteLn('Generated TOML:');
    WriteLn(SerializedTOML);

    // Save to file
    if SerializeTOMLToFile(Config, 'config.toml') then
      WriteLn('Successfully saved to file')
    else
      WriteLn('Error saving to file');

  finally
    Config.Free;
  end;
end.

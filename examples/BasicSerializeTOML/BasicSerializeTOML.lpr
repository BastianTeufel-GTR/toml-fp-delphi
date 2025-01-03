program BasicSerializeTOML;

{$mode objfpc}{$H+}{$J-}

uses
  TOML;

var
  Config: TTOMLTable;
  Database: TTOMLTable;

begin
  Config := TOMLTable;
  try
    Database := TOMLTable;
    Database.Add('host', TOMLString('localhost'));
    Database.Add('port', TOMLInteger(5432));
    Config.Add('database', Database);

    if SerializeTOMLToFile(Config, 'config.toml') then
      WriteLn('Configuration saved successfully')
    else
      WriteLn('Error saving configuration');
  finally
    Config.Free;
  end;
end.

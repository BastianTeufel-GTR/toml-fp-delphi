program BasicParseTOML;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, TOML;

var
  Config: TTOMLTable;
  ProjectValue: TTOMLValue;
  ProjectTable: TTOMLTable;
  ProjectName: TTOMLValue;
begin
  // Parse TOML from file
  Config := ParseTOMLFromFile('config.toml');
  try
    // Access nested values safely
    if Config.TryGetValue('project', ProjectValue) and
       (ProjectValue is TTOMLTable) then
    begin
      ProjectTable := TTOMLTable(ProjectValue);
      if ProjectTable.TryGetValue('name', ProjectName) then
        WriteLn('Project Name: ', ProjectName.AsString)
      else
        WriteLn('Project name not found');
    end
    else
      WriteLn('Project configuration not found');
  finally
    Config.Free;
  end;
end.


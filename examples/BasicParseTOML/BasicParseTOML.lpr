program BasicParseTOML;

{$mode objfpc}{$H+}{$J-}

uses
  TOML;

var
  Config: TTOMLTable;
  RevisionValue, ProjectValue, ProjectName: TTOMLValue;
  ProjectTable: TTOMLTable;

begin
  // Parse TOML from file
  Config := ParseTOMLFromFile('config.toml');
  try

    // Access a string value
    if (Config.TryGetValue('revision', RevisionValue)) then
      WriteLn('The value of ''revision'' is ', RevisionValue.AsString);

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

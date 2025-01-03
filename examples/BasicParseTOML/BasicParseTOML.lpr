program BasicParseTOML;

{$mode objfpc}{$H+}{$J-}

{ 
  This example demonstrates how to parse and read a TOML configuration file.
  It shows:
  - Loading a TOML file
  - Safely accessing top-level values
  - Safely accessing nested values in tables
  - Type checking and type casting
  - Proper resource cleanup
  
  Expected TOML file structure (config.toml):
  
  ```toml
  # config.toml

  revision = "1.2.1af"

  [project]
  name = "My Amazing Project"
  version = "1.0.0"
  ```
}

uses
  TOML;

var
  Config: TTOMLTable;                    // Root TOML table from file
  RevisionValue,                         // Holds the 'revision' value
  ProjectValue,                          // Holds the 'project' table value
  ProjectName: TTOMLValue;               // Holds the project 'name' value
  ProjectTable: TTOMLTable;              // Typed reference to project table

begin
  // Parse TOML from file into a TTOMLTable structure
  Config := ParseTOMLFromFile('config.toml');
  try
    // Access a top-level string value using TryGetValue for safety
    // TryGetValue returns false if the key doesn't exist
    if (Config.TryGetValue('revision', RevisionValue)) then
      WriteLn('The value of ''revision'' is ', RevisionValue.AsString)
    else
      WriteLn('Revision value not found');

    // Access nested values safely with type checking
    if Config.TryGetValue('project', ProjectValue) and
      (ProjectValue is TTOMLTable) then    // Check if value is a table
    begin
      // Cast the generic TTOMLValue to specific TTOMLTable type
      ProjectTable := TTOMLTable(ProjectValue);
      
      // Try to get the 'name' value from the project table
      if ProjectTable.TryGetValue('name', ProjectName) then
        WriteLn('Project Name: ', ProjectName.AsString)
      else
        WriteLn('Project name not found');
    end
    else
      WriteLn('Project configuration not found');
  finally
    // Free the root table (this also frees all nested TOML values)
    Config.Free;
  end;
end.

program TOML.Tests;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF}
  DUnitX.TestFramework,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  TOML in '..\..\src\TOML.pas',
  TOML.Types in '..\..\src\TOML.Types.pas',
  TOML.Parser in '..\..\src\TOML.Parser.pas',
  TOML.Serializer in '..\..\src\TOML.Serializer.pas',
  TOML.Tests.NestedKeys in 'TOML.Tests.NestedKeys.pas',
  TOML.Tests.Comments in 'TOML.Tests.Comments.pas',
  TOML.Tests.MultilineStrings in 'TOML.Tests.MultilineStrings.pas',
  TOML.Tests.Locale in 'TOML.Tests.Locale.pas';

var
  Runner: ITestRunner;
  Results: IRunResults;
  Logger: ITestLogger;
  NUnitLogger: ITestLogger;
begin
  try
    TDUnitX.CheckCommandLine;
    Runner := TDUnitX.CreateRunner;

    Logger := TDUnitXConsoleLogger.Create(True);
    Runner.AddLogger(Logger);

    NUnitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    Runner.AddLogger(NUnitLogger);
    Runner.FailsOnNoAsserts := False;

    Results := Runner.Execute;
    if not Results.AllPassed then
      System.ExitCode := 1
    else
      System.ExitCode := 0;

    {$IFNDEF CI}
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
    begin
      System.Writeln(E.ClassName, ': ', E.Message);
      System.ExitCode := 1;
    end;
  end;
end.

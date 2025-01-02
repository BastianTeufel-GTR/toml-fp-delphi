program TestRunner;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, consoletestrunner, TestCaseTOML, TOML;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'TOML Test Runner';
  Application.Run;
  Application.Free;
end.


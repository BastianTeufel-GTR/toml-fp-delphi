{*******************************************************************************
  Unit Name: TOML.Tests.Locale
  Purpose: DUnitX test verifying that serialization is independent of the
           host locale's DecimalSeparator.

  Copyright (c) 1984-2026 GTR mbH

  Dependencies:
    - DUnitX.TestFramework
    - TOML, TOML.Types, TOML.Serializer
*******************************************************************************}
unit TOML.Tests.Locale;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  TOML,
  TOML.Types;

type
  /// <summary>
  /// Verifies that <c>TTOMLSerializer</c> emits '.' as the float decimal
  /// separator even when the host locale's <c>DecimalSeparator</c> is ','.
  /// Guards against regressions that would re-introduce a global-locale
  /// dependency into float or datetime serialization.
  /// </summary>
  [TestFixture]
  TTOMLLocaleTests = class
  private
    FSavedDecimalSeparator: Char;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    /// <summary>
    /// With <c>DecimalSeparator := ','</c>, serialized output must contain
    /// "3.14" and "2.718" (dot) and must NOT contain "3,14" or "2,718"
    /// (comma), and must round-trip back through the parser with floats
    /// preserved within 1e-9.
    /// </summary>
    [Test]
    procedure Serialize_LocaleWithCommaDecimalSeparator_EmitsDotNotComma;
  end;

implementation

uses
  System.Math,
  TOML.Serializer;

procedure TTOMLLocaleTests.Setup;
begin
  FSavedDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := ',';
end;

procedure TTOMLLocaleTests.TearDown;
begin
  FormatSettings.DecimalSeparator := FSavedDecimalSeparator;
end;

procedure TTOMLLocaleTests.Serialize_LocaleWithCommaDecimalSeparator_EmitsDotNotComma;
var
  LTable: TTOMLTable;
  LSerialized: string;
  LReparsed: TTOMLTable;
  LValue: TTOMLValue;
begin
  LTable := TOMLTable;
  try
    LTable.Add('pi', TOMLFloat(3.14));
    LTable.Add('e', TOMLFloat(2.718));

    LSerialized := SerializeTOML(LTable);

    Assert.Contains(LSerialized, '3.14',
      'Serialized output must contain "3.14" (dot separator)');
    Assert.Contains(LSerialized, '2.718',
      'Serialized output must contain "2.718" (dot separator)');
    Assert.IsFalse(LSerialized.Contains('3,14'),
      'Serialized output must NOT contain "3,14" (locale comma leaked)');
    Assert.IsFalse(LSerialized.Contains('2,718'),
      'Serialized output must NOT contain "2,718" (locale comma leaked)');

    LReparsed := ParseTOML(LSerialized);
    try
      Assert.IsTrue(LReparsed.TryGetValue('pi', LValue), 'pi key present after round-trip');
      Assert.IsTrue(SameValue(LValue.AsFloat, 3.14, 1e-9),
        'pi round-trips within 1e-9');
      Assert.IsTrue(LReparsed.TryGetValue('e', LValue), 'e key present after round-trip');
      Assert.IsTrue(SameValue(LValue.AsFloat, 2.718, 1e-9),
        'e round-trips within 1e-9');
    finally
      LReparsed.Free;
    end;
  finally
    LTable.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTOMLLocaleTests);

end.

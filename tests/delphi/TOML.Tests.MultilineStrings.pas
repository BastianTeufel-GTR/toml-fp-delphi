{*******************************************************************************
  Unit Name: TOML.Tests.MultilineStrings
  Purpose: DUnitX tests for multiline string round-trip, line-ending backslash,
           and literal string fallback

  Copyright (c) 1984-2026 GTR mbH

  Dependencies:
    - DUnitX.TestFramework
    - TOML, TOML.Types, TOML.Parser, TOML.Serializer
*******************************************************************************}
unit TOML.Tests.MultilineStrings;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  TOML,
  TOML.Types;

type
  /// <summary>
  /// Tests for TOML multiline string parsing, round-trip preservation, line-ending
  /// backslash trimming, and literal string fallback serialization.
  /// </summary>
  [TestFixture]
  TTOMLMultilineStringTests = class
  published
    [Test]
    procedure Parse_MultilineBasic_PreservesStyle;
    [Test]
    procedure Parse_MultilineLiteral_PreservesStyle;
    [Test]
    procedure Parse_BasicString_StyleIsBasic;
    [Test]
    procedure Parse_LiteralString_StyleIsLiteral;
    [Test]
    procedure RoundTrip_MultilineBasic_PreservesTripleQuotes;
    [Test]
    procedure RoundTrip_MultilineLiteral_PreservesTripleSingleQuotes;
    [Test]
    procedure Parse_LineEndingBackslash_TrimsWhitespace;
    [Test]
    procedure Parse_LineEndingBackslash_MultipleContinuations;
    [Test]
    procedure Serialize_LiteralWithSingleQuote_FallsBackToBasic;
    [Test]
    procedure Serialize_LiteralWithoutSpecialChars_UsesLiteralStyle;
    [Test]
    procedure RoundTrip_BasicString_StaysBasic;
  end;

implementation

uses
  TOML.Parser, TOML.Serializer;

{ TTOMLMultilineStringTests }

procedure TTOMLMultilineStringTests.Parse_MultilineBasic_PreservesStyle;
var
  doc: TTOMLTable;
  value: TTOMLValue;
begin
  // TOML: key = """<LF>hello<LF>world"""
  doc := ParseTOML('key = """' + #10 + 'hello' + #10 + 'world"""' + #10);
  try
    Assert.IsTrue(doc.TryGetValue('key', value), 'key should exist');
    Assert.AreEqual(
      Ord(tssMultilineBasic),
      Ord(TTOMLString(value).Style),
      'Style should be tssMultilineBasic'
    );
    Assert.AreEqual('hello' + #10 + 'world', value.AsString);
  finally
    doc.Free;
  end;
end;

procedure TTOMLMultilineStringTests.Parse_MultilineLiteral_PreservesStyle;
const
  // Three single-quote characters: ''' used as multiline literal delimiter
  TripleSQ = #39#39#39;
var
  doc: TTOMLTable;
  value: TTOMLValue;
  tomlInput: string;
begin
  // TOML: key = '''<LF>hello<LF>world'''
  tomlInput := 'key = ' + TripleSQ + #10 + 'hello' + #10 + 'world' + TripleSQ + #10;
  doc := ParseTOML(tomlInput);
  try
    Assert.IsTrue(doc.TryGetValue('key', value), 'key should exist');
    Assert.AreEqual(
      Ord(tssMultilineLiteral),
      Ord(TTOMLString(value).Style),
      'Style should be tssMultilineLiteral'
    );
    Assert.AreEqual('hello' + #10 + 'world', value.AsString);
  finally
    doc.Free;
  end;
end;

procedure TTOMLMultilineStringTests.Parse_BasicString_StyleIsBasic;
var
  doc: TTOMLTable;
  value: TTOMLValue;
begin
  doc := ParseTOML('key = "hello"' + #10);
  try
    Assert.IsTrue(doc.TryGetValue('key', value), 'key should exist');
    Assert.AreEqual(
      Ord(tssBasic),
      Ord(TTOMLString(value).Style),
      'Style should be tssBasic'
    );
    Assert.AreEqual('hello', value.AsString);
  finally
    doc.Free;
  end;
end;

procedure TTOMLMultilineStringTests.Parse_LiteralString_StyleIsLiteral;
var
  doc: TTOMLTable;
  value: TTOMLValue;
begin
  // TOML: key = 'hello'
  doc := ParseTOML('key = ' + #39 + 'hello' + #39 + #10);
  try
    Assert.IsTrue(doc.TryGetValue('key', value), 'key should exist');
    Assert.AreEqual(
      Ord(tssLiteral),
      Ord(TTOMLString(value).Style),
      'Style should be tssLiteral'
    );
    Assert.AreEqual('hello', value.AsString);
  finally
    doc.Free;
  end;
end;

procedure TTOMLMultilineStringTests.RoundTrip_MultilineBasic_PreservesTripleQuotes;
var
  doc: TTOMLTable;
  serialized: string;
begin
  doc := ParseTOML('key = """' + #10 + 'hello' + #10 + 'world"""' + #10);
  try
    serialized := SerializeTOML(doc);
    Assert.IsTrue(
      Pos('"""', serialized) > 0,
      'Serialized output should contain triple double-quotes'
    );
  finally
    doc.Free;
  end;
end;

procedure TTOMLMultilineStringTests.RoundTrip_MultilineLiteral_PreservesTripleSingleQuotes;
const
  TripleSQ = #39#39#39;
var
  doc: TTOMLTable;
  serialized: string;
begin
  doc := ParseTOML('key = ' + TripleSQ + #10 + 'hello' + #10 + 'world' + TripleSQ + #10);
  try
    serialized := SerializeTOML(doc);
    Assert.IsTrue(
      Pos(TripleSQ, serialized) > 0,
      'Serialized output should contain triple single-quotes'
    );
  finally
    doc.Free;
  end;
end;

procedure TTOMLMultilineStringTests.Parse_LineEndingBackslash_TrimsWhitespace;
var
  doc: TTOMLTable;
  value: TTOMLValue;
  tomlInput: string;
begin
  // TOML multiline basic with line-ending backslash:
  //   key = """
  //   hello \
  //          world"""
  // The backslash at end of line trims the newline and leading whitespace on the
  // next line, so the result should be 'hello world'.
  tomlInput :=
    'key = """' + #10 +
    'hello \' + #10 +
    '       world"""' + #10;
  doc := ParseTOML(tomlInput);
  try
    Assert.IsTrue(doc.TryGetValue('key', value), 'key should exist');
    Assert.AreEqual('hello world', value.AsString);
  finally
    doc.Free;
  end;
end;

procedure TTOMLMultilineStringTests.Parse_LineEndingBackslash_MultipleContinuations;
var
  doc: TTOMLTable;
  value: TTOMLValue;
  tomlInput: string;
begin
  // Multiple line-ending backslash continuations — all whitespace between
  // continuations is trimmed, yielding a single space-joined result.
  tomlInput :=
    'key = """' + #10 +
    'The quick \' + #10 +
    '           brown \' + #10 +
    '           fox"""' + #10;
  doc := ParseTOML(tomlInput);
  try
    Assert.IsTrue(doc.TryGetValue('key', value), 'key should exist');
    Assert.AreEqual('The quick brown fox', value.AsString);
  finally
    doc.Free;
  end;
end;

procedure TTOMLMultilineStringTests.Serialize_LiteralWithSingleQuote_FallsBackToBasic;
var
  doc: TTOMLTable;
  serialized: string;
begin
  // A literal-style string containing a single quote cannot be serialized as
  // a TOML literal string; the serializer must fall back to basic double-quoted.
  doc := TTOMLTable.Create;
  try
    doc.Add('key', TTOMLString.Create('it''s a test', tssLiteral));
    serialized := SerializeTOML(doc);
    // Should have fallen back to basic double-quoted output
    Assert.IsTrue(
      Pos('"it', serialized) > 0,
      'Serialized output should use double-quoted fallback'
    );
    // Must NOT be wrapped in single quotes
    Assert.AreEqual(
      0,
      Pos(#39 + 'it' + #39 + 's', serialized),
      'Should not emit unescaped single-quote literal form'
    );
  finally
    doc.Free;
  end;
end;

procedure TTOMLMultilineStringTests.Serialize_LiteralWithoutSpecialChars_UsesLiteralStyle;
var
  doc: TTOMLTable;
  serialized: string;
begin
  // A literal-style string with no single quotes or control characters
  // should be serialized as a TOML literal string (single-quoted).
  doc := TTOMLTable.Create;
  try
    doc.Add('path', TTOMLString.Create('C:\Users\example', tssLiteral));
    serialized := SerializeTOML(doc);
    Assert.IsTrue(
      Pos(#39 + 'C:\Users\example' + #39, serialized) > 0,
      'Should emit single-quoted literal string for backslash path'
    );
  finally
    doc.Free;
  end;
end;

procedure TTOMLMultilineStringTests.RoundTrip_BasicString_StaysBasic;
var
  doc: TTOMLTable;
  serialized: string;
begin
  doc := ParseTOML('key = "simple value"' + #10);
  try
    serialized := SerializeTOML(doc);
    // A basic string must NOT be re-serialized as multiline
    Assert.AreEqual(
      0,
      Pos('"""', serialized),
      'Basic string should not be serialized as multiline'
    );
    // But it must still contain the value in double quotes
    Assert.IsTrue(
      Pos('"simple value"', serialized) > 0,
      'Basic string should still be double-quoted in output'
    );
  finally
    doc.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTOMLMultilineStringTests);

end.

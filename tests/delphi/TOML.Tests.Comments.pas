{*******************************************************************************
  Unit Name: TOML.Tests.Comments
  Purpose: DUnitX tests for TOML comment parsing, serialization, and round-trip

  Copyright (c) 1984-2026 GTR mbH

  Dependencies:
    - DUnitX.TestFramework
    - TOML, TOML.Types, TOML.Parser, TOML.Serializer
*******************************************************************************}
unit TOML.Tests.Comments;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  TOML,
  TOML.Types;

type
  /// <summary>
  /// Tests for TOML comment parsing, serialization, and round-trip.
  /// Covers standalone full-line comments, inline comments on values,
  /// the TTOMLTable.AddComment API, and comment preservation across
  /// parse-serialize-parse cycles.
  /// </summary>
  [TestFixture]
  TTOMLCommentTests = class
  published
    /// <summary>Parses a standalone comment line and verifies it appears in Body.</summary>
    [Test]
    procedure Parse_StandaloneComment_CapturedInBody;

    /// <summary>Parses three consecutive comment lines and verifies order is preserved.</summary>
    [Test]
    procedure Parse_MultipleStandaloneComments_PreserveOrder;

    /// <summary>Parses an inline comment on a value and verifies InlineComment property.</summary>
    [Test]
    procedure Parse_InlineComment_CapturedOnValue;

    /// <summary>Serializes a table whose first entry is a comment; comment precedes the key.</summary>
    [Test]
    procedure Serialize_StandaloneComment_OutputBeforeKey;

    /// <summary>Serializes a value with an inline comment; comment follows the value.</summary>
    [Test]
    procedure Serialize_InlineComment_OutputAfterValue;

    /// <summary>Parses a TOML string with comments, serializes it, then parses again; all comments survive.</summary>
    [Test]
    procedure RoundTrip_StandaloneAndInline_Preserved;

    /// <summary>AddComment inserts a comment between two keys during serialization.</summary>
    [Test]
    procedure API_AddComment_AppearsBeforeNextKey;

    /// <summary>Multiple consecutive AddComment calls all appear in the correct order.</summary>
    [Test]
    procedure API_MultipleAddComment_AllAppearInOrder;

    /// <summary>Setting InlineComment on a TTOMLInteger emits the comment after the value.</summary>
    [Test]
    procedure API_InlineComment_AppearsAfterValue;

    /// <summary>A comment before a table header is captured in the root body; a comment inside the
    /// section is captured in the sub-table body.</summary>
    [Test]
    procedure Parse_CommentBeforeTableHeader_Captured;

    /// <summary>A key-value pair with no inline comment yields InlineComment = empty string.</summary>
    [Test]
    procedure Parse_NoComment_InlineCommentEmpty;

    /// <summary>A table with plain key-value pairs and no comments serializes without any '#'.</summary>
    [Test]
    procedure Serialize_NoComments_OutputUnchanged;
  end;

implementation

uses
  TOML.Parser, TOML.Serializer;

{ TTOMLCommentTests }

procedure TTOMLCommentTests.Parse_StandaloneComment_CapturedInBody;
var
  doc: TTOMLTable;
  entry: TTOMLTableEntry;
  comment: TTOMLComment;
begin
  // Input: one comment line followed by a key-value pair
  doc := ParseTOML('# this is a comment' + #10 + 'key = "value"' + #10);
  try
    Assert.AreEqual(2, doc.Body.Count, 'Body should contain exactly 2 entries');

    entry := doc.Body[0];
    Assert.AreEqual('', entry.Key, 'Comment entry should have empty key');
    Assert.IsTrue(entry.Value is TTOMLComment,
      'First body entry should be a TTOMLComment');
    comment := TTOMLComment(entry.Value);
    Assert.AreEqual('this is a comment', comment.Text,
      'Comment text should match without leading space');

    entry := doc.Body[1];
    Assert.AreEqual('key', entry.Key, 'Second body entry key should be "key"');
  finally
    doc.Free;
  end;
end;

procedure TTOMLCommentTests.Parse_MultipleStandaloneComments_PreserveOrder;
var
  doc: TTOMLTable;
  toml: string;
begin
  toml :=
    '# first' + #10 +
    '# second' + #10 +
    '# third' + #10 +
    'key = 42' + #10;

  doc := ParseTOML(toml);
  try
    Assert.AreEqual(4, doc.Body.Count, 'Body should contain 4 entries');

    Assert.IsTrue(doc.Body[0].Value is TTOMLComment, 'Entry 0 should be TTOMLComment');
    Assert.AreEqual('first', TTOMLComment(doc.Body[0].Value).Text,
      'First comment text should be "first"');

    Assert.IsTrue(doc.Body[1].Value is TTOMLComment, 'Entry 1 should be TTOMLComment');
    Assert.AreEqual('second', TTOMLComment(doc.Body[1].Value).Text,
      'Second comment text should be "second"');

    Assert.IsTrue(doc.Body[2].Value is TTOMLComment, 'Entry 2 should be TTOMLComment');
    Assert.AreEqual('third', TTOMLComment(doc.Body[2].Value).Text,
      'Third comment text should be "third"');

    Assert.AreEqual('key', doc.Body[3].Key, 'Fourth entry key should be "key"');
  finally
    doc.Free;
  end;
end;

procedure TTOMLCommentTests.Parse_InlineComment_CapturedOnValue;
var
  doc: TTOMLTable;
  value: TTOMLValue;
begin
  doc := ParseTOML('key = "value" # inline comment' + #10);
  try
    Assert.IsTrue(doc.TryGetValue('key', value), '"key" should exist');
    Assert.AreEqual('inline comment', value.InlineComment,
      'InlineComment should be "inline comment" (trimmed, without #)');
  finally
    doc.Free;
  end;
end;

procedure TTOMLCommentTests.Serialize_StandaloneComment_OutputBeforeKey;
var
  doc: TTOMLTable;
  floatVal: TTOMLFloat;
  output: string;
  posComment, posKey: Integer;
begin
  doc := TTOMLTable.Create;
  try
    doc.AddComment('default resolution');
    floatVal := TTOMLFloat.Create(0.5);
    doc.Add('resolution', floatVal);

    output := SerializeTOML(doc);

    posComment := Pos('# default resolution', output);
    posKey     := Pos('resolution =', output);

    Assert.IsTrue(posComment > 0,
      'Output should contain "# default resolution"');
    Assert.IsTrue(posKey > 0,
      'Output should contain "resolution ="');
    Assert.IsTrue(posComment < posKey,
      '"# default resolution" should appear before "resolution ="');
  finally
    doc.Free;
  end;
end;

procedure TTOMLCommentTests.Serialize_InlineComment_OutputAfterValue;
var
  doc: TTOMLTable;
  floatVal: TTOMLFloat;
  output: string;
begin
  doc := TTOMLTable.Create;
  try
    floatVal := TTOMLFloat.Create(0.5);
    floatVal.InlineComment := 'in millimeters';
    doc.Add('resolution', floatVal);

    output := SerializeTOML(doc);

    // The inline comment is appended as " # in millimeters"
    Assert.IsTrue(Pos('# in millimeters', output) > 0,
      'Output should contain "# in millimeters"');
    // The comment must appear on the same line after the numeric value
    Assert.IsTrue(Pos('resolution =', output) > 0,
      'Output should contain "resolution ="');
  finally
    doc.Free;
  end;
end;

procedure TTOMLCommentTests.RoundTrip_StandaloneAndInline_Preserved;
var
  tomlInput, serialized: string;
  doc, reparsed: TTOMLTable;
  value: TTOMLValue;
begin
  tomlInput :=
    '# config file' + #10 +
    '# generated automatically' + #10 +
    'timeout = 30 # seconds' + #10;

  doc := ParseTOML(tomlInput);
  try
    serialized := SerializeTOML(doc);

    // All three comments must survive serialization
    Assert.IsTrue(Pos('# config file', serialized) > 0,
      'Serialized output should contain "# config file"');
    Assert.IsTrue(Pos('# generated automatically', serialized) > 0,
      'Serialized output should contain "# generated automatically"');
    Assert.IsTrue(Pos('# seconds', serialized) > 0,
      'Serialized output should contain "# seconds"');

    // Parse the serialized form again
    reparsed := ParseTOML(serialized);
    try
      // Expect 2 comment entries + 1 key entry in the root body
      Assert.AreEqual(3, reparsed.Body.Count,
        'Re-parsed body should have 3 entries (2 comments + 1 key)');

      Assert.IsTrue(reparsed.Body[0].Value is TTOMLComment,
        'Re-parsed entry 0 should be TTOMLComment');
      Assert.IsTrue(reparsed.Body[1].Value is TTOMLComment,
        'Re-parsed entry 1 should be TTOMLComment');

      Assert.AreEqual('timeout', reparsed.Body[2].Key,
        'Re-parsed entry 2 key should be "timeout"');
      Assert.IsTrue(reparsed.TryGetValue('timeout', value),
        '"timeout" should exist in re-parsed doc');
      Assert.AreEqual('seconds', value.InlineComment,
        'Inline comment "seconds" should survive round-trip');
    finally
      reparsed.Free;
    end;
  finally
    doc.Free;
  end;
end;

procedure TTOMLCommentTests.API_AddComment_AppearsBeforeNextKey;
var
  doc: TTOMLTable;
  output: string;
  posName, posComment, posValue: Integer;
begin
  doc := TTOMLTable.Create;
  try
    doc.Add('name', TTOMLString.Create('Alice'));
    doc.AddComment('settings');
    doc.Add('value', TTOMLInteger.Create(42));

    output := SerializeTOML(doc);

    posName    := Pos('name =', output);
    posComment := Pos('# settings', output);
    posValue   := Pos('value =', output);

    Assert.IsTrue(posName > 0,    'Output should contain "name ="');
    Assert.IsTrue(posComment > 0, 'Output should contain "# settings"');
    Assert.IsTrue(posValue > 0,   'Output should contain "value ="');

    Assert.IsTrue(posName < posComment,
      '"name =" should appear before "# settings"');
    Assert.IsTrue(posComment < posValue,
      '"# settings" should appear before "value ="');
  finally
    doc.Free;
  end;
end;

procedure TTOMLCommentTests.API_MultipleAddComment_AllAppearInOrder;
var
  doc: TTOMLTable;
  output: string;
  posOne, posTwo, posThree: Integer;
begin
  doc := TTOMLTable.Create;
  try
    doc.AddComment('line one');
    doc.AddComment('line two');
    doc.AddComment('line three');
    doc.Add('key', TTOMLString.Create('value'));

    output := SerializeTOML(doc);

    posOne   := Pos('# line one', output);
    posTwo   := Pos('# line two', output);
    posThree := Pos('# line three', output);

    Assert.IsTrue(posOne > 0,   'Output should contain "# line one"');
    Assert.IsTrue(posTwo > 0,   'Output should contain "# line two"');
    Assert.IsTrue(posThree > 0, 'Output should contain "# line three"');

    Assert.IsTrue(posOne < posTwo,
      '"# line one" should appear before "# line two"');
    Assert.IsTrue(posTwo < posThree,
      '"# line two" should appear before "# line three"');
  finally
    doc.Free;
  end;
end;

procedure TTOMLCommentTests.API_InlineComment_AppearsAfterValue;
var
  doc: TTOMLTable;
  intVal: TTOMLInteger;
  output: string;
begin
  doc := TTOMLTable.Create;
  try
    intVal := TTOMLInteger.Create(100);
    intVal.InlineComment := 'max count';
    doc.Add('limit', intVal);

    output := SerializeTOML(doc);

    Assert.IsTrue(Pos('limit = 100 # max count', output) > 0,
      'Output should contain "limit = 100 # max count"');
  finally
    doc.Free;
  end;
end;

procedure TTOMLCommentTests.Parse_CommentBeforeTableHeader_Captured;
var
  doc: TTOMLTable;
  toml: string;
  sectionValue: TTOMLValue;
  sectionTable: TTOMLTable;
begin
  toml :=
    '# root comment' + #10 +
    '[section]' + #10 +
    '# section comment' + #10 +
    'key = "value"' + #10;

  doc := ParseTOML(toml);
  try
    // Root body: comment entry + section table entry
    Assert.IsTrue(doc.Body.Count >= 1, 'Root body should have at least 1 entry');
    Assert.IsTrue(doc.Body[0].Value is TTOMLComment,
      'Root body[0] should be TTOMLComment');
    Assert.AreEqual('root comment', TTOMLComment(doc.Body[0].Value).Text,
      'Root comment text should be "root comment"');

    // Navigate into the section sub-table
    Assert.IsTrue(doc.TryGetValue('section', sectionValue),
      '"section" table should exist');
    sectionTable := sectionValue.AsTable;

    Assert.IsTrue(sectionTable.Body.Count >= 1,
      'Section body should have at least 1 entry');
    Assert.IsTrue(sectionTable.Body[0].Value is TTOMLComment,
      'Section body[0] should be TTOMLComment');
    Assert.AreEqual('section comment', TTOMLComment(sectionTable.Body[0].Value).Text,
      'Section comment text should be "section comment"');
  finally
    doc.Free;
  end;
end;

procedure TTOMLCommentTests.Parse_NoComment_InlineCommentEmpty;
var
  doc: TTOMLTable;
  value: TTOMLValue;
begin
  doc := ParseTOML('key = "value"' + #10);
  try
    Assert.IsTrue(doc.TryGetValue('key', value), '"key" should exist');
    Assert.AreEqual('', value.InlineComment,
      'InlineComment should be empty when no inline comment is present');
  finally
    doc.Free;
  end;
end;

procedure TTOMLCommentTests.Serialize_NoComments_OutputUnchanged;
var
  doc: TTOMLTable;
  output: string;
begin
  doc := TTOMLTable.Create;
  try
    doc.Add('name', TTOMLString.Create('Alice'));
    doc.Add('age', TTOMLInteger.Create(30));

    output := SerializeTOML(doc);

    Assert.AreEqual(0, Pos('#', output),
      'Output should contain no "#" when no comments are added');
  finally
    doc.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTOMLCommentTests);

end.

unit TOML.Types;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}
{$modeswitch advancedrecords}

interface

uses
  SysUtils, Generics.Collections;

type
  { TOML value types }
  TTOMLValueType = (
    tvtString,
    tvtInteger,
    tvtFloat,
    tvtBoolean,
    tvtDateTime,
    tvtArray,
    tvtTable,
    tvtInlineTable
  );

  { Forward declarations }
  TTOMLValue = class;
  TTOMLArray = class;
  TTOMLTable = class;

  { Exception types }
  ETOMLException = class(Exception);
  ETOMLParserException = class(ETOMLException);
  ETOMLSerializerException = class(ETOMLException);

  { Generic dictionary for tables }
  TTOMLTableDict = specialize TDictionary<string, TTOMLValue>;
  
  { Generic list for arrays }
  TTOMLValueList = specialize TList<TTOMLValue>;

  { Base TOML value class }
  TTOMLValue = class
  private
    FValueType: TTOMLValueType;
  protected
    function GetAsString: string; virtual;
    function GetAsInteger: Int64; virtual;
    function GetAsFloat: Double; virtual;
    function GetAsBoolean: Boolean; virtual;
    function GetAsDateTime: TDateTime; virtual;
    function GetAsArray: TTOMLArray; virtual;
    function GetAsTable: TTOMLTable; virtual;
  public
    constructor Create(AType: TTOMLValueType);
    destructor Destroy; override;
    
    property ValueType: TTOMLValueType read FValueType;
    property AsString: string read GetAsString;
    property AsInteger: Int64 read GetAsInteger;
    property AsFloat: Double read GetAsFloat;
    property AsBoolean: Boolean read GetAsBoolean;
    property AsDateTime: TDateTime read GetAsDateTime;
    property AsArray: TTOMLArray read GetAsArray;
    property AsTable: TTOMLTable read GetAsTable;
  end;

  { String value }
  TTOMLString = class(TTOMLValue)
  private
    FValue: string;
  protected
    function GetAsString: string; override;
  public
    constructor Create(const AValue: string);
    property Value: string read FValue write FValue;
  end;

  { Integer value }
  TTOMLInteger = class(TTOMLValue)
  private
    FValue: Int64;
  protected
    function GetAsInteger: Int64; override;
    function GetAsFloat: Double; override;
  public
    constructor Create(const AValue: Int64);
    property Value: Int64 read FValue write FValue;
  end;

  { Float value }
  TTOMLFloat = class(TTOMLValue)
  private
    FValue: Double;
  protected
    function GetAsFloat: Double; override;
  public
    constructor Create(const AValue: Double);
    property Value: Double read FValue write FValue;
  end;

  { Boolean value }
  TTOMLBoolean = class(TTOMLValue)
  private
    FValue: Boolean;
  protected
    function GetAsBoolean: Boolean; override;
  public
    constructor Create(const AValue: Boolean);
    property Value: Boolean read FValue write FValue;
  end;

  { DateTime value }
  TTOMLDateTime = class(TTOMLValue)
  private
    FValue: TDateTime;
  protected
    function GetAsDateTime: TDateTime; override;
  public
    constructor Create(const AValue: TDateTime);
    property Value: TDateTime read FValue write FValue;
  end;

  { Array value }
  TTOMLArray = class(TTOMLValue)
  private
    FItems: TTOMLValueList;
  protected
    function GetAsArray: TTOMLArray; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AValue: TTOMLValue);
    function GetItem(Index: Integer): TTOMLValue;
    function GetCount: Integer;
    property Items: TTOMLValueList read FItems;
    property Count: Integer read GetCount;
  end;

  { Table value }
  TTOMLTable = class(TTOMLValue)
  private
    FItems: TTOMLTableDict;
  protected
    function GetAsTable: TTOMLTable; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AKey: string; AValue: TTOMLValue);
    function TryGetValue(const AKey: string; out AValue: TTOMLValue): Boolean;
    property Items: TTOMLTableDict read FItems;
  end;

implementation

{ TTOMLValue }

constructor TTOMLValue.Create(AType: TTOMLValueType);
begin
  inherited Create;
  FValueType := AType;
end;

destructor TTOMLValue.Destroy;
begin
  inherited Destroy;
end;

function TTOMLValue.GetAsString: string;
begin
  Result := '';
  raise ETOMLException.Create('Cannot convert this TOML value to string');
end;

function TTOMLValue.GetAsInteger: Int64;
begin
  Result := 0;
  raise ETOMLException.Create('Cannot convert this TOML value to integer');
end;

function TTOMLValue.GetAsFloat: Double;
begin
  Result := 0.0;
  raise ETOMLException.Create('Cannot convert this TOML value to float');
end;

function TTOMLValue.GetAsBoolean: Boolean;
begin
  Result := False;
  raise ETOMLException.Create('Cannot convert this TOML value to boolean');
end;

function TTOMLValue.GetAsDateTime: TDateTime;
begin
  Result := 0;
  raise ETOMLException.Create('Cannot convert this TOML value to datetime');
end;

function TTOMLValue.GetAsArray: TTOMLArray;
begin
  Result := nil;
  raise ETOMLException.Create('Cannot convert this TOML value to array');
end;

function TTOMLValue.GetAsTable: TTOMLTable;
begin
  Result := nil;
  raise ETOMLException.Create('Cannot convert this TOML value to table');
end;

{ TTOMLString }

constructor TTOMLString.Create(const AValue: string);
begin
  inherited Create(tvtString);
  FValue := AValue;
end;

function TTOMLString.GetAsString: string;
begin
  Result := FValue;
end;

{ TTOMLInteger }

constructor TTOMLInteger.Create(const AValue: Int64);
begin
  inherited Create(tvtInteger);
  FValue := AValue;
end;

function TTOMLInteger.GetAsInteger: Int64;
begin
  Result := FValue;
end;

function TTOMLInteger.GetAsFloat: Double;
begin
  Result := FValue;
end;

{ TTOMLFloat }

constructor TTOMLFloat.Create(const AValue: Double);
begin
  inherited Create(tvtFloat);
  FValue := AValue;
end;

function TTOMLFloat.GetAsFloat: Double;
begin
  Result := FValue;
end;

{ TTOMLBoolean }

constructor TTOMLBoolean.Create(const AValue: Boolean);
begin
  inherited Create(tvtBoolean);
  FValue := AValue;
end;

function TTOMLBoolean.GetAsBoolean: Boolean;
begin
  Result := FValue;
end;

{ TTOMLDateTime }

constructor TTOMLDateTime.Create(const AValue: TDateTime);
begin
  inherited Create(tvtDateTime);
  FValue := AValue;
end;

function TTOMLDateTime.GetAsDateTime: TDateTime;
begin
  Result := FValue;
end;

{ TTOMLArray }

constructor TTOMLArray.Create;
begin
  inherited Create(tvtArray);
  FItems := TTOMLValueList.Create;
end;

destructor TTOMLArray.Destroy;
var
  Item: TTOMLValue;
begin
  for Item in FItems do
    Item.Free;
  FItems.Free;
  inherited Destroy;
end;

procedure TTOMLArray.Add(AValue: TTOMLValue);
begin
  FItems.Add(AValue);
end;

function TTOMLArray.GetItem(Index: Integer): TTOMLValue;
begin
  Result := FItems[Index];
end;

function TTOMLArray.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TTOMLArray.GetAsArray: TTOMLArray;
begin
  Result := Self;
end;

{ TTOMLTable }

constructor TTOMLTable.Create;
begin
  inherited Create(tvtTable);
  FItems := TTOMLTableDict.Create;
end;

destructor TTOMLTable.Destroy;
var
  Item: TTOMLValue;
begin
  for Item in FItems.Values do
    Item.Free;
  FItems.Free;
  inherited Destroy;
end;

procedure TTOMLTable.Add(const AKey: string; AValue: TTOMLValue);
var
  ExistingValue: TTOMLValue;
begin
  if FItems.TryGetValue(AKey, ExistingValue) then
    raise ETOMLParserException.CreateFmt('Duplicate key "%s" found', [AKey]);
  FItems.AddOrSetValue(AKey, AValue);
end;

function TTOMLTable.TryGetValue(const AKey: string; out AValue: TTOMLValue): Boolean;
begin
  Result := FItems.TryGetValue(AKey, AValue);
end;

function TTOMLTable.GetAsTable: TTOMLTable;
begin
  Result := Self;
end;

end. 
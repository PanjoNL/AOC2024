unit uAOCUtils;

interface

uses
  System.SysUtils, System.Generics.Collections, AOCBase, RTTI, System.Classes, Math,
  System.Net.HttpClient, System.Net.urlclient, system.Generics.Defaults, uAocConfig, vcl.Dialogs, system.uiTypes;

type
  TAdventOfCodeRef = class of TAdventOfCode;
  TAOCDirection = (North = 0, East, South, West);
  TAOCDirections = set of TAOCDirection;

type AOCUtils = class
  public
    class function GetAdventOfCode: TList<TAdventOfCodeRef>;
    class function DayIndexFromClassName(Const aClassName: String): String;
    class procedure DoAdventOfCode(aAdventOfCodeRef: TAdventOfCodeRef; aConfig: TAOCConfig);
    class procedure DownLoadPuzzleInput(var InputList: TStrings; Const DayIndex: String; Config: TAOCConfig; RefreshSessionCookie: Boolean = False);
    class function GetAocIniFilePath(Const aIniName: string): string;
end;

type TAOCDictionary<TKey,TValue> = class(TDictionary<TKey,TValue>)

  public
    procedure AddOrIgnoreValue(const Key: TKey; const Value: TValue);
    function AddOrSetValueEx(aKey: TKey; aValue: TValue): Boolean;
    constructor Create(const aOnValueNoify: TCollectionNotifyEvent<TValue>); overload;
    procedure Free; overload;
end;

type
  TPosition = record
    x: int64;
    y: int64;
    class function Create(const aX, aY: int64): TPosition; static;
    function AddDelta(const aX, aY: int64): TPosition;
    function Equals(Const Other: TPosition): Boolean;
    function Clone: TPosition;
    function ApplyDirection(Const aDirection: TAOCDirection; aDelta: Int64 = 1): TPosition;
    function CacheKey: Int64;
  private
    function SetIt(const aX, aY: int64): TPosition;
  end;

  TPosition3 = record
    x, y, z: int64;
    class function Create(Const aX, aY, aZ: int64): TPosition3; static;
    class operator Add(a, b: TPosition3): TPosition3;
    class operator Subtract(a, b: TPosition3): TPosition3;
    class operator GreaterThan(a, b: TPosition3): Boolean;
    class operator LessThan(a, b: TPosition3): Boolean;

    class function Min(a, b: TPosition3): TPosition3; static;
    class function Max(a, b: TPosition3): TPosition3; static;
  end;

  TAocGrid<TValue> = class
  type
    TCharToValueConverter = function(const aChar: Char): TValue of object;
    TValueToCharConverter = function(const aValue: TValue): Char of object;

  private
    FData: TDictionary<int64,TValue>;
    FMaxX, FMaxY: integer;
    FValueConverter: TValueToCharConverter;
  public
    constructor create(aStrings: TStrings; aCharToValueConverter: TCharToValueConverter; aValueToCharConverter: TValueToCharConverter); reintroduce;
    destructor Destroy; override;

    procedure PrintToDebug;
    procedure SetData(aX, aY: integer; aValue: TValue);
    function TryGetValue(aX, aY: integer; out aValue: TValue): boolean; overload;
    function TryGetValue(aPosition: TPosition; out aValue: TValue): Boolean; overload;
    function GetValue(aX, aY: integer): TValue;

    property MaxX: integer read FMaxX;
    property MaxY: integer read FMaxY;
  end;

  TAocGridHelper = class
  private
    class function CharToChar(const aChar: Char): Char;
    class function CharToInt(const aChar: Char): Integer;
    class function IntToChar(const aInt: integer): char;
  public
    class function CreateCharGrid(aStrings: TStrings): TAocGrid<Char>;
    class function CreateIntegerGrid(aStrings: TStrings): TAocGrid<Integer>;
  end;


function GCD(Number1, Number2: int64): int64;
function LCM(Number1, Number2: int64): int64;
function OccurrencesOfChar(const S: string; const C: string): integer;
function BitStringToInt(Const aBit: string): int64;
function CountTrueBits(aInt: integer): integer;
function InRange(const aTarget, aLeft, aRight: int64): boolean;
function RotateDirection(aDirection: TAOCDirection; aAmmount: integer): TAOCDirection;
function IsNumber(aNumber: string): Boolean;
function DeleteRepeatedSpaces(const s: string):string;

Const
  MaxInt64: Int64 = 9223372036854775807;
  Base10Table: array[0..18] of int64 =
  (
    1,
    10,
    100,
    1000,
    10000,
    100000,
    1000000,
    10000000,
    100000000,
    1000000000,
    10000000000,
    100000000000,
    1000000000000,
    10000000000000,
    100000000000000,
    1000000000000000,
    10000000000000000,
    100000000000000000,
    1000000000000000000);

implementation

uses
  System.strUtils;

class function AOCUtils.GetAdventOfCode: TList<TAdventOfCodeRef>;
var
  ctx: TRttiContext;
  lType: TRttiType;
  AdventOfCode: TAdventOfCodeRef;
  Comparison: TComparison<TAdventOfCodeRef>;
begin
  result := TList<TAdventOfCodeRef>.Create;
  ctx := TRttiContext.Create;
  Writeln('Discovering advent of code');
  for lType in ctx.GetTypes do
    if (lType is TRttiInstanceType) and (TRttiInstanceType(lType).MetaclassType.InheritsFrom(TAdventOfCode))
    then
    begin
      AdventOfCode := TAdventOfCodeRef(TRttiInstanceType(lType).MetaclassType);
      if AdventOfCode.ClassName <> TAdventOfCode.ClassName then
      begin
        Writeln('Found '+ AdventOfCode.ClassName);
        Result.Add(adventOfCode);
      end;
    end;

  Comparison :=
    function(const Left, Right: TAdventOfCodeRef): Integer
    begin
      Result := StrToInt(AOCUtils.DayIndexFromClassName(Left.ClassName)) -
                StrToInt(AOCUtils.DayIndexFromClassName(Right.ClassName));
    end;
  Result.Sort(TComparer<TAdventOfCodeRef>.Construct(Comparison));
end;

class function AOCUtils.GetAocIniFilePath(const aIniName: string): string;
begin
  Result := ParamStr(0);
  while (Not FileExists(Result+PathDelim+aIniName)) do
  begin
    Result := ExtractFileDir(Result);
    if Result = '' then
      Break;
  end;
end;

class function AOCUtils.DayIndexFromClassName(Const aClassName: String): String;
var i: Integer;
begin
  i := Length('TAdventOfCodeDay');
  Result := Copy(aClassName, i + 1, Length(aClassName) - i); //
end;

class procedure AOCUtils.DoAdventOfCode(aAdventOfCodeRef: TAdventOfCodeRef; aConfig: TAOCConfig);
var AdventOfCode: TAdventOfCode;
begin
  AdventOfCode := aAdventOfCodeRef.Create(aConfig);
  try
    AdventOfCode.Solve;
  finally
    AdventOfCode.Free;
  end;
end;

class procedure AOCUtils.DownLoadPuzzleInput(var InputList: TStrings; Const DayIndex: String; Config: TAOCConfig; RefreshSessionCookie: Boolean = False);
var HttpClient: THttpClient;
    lHeader: TNetHeader;
    Headers: TNetHeaders;
    HttpOutput: IHTTPResponse;
    Url, SessionCookie: string;
begin
  Url := Config.BaseUrl+'/day/'+DayIndex+'/input';
  WriteLn('Downloading puzzle data from ' + Url);

  HttpClient := THTTPClient.Create;

  Headers := nil;
  SessionCookie := Config.SessionCookie;
  if (SessionCookie = '') or RefreshSessionCookie then
  begin
    SessionCookie := InputBox('SessionCookie', 'Whats your sessionCookie?', '');
    if SessionCookie = '' then
      raise Exception.Create('No session cookie provided')
    else
      Config.SessionCookie := SessionCookie;
  end;

  lHeader := LHeader.Create('cookie', 'session=' + SessionCookie);
  SetLength(Headers, 1);
  Headers[0] := lHeader;
  HttpClient.UserAgent := Config.GithubRepo;
  
  try
    HttpOutput := HttpClient.Get(Url, nil, Headers);
    WriteLn(HttpOutput.StatusCode);
    if HttpOutput.StatusCode = 200 then
      InputList.LoadFromStream(HttpOutput.ContentStream)
    else if HttpOutput.StatusCode = 400 then
    begin
      if MessageDlg('Error conecting to AOC, delete session and try again?', mtError, mbYesNo, 0) = mrYes then
      begin
        Config.SessionCookie := '';
        AOCUtils.DownLoadPuzzleInput(InputList, DayIndex, Config, True);
      end
    end
    else
      raise Exception.Create(HttpOutput.ContentAsString());
  finally
    HttpClient.Free;
  end;
end;

procedure TAOCDictionary<TKey,TValue>.AddOrIgnoreValue(const Key: TKey; const Value: TValue);
begin
  if not Self.ContainsKey(Key) then
    Self.Add(Key, Value);
end;

function TAOCDictionary<TKey, TValue>.AddOrSetValueEx(aKey: TKey; aValue: TValue): Boolean;
var
  PrevCount: Integer;
begin
  PrevCount := Count;
  AddOrSetValue(aKey, aValue);
  Result := PrevCount <> Count;
end;

constructor TAOCDictionary<TKey,TValue>.Create(const aOnValueNoify: TCollectionNotifyEvent<TValue>);
begin
  inherited Create;
  OnValueNotify := aOnValueNoify;
end;

procedure TAOCDictionary<TKey,TValue>.Free;
begin
  Self.Clear;
  inherited Free;
end;

function TPosition.SetIt(const aX: int64; const aY: int64): TPosition;
begin
  x := aX;
  y := aY;
  Result := Self;
end;

function TPosition.AddDelta(const aX, aY: int64): TPosition;
begin
  x := x + aX;
  y := y + aY;
  Result := Self;
end;

function TPosition.Equals(Const Other: TPosition): Boolean;
begin
  Result := (x = Other.x) and (y = Other.y);
end;

function TPosition.CacheKey: Int64;
begin
  if (x > MaxInt) or (y > MaxInt) then
    raise Exception.Create('Index out of range');
  Result := x shl 32 + y;
end;

function TPosition.Clone: TPosition;
begin
  Result.x := Self.x;
  Result.y := Self.y;
end;

class function TPosition.Create(const aX, aY: int64): TPosition;
begin
  Result.SetIt(aX, aY);
end;

function TPosition.ApplyDirection(Const aDirection: TAOCDirection; aDelta: Int64 = 1): TPosition;
begin
  case aDirection of
    North: AddDelta(0, -aDelta);
    East: AddDelta(aDelta, 0);
    South: AddDelta(0, aDelta);
    West: AddDelta(-aDelta, 0);
  end;
  Result := Self
end;

class function TPosition3.Create(Const aX, aY, aZ: int64): TPosition3;
begin
  Result.x := aX;
  Result.y := aY;
  Result.z := aZ;
end;

class operator TPosition3.GreaterThan(a, b: TPosition3): Boolean;
begin
  Result := (a.x > b.x) or (a.y > b.y) or (a.z > b.z);
end;

class operator TPosition3.LessThan(a, b: TPosition3): Boolean;
begin
  Result := (a.x < b.x) or (a.y < b.y) or (a.z < b.z);
end;

class operator TPosition3.Add(a, b: TPosition3): TPosition3;
begin
  Result.x := a.x + b.x;
  Result.y := a.y + b.y;
  Result.z := a.z + b.z;
end;

class operator TPosition3.Subtract(a, b: TPosition3): TPosition3;
begin
  Result.x := a.x - b.x;
  Result.y := a.y - b.y;
  Result.z := a.z - b.z;
end;

class function TPosition3.Max(a, b: TPosition3): TPosition3;
begin
  Result.x := Math.Max(a.x, b.x);
  Result.y := Math.Max(a.y, b.y);
  Result.z := Math.Max(a.z, b.z);
end;

class function TPosition3.Min(a, b: TPosition3): TPosition3;
begin
  Result.x := Math.Min(a.x, b.x);
  Result.y := Math.Min(a.y, b.y);
  Result.z := Math.Min(a.z, b.z);
end;

constructor TAocGrid<TValue>.create(aStrings: TStrings; aCharToValueConverter: TCharToValueConverter; aValueToCharConverter: TValueToCharConverter);
var
  tmpX, tmpY: Integer;
begin
  FData := TDictionary<int64,TValue>.Create;
  FMaxX := Length(aStrings[0]) -1;
  FMaxY := aStrings.Count -1;
  FValueConverter := aValueToCharConverter;

  for tmpY := 0 to MaxY do
    for tmpX := 0 to MaxX do
      FData.Add(TPosition.Create(tmpX, tmpY).CacheKey, aCharToValueConverter(aStrings[tmpY][tmpX+1]));
end;

destructor TAocGrid<TValue>.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TAocGrid<TValue>.PrintToDebug;
var
  x, y: integer;
  s: string;
begin
  Writeln('');
  for y := 0 to MaxY do
  begin
    s := '';
    for x := 0 to MaxX do
      s := s + FValueConverter(FData[TPosition.Create(x, y).CacheKey]);
    Writeln(s);
  end;
end;

procedure TAocGrid<TValue>.SetData(aX, aY: integer; aValue: TValue);
begin
  FData.AddOrSetValue(TPosition.Create(aX, aY).CacheKey, aValue);
end;

function TAocGrid<TValue>.TryGetValue(aPosition: TPosition; out aValue: TValue): Boolean;
begin
  Result := False;
  aValue := Default(TValue);
  if InRange(aPosition.X, 0, MaxX) and InRange(aPosition.Y, 0, MaxY) then
    Result := FData.TryGetValue(aPosition.CacheKey, aValue);
end;

function TAocGrid<TValue>.TryGetValue(aX, aY: integer; out aValue: TValue): boolean;
begin
  Result := TryGetValue(TPosition.Create(aX, aY), aValue);
end;

function TAocGrid<TValue>.GetValue(aX, aY: integer): TValue;
begin
  Result := FData[TPosition.Create(aX, aY).CacheKey];
end;

function GCD(Number1, Number2: int64): int64;
var Temp: int64;
begin
  if Number1 < 0 then Number1 := -Number1;
  if Number2 < 0 then Number2 := -Number2;

  repeat
    if Number1 < Number2 then
      begin
        Temp := Number1;
        Number1 := Number2;
        Number2 := Temp;
      end;

    Number1 := Number1 mod Number2;
  until (Number1 = 0);

  result := Number2;
end;

function LCM(Number1, Number2: int64): int64;
begin
  Result := Trunc(Number1 * Number2 / GCD(Number1, Number2));
end;

function OccurrencesOfChar(const S: string; const C: string): integer;
var
  i: Integer;
begin
  result := 0;
  for i := 1 to Length(S) do
    if S[i] = C then
      inc(result);
end;

function BitStringToInt(Const aBit: string): int64;
var i: Integer;
begin
  Result := 0;
  for i := 1 to Length(aBit) do
  begin
    Result := Result shl 1;
    case IndexStr(aBit[i], ['0', '1']) of
      0:; //Do nothing
      1: Inc(Result);
    else
      raise Exception.CreateFmt('BitStringToInt encounterd: %s', [aBit[i]]);
    end;
  end;
end;

function CountTrueBits(aInt: integer): integer;
begin
  Result := 0;
  while aInt > 0 do
  begin
    if Odd(aInt) then
      inc(Result);
    aInt := aInt shr 1;
  end;
end;

function InRange(const aTarget, aLeft, aRight: int64): boolean;
begin
  Result := (aTarget >= aLeft) and (aTarget <= aRight);
end;

function RotateDirection(aDirection: TAOCDirection; aAmmount: integer): TAOCDirection;
begin
  Result := TAOCDirection((aAmmount + Ord(aDirection)) mod 4);
end;

function IsNumber(aNumber: string): Boolean;
var
  Dummy: int64;
begin
  Result := TryStrToInt64(aNumber, Dummy);
end;

function DeleteRepeatedSpaces(const s: string):string;
var
  i:integer;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    if not ((s[i]=' ') and (s[i-1]=' ')) then
    begin
      Result := Result + s[i];
    end;
  end;
end;

{ TAocGridHelper }

class function TAocGridHelper.CreateCharGrid(aStrings: TStrings): TAocGrid<Char>;
begin
  Result := TAocGrid<Char>.create(aStrings, CharToChar, CharToChar);
end;

class function TAocGridHelper.CreateIntegerGrid(aStrings: TStrings): TAocGrid<Integer>;
begin
  Result := TAocGrid<integer>.create(aStrings, CharToInt, IntToChar);
end;

class function TAocGridHelper.CharToChar(const aChar: Char): Char;
begin
  Result := aChar;
end;

class function TAocGridHelper.CharToInt(const aChar: Char): Integer;
begin
  Result := StrToInt(aChar);
end;

class function TAocGridHelper.IntToChar(const aInt: integer): char;
begin
  Assert(aInt < 10);
  Result := aInt.ToString[1];
end;


end.

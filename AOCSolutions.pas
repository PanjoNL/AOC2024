unit AOCSolutions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, RTTI,
  Generics.Defaults, System.Generics.Collections,
  System.Diagnostics, AOCBase, RegularExpressions, System.DateUtils,
  System.StrUtils,
  System.Math, uAOCUtils, System.Types, PriorityQueues, System.Json,
  AocLetterReader, uAOCTimer,
  System.Threading, system.Hash;

type
  IntegerArray = Array Of Integer;

  TAdventOfCodeDay1 = class(TAdventOfCode)
  private
    IdList1, IdList2: TList<integer>;
    Counts: TDictionary<integer, integer>;
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay2 = class(TAdventOfCode)
  private
    function CheckReactor(TolerateBadLevel: Boolean): integer;
    function IsReportSave(aRepport: IntegerArray; idxToIgnore: integer = -1): Boolean;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay3 = class(TAdventOfCode)
  private
    SolutionA, SolutionB: int64;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay4 = class(TAdventOfCode)
  private
    SolutionA, SolutionB: int64;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay5 = class(TAdventOfCode)
  private
    SolutionA, SolutionB: int64;
  protected
    procedure BeforeSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay = class(TAdventOfCode)
  private
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

implementation

{$REGION 'TAdventOfCodeDay1'}
procedure TAdventOfCodeDay1.BeforeSolve;
var
  s: string;
  split: TStringDynArray;
  id2, cnt: integer;
begin
  inherited;

  IdList1 := TList<integer>.Create;
  IdList2 := TList<integer>.Create;
  Counts := TDictionary<integer, integer>.Create;

  for s in FInput do
  begin
    split := SplitString(s, ' ');

    IdList1.Add(split[0].ToInteger);
    id2 := split[length(split)-1].ToInteger;
    IdList2.Add(id2);
    Counts.TryGetValue(id2, cnt);
    Counts.AddOrSetValue(id2, cnt + 1);
  end;
end;

procedure TAdventOfCodeDay1.AfterSolve;
begin
  inherited;

  IdList1.Free;
  IdList2.Free;
  Counts.Free;
end;

function TAdventOfCodeDay1.SolveA: Variant;
var
  i: integer;
begin
  IdList1.Sort;
  IdList2.Sort;

  Result := 0;
  for i := 0 to IdList1.Count -1 do
    result := Result + Abs(IdList1[i] - IdList2[i]);
end;

function TAdventOfCodeDay1.SolveB: Variant;
var
  i, cnt: integer;
begin
  Result := 0;
  for i in idList1 do
  begin
    Counts.TryGetValue(i, cnt);
    result := Result + i * cnt;
  end;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay2'}
function TAdventOfCodeDay2.CheckReactor(TolerateBadLevel: Boolean): integer;
var
  split: TStringDynArray;
  s: string;
  i: integer;
  Report: IntegerArray;
begin
  Result := 0;
  for s in FInput do
  begin
    split := SplitString(s, ' ');

    SetLength(Report, Length(split));
    for i := 0 to Length(split)-1 do
      Report[i] := Split[i].ToInteger; 

    if (not TolerateBadLevel) and IsReportSave(Report) then
      Result := Result + 1
    else if TolerateBadLevel then
    begin
      for i := 0 to length(Report)-1 do
      begin
        if IsReportSave(Report, i) then
        begin
          Result := Result + 1;
          break;
        end;
      end;
    end;
  end;
end;

function TAdventOfCodeDay2.IsReportSave(aRepport: IntegerArray; idxToIgnore: integer = -1): Boolean;

  function _idx(i: integer): integer;
  begin
    Result := i;
    if (idxToIgnore >= 0) and (IdxToIgnore <= i) then
      Inc(Result);
  end;

var
  i, diff, CheckSign, CurrentSign: integer;
begin
  CheckSign := Sign(aRepport[_idx(1)] - aRepport[_idx(0)]);
  for i := 1 to Length(aRepport) -1 - ifthen(idxToIgnore >= 0, 1, 0) do
  begin
    CurrentSign := Sign(aRepport[_idx(i)] - aRepport[_idx(i-1)]);
    diff := abs(aRepport[_idx(i)] - aRepport[_idx(i-1)]);
    if (CheckSign <> CurrentSign) or (diff < 1) or (diff > 3) then
      Exit(False)
  end;

  result := True;
end;

function TAdventOfCodeDay2.SolveA: Variant;
begin
  Result := CheckReactor(False);
end;

function TAdventOfCodeDay2.SolveB: Variant;
begin
  Result := CheckReactor(True);
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay3'}
procedure TAdventOfCodeDay3.BeforeSolve;
var
  mulResult: Int64;
  s: string;
  split: TStringDynArray;
  Regex: TRegEx;
  Matches: TMatchCollection;
  Match: TMatch;
  Enabled: Boolean;
begin
  SolutionA := 0;
  SolutionB := 0;

  Regex := TRegex.Create('mul\(\d*,\d*\)|do\(\)|don''t\(\)');

  Enabled := True;
  for s in FInput do
  begin
    Matches := RegEx.Matches(s);
    for match in Matches do
    begin
      if Match.Value = 'do()' then
        Enabled := True
      else if Match.Value = 'don''t()' then
        Enabled := False
      else
      begin
        Split := SplitString(Copy(Match.Value, 5, Length(Match.Value)-5), ',');
        mulResult := Split[0].ToInt64 * Split[1].ToInt64;
        Inc(SolutionA, mulResult);

        if Enabled then
          Inc(SolutionB, mulResult);
      end;
    end;
  end;
end;

function TAdventOfCodeDay3.SolveA: Variant;
begin
  Result := SolutionA;
end;

function TAdventOfCodeDay3.SolveB: Variant;
begin
  Result := SolutionB;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay4'}
type
  TAocGrid = class
    FData: TDictionary<int64,char>;
    FMaxX, FMaxY: integer;

  public
    constructor create(aStrings: TStrings); reintroduce;
    destructor Destroy; override;

    procedure PrintToDebug;
    function TryGetValue(aX, aY: integer; out aValue: char): boolean;

    property MaxX: integer read FMaxX;
    property MaxY: integer read FMaxY;


  end;

{ TAocGrid }

constructor TAocGrid.create(aStrings: TStrings);
var
  tmpX, tmpY: Integer;
begin
  FData := TDictionary<int64,Char>.Create;
  FMaxX := Length(aStrings[0]) -1;
  FMaxY := aStrings.Count -1;

  for tmpY := 0 to MaxY do
    for tmpX := 0 to MaxX do
      FData.Add(TPosition.Create(tmpX, tmpY).CacheKey, aStrings[tmpY][tmpX+1]);
end;

destructor TAocGrid.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TAocGrid.PrintToDebug;
var
  x, y: integer;
  s: string;
begin
  Writeln('');
  for y := 0 to MaxY do
  begin
    s := '';
    for x := 0 to MaxX do
      s := s + FData[TPosition.Create(x, y).CacheKey];
    Writeln(s);
  end;
end;

function TAocGrid.TryGetValue(aX, aY: integer; out aValue: char): boolean;
begin
  Result := False;
  if InRange(aX, 0, MaxX) and InRange(aY, 0, MaxY) then
    Result := FData.TryGetValue(TPosition.Create(aX, aY).CacheKey, aValue);
end;

procedure TAdventOfCodeDay4.BeforeSolve;
const
  DeltaX: array[0..7] of integer = (1,1,1,0,0,-1,-1,-1);
  DeltaY: array[0..7] of Integer = (-1,0,1,-1,1,-1,0,1);
  DeltaX_2: array[0..3] of integer = (1,1,-1,-1);
  DeltaY_2: array[0..3] of Integer = (1,-1,-1,1);
var
  x,y,i: Integer;
  Grid: TAocGrid;
  chr: Char;
  Pos: TPosition;
begin
  SolutionA := 0;
  SolutionB := 0;
  Grid := TAocGrid.create(FInput);

  for x := 0 to Grid.MaxX do
    for y := 0 to Grid.MaxY do
    begin
      Pos := TPosition.Create(x, y)

      if Grid.TryGetValue(x, y, chr) and (chr = 'X') then
        for i := 0 to 7 do
          if Grid.TryGetValue(x + DeltaX[i] * 3, Y + DeltaY[i] * 3, chr) and (chr = 'S') and
             Grid.TryGetValue(x + DeltaX[i] * 2, Y + DeltaY[i] * 2, chr) and (chr = 'A') and
             Grid.TryGetValue(x + DeltaX[i] * 1, Y + DeltaY[i] * 1, chr) and (chr = 'M') then
            inc(SolutionA);

      if Grid.TryGetValue(x, y, chr) and (chr = 'A') then
        for i := 0 to 3 do
          if Grid.TryGetValue(x + DeltaX_2[(i+0) mod 4], Y + DeltaY_2[(i+0) mod 4], chr) and (chr = 'M') and
             Grid.TryGetValue(x + DeltaX_2[(i+1) mod 4], Y + DeltaY_2[(i+1) mod 4], chr) and (chr = 'M') and
             Grid.TryGetValue(x + DeltaX_2[(i+2) mod 4], Y + DeltaY_2[(i+2) mod 4], chr) and (chr = 'S') and
             Grid.TryGetValue(x + DeltaX_2[(i+3) mod 4], Y + DeltaY_2[(i+3) mod 4], chr) and (chr = 'S') then
            inc(SolutionB);
    end;

  Grid.Free
end;

function TAdventOfCodeDay4.SolveA: Variant;
begin
  Result := SolutionA;
end;

function TAdventOfCodeDay4.SolveB: Variant;
begin
  Result := SolutionB;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay5'}
type
  TPageComparer = class (TInterfacedObject, IComparer<integer>)
  private
    FRules: TDictionary<integer, boolean{greatherThen}>;

    function GenerateKey(x, y: integer): integer;
    function Compare(const Left, Right: integer): Integer;

  public
    constructor create; reintroduce;
    destructor Destroy; override;

    procedure AddGreaterThenRule(x, y: integer);
    procedure AddSmallerThenRule(x, y: integer);
  end;

{ TPageComparer }

constructor TPageComparer.create;
begin
  FRules := TDictionary<integer, boolean{greatherThen}>.Create;
end;

destructor TPageComparer.Destroy;
begin
  FRules.Free;

  inherited;
end;

function TPageComparer.Compare(const Left, Right: integer): Integer;
begin
  Result := 1;

  if Left = Right then
    Exit(0);

  if not FRules[GenerateKey(Left, Right)] then
    Result := -1
end;

procedure TPageComparer.AddGreaterThenRule(x, y: integer);
begin
  FRules.Add(GenerateKey(x, y), true);
end;

procedure TPageComparer.AddSmallerThenRule(x, y: integer);
begin
  FRules.Add(GenerateKey(x, y), false);
end;

function TPageComparer.GenerateKey(x, y: integer): integer;
begin
  Result := (x shl 16) + y;
end;

procedure TAdventOfCodeDay5.BeforeSolve;
var
  i,j: integer;
  Split: TStringDynArray;
  Page: TList<integer>;
  x,y: integer;
  ReadingRules, Valid: Boolean;
  Comparer: TPageComparer;
begin
  ReadingRules := True;
  SolutionA := 0;
  SolutionB := 0;

  Comparer := TPageComparer.create;
  Page := TList<integer>.Create(Comparer);

  for i := 0 to FInput.Count -1 do
  begin
    if FInput[i] = '' then
    begin
      ReadingRules := False;
      continue;
    end;

    if ReadingRules then
    begin
      Split := SplitString(FInput[i], '|');
      x := Split[0].ToInteger;
      y := Split[1].ToInteger;

      Comparer.AddSmallerThenRule(x, y);
      Comparer.AddGreaterThenRule(y, x);
      Continue;
    end;

    Split := SplitString(FInput[i], ',');
    Valid := True;
    Page.Clear;

    for j := 0 to Length(split) - 1 do
      Page.Add(split[j].ToInteger);

    Page.Sort;
    for j := 0 to Length(split) - 1 do
    begin
      if Page[j] <> split[j].ToInteger then
      begin
        Valid := False;
        Break;
      end;
    end;

    if Valid then
      Inc(SolutionA, Page[Page.Count shr 1])
    else
      Inc(SolutionB, Page[Page.Count shr 1]);
  end;

  Page.Free;
end;

function TAdventOfCodeDay5.SolveA: Variant;
begin
  Result := SolutionA;
end;

function TAdventOfCodeDay5.SolveB: Variant;
begin
  Result := SolutionB;
end;
{$ENDREGION}

{$REGION 'Placeholder'}
function TAdventOfCodeDay.SolveA: Variant;
begin
  Result := null;
end;

function TAdventOfCodeDay.SolveB: Variant;
begin
  Result := null;
end;
{$ENDREGION}

initialization

RegisterClasses([
  TAdventOfCodeDay1, TAdventOfCodeDay2, TAdventOfCodeDay3, TAdventOfCodeDay4, TAdventOfCodeDay5
  ]);

end.

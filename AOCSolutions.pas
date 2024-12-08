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
  System.Threading, System.SyncObjs, system.Hash;

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

  TAdventOfCodeDay6 = class(TAdventOfCode)
  private
    FGrid: TAocGrid;
    FGaurdStartPosition: TPosition;
    BaseSeenCells: TDictionary<TPosition,TAOCDirections>;
    procedure FindPath(aSeenCells: TDictionary<TPosition,TAOCDirections>; BlockX, BlockY: integer);
  protected
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay7 = class(TAdventOfCode)
  private
    function Solve(aUseConcatenation: boolean): int64;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay8 = class(TAdventOfCode)
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
procedure TAdventOfCodeDay4.BeforeSolve;
const
  DeltaX: array[0..7] of integer = (1,1,1,0,0,-1,-1,-1);
  DeltaY: array[0..7] of Integer = (-1,0,1,-1,1,-1,0,1);
  DeltaX_2: array[0..3] of integer = (1,1,-1,-1);
  DeltaY_2: array[0..3] of Integer = (1,-1,-1,1);
var
  x,y,i,MaxY,MaxX: Integer;

  function _checkPos(const aX, aY: integer; Const aChar: string): Boolean;
  begin
    Result := InRange(aX, 1, MaxX) and InRange(aY, 0, MaxY) and SameText(FInput[aY][aX], aChar);
  end;

begin
  MaxY := FInput.Count-1;
  MaxX := Length(FInput[0]);

  SolutionA := 0;
  SolutionB := 0;

  for x := 1 to MaxX do
    for y := 0 to MaxY do
    begin

      if _checkPos(x, y, 'X') then
        for i := 0 to 7 do
          if _checkPos(x + DeltaX[i] * 1, Y + DeltaY[i] * 1, 'M') and
             _checkPos(x + DeltaX[i] * 2, Y + DeltaY[i] * 2, 'A') and
             _checkPos(x + DeltaX[i] * 3, Y + DeltaY[i] * 3, 'S') then
            inc(SolutionA);

      if _checkPos(x, y, 'A') then
        for i := 0 to 3 do
          if _checkPos(x + DeltaX_2[(i+0) mod 4], Y + DeltaY_2[(i+0) mod 4], 'M') and
             _checkPos(x + DeltaX_2[(i+1) mod 4], Y + DeltaY_2[(i+1) mod 4], 'M') and
             _checkPos(x + DeltaX_2[(i+2) mod 4], Y + DeltaY_2[(i+2) mod 4], 'S') and
             _checkPos(x + DeltaX_2[(i+3) mod 4], Y + DeltaY_2[(i+3) mod 4], 'S') then
            inc(SolutionB);
    end;
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
{$REGION 'TAdventOfCodeDay6'}
procedure TAdventOfCodeDay6.BeforeSolve;
var
  x,y: integer;
  chr: char;
begin
  inherited;

  FGrid := TAocGrid.Create(FInput);
  BaseSeenCells := TDictionary<TPosition,TAOCDirections>.Create;

  for x := 0 to FGrid.MaxX do
    for y := 0 to FGrid.MaxY do
    begin
      if FGrid.TryGetValue(x, y, chr) and (chr = '^') then
      begin
        FGaurdStartPosition := TPosition.Create(x, y);
        break;
      end;
    end;

  FindPath(BaseSeenCells, -1, -1)
end;

procedure TAdventOfCodeDay6.AfterSolve;
begin
  inherited;

  FGrid.Free;
  BaseSeenCells.Free;
end;

procedure TAdventOfCodeDay6.FindPath(aSeenCells: TDictionary<TPosition, TAOCDirections>; BlockX, BlockY: integer);
var
  GaurdPosition, Next: TPosition;
  GaurdFacing: TAOCDirection;
  GaurdFacings: TAOCDirections;
  chr: char;
begin
  GaurdFacing := TAOCDirection.North;
  GaurdPosition := FGaurdStartPosition.Clone;

  aSeenCells.Add(GaurdPosition, []);
  while True do
  begin
    next := GaurdPosition.Clone.ApplyDirection(GaurdFacing);
    if not FGrid.TryGetValue(next.x, next.y, Chr) then
      Break; // Out of bounds

    if (chr = '#') or ((Next.X = BlockX) and (Next.Y = BlockY)) then
      GaurdFacing := TAOCDirection((ord(GaurdFacing) + 1) mod 4)
    else
    begin
      GaurdPosition := next;

      if aSeenCells.TryGetValue(next, GaurdFacings) then
      begin
        if GaurdFacing in GaurdFacings then // Cycle detected
        begin
          aSeenCells.Clear;
          exit;
        end;
      end;

      aSeenCells.AddOrSetValue(next, GaurdFacings + [GaurdFacing]);
    end;
  end;
end;

function TAdventOfCodeDay6.SolveA: Variant;
begin
  Result := BaseSeenCells.Count;
end;

function TAdventOfCodeDay6.SolveB: Variant;
var
  PossibleBlocks: Integer;

  function CreateTask(aBlock: TPosition): ITask;
  begin
    Result := TTask.Create(procedure()
      var
        Seen: TDictionary<TPosition,TAOCDirections>;
      begin
        Seen := TDictionary<TPosition,TAOCDirections>.Create;
        FindPath(Seen, aBlock.X, aBlock.Y);
        if Seen.Count = 0 then
          TInterlocked.Increment(PossibleBlocks);
        Seen.Free;
      end);
  end;

var
  Block: TPosition;
  Tasks: TList<ITask>;
  Task: ITask;
begin
  PossibleBlocks := 0;
  Tasks := TList<ITask>.Create;

  for Block in BaseSeenCells.Keys do
  begin
    if Block.CacheKey = FGaurdStartPosition.CacheKey then
      Continue;

    Task := CreateTask(Block);
    Task.Start;
    Tasks.Add(Task);
  end;

  TTask.WaitForAll (tasks.ToArray);
  Result := PossibleBlocks;
  Tasks.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay7'}
function TAdventOfCodeDay7.Solve(aUseConcatenation: boolean): int64;

  function Calc(values: TStringDynArray; const aTarget, aIndex: int64): boolean;
  var
    strValue: string;
    value: int64;
  begin
    if aIndex = 2 then
      exit(aTarget = Values[2].ToInt64);

    strValue := values[aIndex];
    value := strValue.ToInt64;

    Result :=
      Calc(Values, aTarget - value, aIndex - 1) or
      ((aTarget mod value = 0) and Calc(Values, aTarget div value, aIndex - 1)) or
      (aUseConcatenation and aTarget.ToString.EndsWith(strValue) and Calc(Values, aTarget div round(power(10, Length(strValue))), aIndex - 1));
  end;

var
  target: int64;
  s: string;
  split: TStringDynArray;
begin
  Result := 0;

  for s in FInput do
  begin
    split := SplitString(s, ': ');
    target := Split[0].ToInt64;
    if Calc(split, target, Length(Split)-1) then
      Inc(result, target);
  end;
end;

function TAdventOfCodeDay7.SolveA: Variant;
begin
  Result := Solve(False);
end;

function TAdventOfCodeDay7.SolveB: Variant;
begin
  Result := Solve(True);
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay8'}
procedure TAdventOfCodeDay8.BeforeSolve;
var
  Grid: TAocGrid;

  procedure CheckAntinode_A(Seen: TDictionary<int64, boolean>; LocI, LocJ, Start, Delta: TPosition);
  var
    Pos: TPosition;
  begin
    Pos := TPosition.Create(Start.x + Delta.X, Start.Y + Delta.Y);
    if (Pos.CacheKey <> LocI.CacheKey) and (Pos.CacheKey <> LocJ.CacheKey) and InRange(Pos.X, 0, Grid.MaxX) and InRange(pos.y, 0, Grid.MaxY) then
      Seen.AddOrSetValue(Pos.CacheKey, true);
  end;

  procedure CheckAntinode_B(Seen: TDictionary<int64, boolean>; Start, Delta: TPosition);
  var
    Pos: TPosition;
  begin
    Pos := TPosition.Create(Start.x, Start.Y);

    while InRange(Pos.X, 0, Grid.MaxX) and InRange(pos.y, 0, Grid.MaxY) do
    begin
      Seen.AddOrSetValue(Pos.CacheKey, True);
      Pos.AddDelta(Delta.X, Delta.Y);
    end;
  end;

var
  Antennas: TDictionary<char, TList<TPosition>>;
  lst: TList<TPosition>;
  chr: char;
  x,y,i,j: integer;
  posI, posJ: TPosition;
  SeenA, SeenB: TDictionary<int64, boolean>;
begin
  Grid := TAocGrid.Create(FInput);
  Antennas := TDictionary<char, TList<TPosition>>.Create;
  SeenA := TDictionary<int64, boolean>.Create;
  SeenB := TDictionary<int64, boolean>.Create;

  for x := 0 to Grid.MaxX do
    for y := 0 to Grid.MaxY do
    begin
      chr := Grid.GetValue(x, y);
      if chr = '.' then
        continue;

      if not Antennas.TryGetValue(chr, lst) then
      begin
        lst := TList<TPosition>.Create;
        Antennas.Add(chr, lst);
      end;

      lst.Add(TPosition.Create(x,y));
    end;

  for lst in Antennas.Values do
  begin
    for i := 0 to lst.Count - 2 do
      for j := i + 1 to lst.Count -1 do
      begin
        posI := lst[i];
        posJ := lst[j];

        x := posI.x - posJ.x;
        y := posI.y - posJ.y;

        CheckAntinode_A(SeenA, PosI, PosJ, PosI, TPosition.Create(x, y));
        CheckAntinode_A(SeenA, PosI, PosJ, PosI, TPosition.Create(-x, -y));
        CheckAntinode_A(SeenA, PosI, PosJ, PosJ, TPosition.Create(x, y));
        CheckAntinode_A(SeenA, PosI, PosJ, PosJ, TPosition.Create(-x, -y));

        CheckAntinode_B(SeenB, PosI, TPosition.Create(x, y));
        CheckAntinode_B(SeenB, PosI, TPosition.Create(-x, -y));
      end;
  end;

  SolutionA := SeenA.Count;
  SolutionB := SeenB.Count;

  SeenA.Free;
  SeenB.Free;
  Grid.Free;
  Antennas.Free;
end;

function TAdventOfCodeDay8.SolveA: Variant;
begin
  Result := SolutionA;
end;

function TAdventOfCodeDay8.SolveB: Variant;
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
  TAdventOfCodeDay1, TAdventOfCodeDay2, TAdventOfCodeDay3, TAdventOfCodeDay4, TAdventOfCodeDay5,
  TAdventOfCodeDay6, TAdventOfCodeDay7, TAdventOfCodeDay8
  ]);

end.

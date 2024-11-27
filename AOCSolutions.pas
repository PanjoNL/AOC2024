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
  TAdventOfCodeDay1 = class(TAdventOfCode)
  protected
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
function TAdventOfCodeDay1.SolveA: Variant;
var
  s: string;
  split: TStringDynArray;
begin
  for s in FInput do
    split := SplitString(s, '-');

  Result := 7734;


end;

function TAdventOfCodeDay1.SolveB: Variant;
begin
  Result := Null
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
  TAdventOfCodeDay1
  ]);

end.

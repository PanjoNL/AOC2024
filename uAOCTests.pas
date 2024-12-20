unit uAOCTests;

interface

uses
  System.SysUtils, Winapi.Windows, system.Classes,
  uAocUtils, AocSolutions, AOCBase, uAOCConfig, uAocTimer;

type
  AOCTest = record
    AOCClass: TAdventOfCodeRef;
    ExpectedSolutionA, ExpectedSolutionB: String;
    LoadOverridenTestData: TLoadOverridenTestData
end;

type AOCTests = class
public
  Class procedure RunTests(aConfig: TAOCConfig);
end;

Const AOCTestData: array[0..19] of AOCTest =
(
 (AOCClass: TAdventOfCodeDay1; ExpectedSolutionA: '3714264'; ExpectedSolutionB: '18805872'),
 (AOCClass: TAdventOfCodeDay2; ExpectedSolutionA: '670'; ExpectedSolutionB: '700'),
 (AOCClass: TAdventOfCodeDay3; ExpectedSolutionA: '173529487'; ExpectedSolutionB: '99532691'),
 (AOCClass: TAdventOfCodeDay4; ExpectedSolutionA: '2514'; ExpectedSolutionB: '1888'),
 (AOCClass: TAdventOfCodeDay5; ExpectedSolutionA: '5651'; ExpectedSolutionB: '4743'),
 (AOCClass: TAdventOfCodeDay6; ExpectedSolutionA: '5242'; ExpectedSolutionB: '1424'),
 (AOCClass: TAdventOfCodeDay7; ExpectedSolutionA: '5540634308362'; ExpectedSolutionB: '472290821152397'),
 (AOCClass: TAdventOfCodeDay8; ExpectedSolutionA: '359'; ExpectedSolutionB: '1293'),
 (AOCClass: TAdventOfCodeDay9; ExpectedSolutionA: '6398252054886'; ExpectedSolutionB: '6415666220005'),
 (AOCClass: TAdventOfCodeDay10;ExpectedSolutionA: '667'; ExpectedSolutionB: '1344'),
 (AOCClass: TAdventOfCodeDay11;ExpectedSolutionA: '216042'; ExpectedSolutionB: '255758646442399'),
 (AOCClass: TAdventOfCodeDay12;ExpectedSolutionA: '1375476'; ExpectedSolutionB: '821372'),
 (AOCClass: TAdventOfCodeDay13;ExpectedSolutionA: '29187'; ExpectedSolutionB: '99968222587852'),
 (AOCClass: TAdventOfCodeDay14;ExpectedSolutionA: '216027840'; ExpectedSolutionB: '6876'),
 (AOCClass: TAdventOfCodeDay15;ExpectedSolutionA: '1451928'; ExpectedSolutionB: '1462788'),
 (AOCClass: TAdventOfCodeDay16;ExpectedSolutionA: '94444'; ExpectedSolutionB: '502'),
 (AOCClass: TAdventOfCodeDay17;ExpectedSolutionA: '1,5,3,0,2,5,2,5,3'; ExpectedSolutionB: '108107566389757'),
 (AOCClass: TAdventOfCodeDay18;ExpectedSolutionA: '316'; ExpectedSolutionB: '45,18'),
 (AOCClass: TAdventOfCodeDay19;ExpectedSolutionA: '358'; ExpectedSolutionB: '600639829400603'),
 (AOCClass: TAdventOfCodeDay20;ExpectedSolutionA: '1426'; ExpectedSolutionB: '1000697')

 );

implementation
class procedure AOCTests.RunTests(aConfig: TAOCConfig);

  procedure _Check(const DisplayName, Expected, Actual: String);
  begin
    if Expected <> '' then
      if Expected <> Actual then
      begin
        WriteLn(Format('FAIL, %s Expected: %s, Actual: %s', [DisplayName, Expected, Actual]));
        Assert(False);
      end
  end;

Var
  Test: AOCTest;
  AdventOfCode: TAdventOfCode;
  TotalTime, TestTimer: AocTimer;
  SolutionA, SolutionB: string;
  Times: TStringList;
  ElapsedMicroSeconds: Integer;
  s: string;
begin
  Writeln('');

  Times := TStringList.Create;
  try
    TotalTime := AOCTimer.Start;
    for Test in AOCTestData do
    begin
      Writeln(Format('Running tests for %s', [Test.AOCClass.ClassName]));

      AdventOfCode := Test.AOCClass.Create(aConfig);

      TestTimer := AOCTimer.Start;
      AdventOfCode.Test(SolutionA, SolutionB, Test.LoadOverridenTestData);
      ElapsedMicroSeconds := TestTimer.ElapsedTime;
      Times.Add(Format('%s -> Time: %d %s', [Test.AOCClass.Classname, ElapsedMicroSeconds, TimeIndicator[MicroSeconds]]));
      AdventOfCode.Free;

      _Check('Part a', Test.ExpectedSolutionA, SolutionA);
      _Check('Part b', Test.ExpectedSolutionB, SolutionB);
      Writeln(FormAt('Total time %d %s', [ElapsedMicroSeconds, TimeIndicator[MicroSeconds]]));
      Writeln('');
    end;

    Writeln(Format('All tests done in %d %s', [TotalTime.ElapsedTime(MilliSeconds), TimeIndicator[MilliSeconds]]));
    for s in Times do
      WriteLn(s);
  finally
    Times.Free;
  end
end;

end.

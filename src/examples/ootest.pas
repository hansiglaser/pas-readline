Program OOTest;

{$mode objfpc}{$H+}

Uses
  Classes, SysUtils, HistoryOOP, ReadlineOOP;

Type

  { TCompleter }

  TCompleter = class
  private
    FReadline : TReadline;
  public
    Constructor Create(AReadline:TReadline);
    Function GetCompletions(Text:PChar;Start,TheEnd:Integer) : TDynArrString;
    Function GetCommands   (Text:PChar;Start,TheEnd:Integer) : TDynArrString;
  End;

{ TCompleter }

Constructor TCompleter.Create(AReadline:TReadline);
Begin
  inherited Create;
  FReadline := AReadline;
End;

Function TCompleter.GetCompletions(Text:PChar;Start,TheEnd:Integer):TDynArrString;
Begin
  Result := Nil;
  { context dependent completion }
  if Start = 0 then
    Begin
      { first word -> command }
      Result := GetCommands(Text,Start,TheEnd);
    End
  else
    Begin
      { 2nd or later word -> filename }
      FReadline.UseFilenames;   // re-enable filename completions
    End;
End;

Function TCompleter.GetCommands(Text:PChar;Start,TheEnd:Integer):TDynArrString;
Const Commands : Array[0..6] of String = ('list','history','quit','help','linux','open','ootest-1');
Var I : Integer;
    N : Integer;
Begin
  SetLength(Result,Length(Commands));
  N := 0;
  For I := Low(Commands) to High(Commands) do
    if Text = Copy(Commands[I],1,TheEnd-Start) then
      Begin
        Result[N] := Commands[I];
        Inc(N);
      End;
  // set correct length of array
  SetLength(Result,N);
End;

Var H  : THistory;
    R  : TReadline;
    St : String;
    C  : TCompleter;

Begin
  H := THistory.Create('histtest.delme');
  H.Read;    // read history

  R := TReadline.Create('OOTest');
  R.History := H;

  C := TCompleter.Create(R);
  R.GetCompletions := @C.GetCompletions;

  // command line
  repeat
    St := R.Read;
  Until St = 'quit';

  C.Free;
  H.Print;   // print history to screen
  H.Write;   // write history to file
  H.Free;
  R.Free;
End.


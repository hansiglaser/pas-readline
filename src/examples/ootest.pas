(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   Test program demonstrating the OOP wrapper for the GNU Readline       *
 *   Library                                                               *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************)

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


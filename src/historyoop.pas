(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
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

Unit HistoryOOP;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, History;

Type

  { THistory }

  THistory = class
  private
    FFilename      : String;
    FAddDuplicates : Boolean;
  public
    Constructor Create(AFilename:String);
    Destructor  Destroy; override;

    Procedure Read;
    Procedure Write;
    Procedure Print;
    Function  List    : PPHIST_ENTRY; inline;
    Function  Length  : Integer; inline;
    Function  GetLast : PHIST_ENTRY; inline;
    Procedure Add(St:String);
    property AddDuplicates : Boolean read FAddDuplicates write FAddDuplicates;
  End;

Implementation

// signelton static variable
Const HistoryInst : THistory = Nil;

{ THistory }

(**
 * Constructor
 *
 * AFilename: path to history file, if its empty, no history file is used, if
 *   it doesn't contain a '/', it is interpreted relative to the user's home
 *   directory.
 *)
Constructor THistory.Create(AFilename:String);
Begin
  { History is not thread-save }
  if assigned(HistoryInst) then
    raise Exception.Create('You can only have a single THistory instance!');
  HistoryInst := Self;     // set singleton static variable

  inherited Create;

  if AFilename <> '' then
    Begin
      if Pos('/',AFilename) > 0 then
        Begin
          // filename contains a '/' -> absolute path
          FFilename := AFilename;
        End
      else
        Begin
          // filename doesn't contain a '/' -> relative to home directory
          FFilename := GetEnvironmentVariable('HOME') + '/' + AFilename;
        End;
    End;
End;

Destructor THistory.Destroy;
Begin
  { clean up singleton static variable }
  if HistoryInst <> Self then
    raise Exception.Create('Internal Error: HistoryInst <> Self');
  HistoryInst := Nil;

  Inherited Destroy;
End;

Procedure THistory.Read;
Begin
  if FFilename = '' then Exit;
  read_history(PChar(FFilename));
End;

Procedure THistory.Write;
Begin
  if FFilename = '' then Exit;
  write_history(PChar(FFilename));
End;

(**
 * Print the full history
 *)
Procedure THistory.Print;
Var H : PPHIST_ENTRY;
    I : Integer;
    W : Integer;
Begin
  // get the required width for the enumeration
  W := 0;
  I := Length;
  While I > 0 do
    Begin
      Inc(W);
      I := I div 10;
    End;

  // print the whole history list
  H := List;
  if H = Nil then Exit;
  I := 0;
  While H^[I] <> Nil do
    Begin
      WriteLn(' ',(I+1):W,'  ',H^[I]^.Line);
      Inc(I);
    End;
End;

Function THistory.List:PPHIST_ENTRY; inline;
Begin
  Result := history_list;
End;

Function THistory.Length:Integer; inline;
Begin
  Result := history_length;
End;

Function THistory.GetLast:PHIST_ENTRY;Inline;
Begin
  Result := history_get(Length);
End;

Procedure THistory.Add(St:String);
Var H : PHIST_ENTRY;
Begin
  if not FAddDuplicates then
    Begin
      H := GetLast;
      if (H <> Nil) and (St = H^.Line) then
        Exit;
    End;
  add_history(PChar(St));
End;

End.


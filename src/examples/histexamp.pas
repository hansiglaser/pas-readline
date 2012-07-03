(* histexamp.c - history library example program. *)

(* Copyright (C) 1987-2009 Free Software Foundation, Inc.

   This file is part of the GNU Readline Library (Readline), a library for
   reading lines of text with interactive input and history editing.

   Readline is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Readline is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Readline.  If not, see <http://www.gnu.org/licenses/>.
*)
{$MODE ObjFpc}
Program HistExamp;
Uses SysUtils,History,CMem,UnixType;

{$LINKLIB c}
Type Ptm = Pointer;
function strftime(__s:Pchar; __maxsize:size_t; __format:Pchar; __tp:Ptm):size_t;cdecl;external name 'strftime';
function localtime(var __timer : time_t):Ptm;cdecl;external name 'localtime';

Function DoExpansion(Var Line:AnsiString):Boolean;
Var Expansion : PChar;
    Res       : Integer;
Begin
  using_history();

  Res := history_expand(PChar(Line), @Expansion);
  if Res <> 0 then
    WriteLn(StdErr,Expansion);

  if (Res < 0) or (Res = 2) then
    Begin
      Free(Expansion);
      Exit(False);
    End;

  add_history(Expansion);
  Line := Expansion;
  Free(Expansion);
  Result := True;
End;

Procedure DoList;
Var
  List    : PPHIST_ENTRY;
  I       : Integer;
  TT      : Time_t;
  TimeStr : Array[0..127] of Char;
Begin
  List := history_list();
  if List = Nil then Exit;
  I := 0;
  While List^[I] <> Nil do
    Begin
      TT := history_get_time(List^[I]);
      if TT <> 0 then
        strftime (TimeStr, sizeof (TimeStr), '%a %R', localtime(&tt))
      else
        TimeStr := '??';
      WriteLn(I + history_base,': ',Timestr,': ',List^[I]^.Line);
      Inc(I);
    End;
End;

Procedure DoDelete(Line:AnsiString);
Var Which : Integer;
    Entry : PHIST_ENTRY;
Begin
  Line := Copy(Line,7,Length(Line));  // get number argument after 'delete '
  if TryStrToInt(Line,Which) then
    Begin
      Entry := remove_history(Which);
      if Entry = Nil then
	WriteLn(StdErr,'No such entry ',Which)
      else
	Begin
	  Free(Entry^.line);
	  Free(Entry);
	End;
    End
  else
    Begin
      WriteLn(StdErr,'non-numeric arg given to `delete`');
    End;
End;

Var
  Line : AnsiString;
  Len  : Integer;
  Done : Boolean;

Begin
  Line := '';
  Done := false;

  using_history();
  while not Done do
    Begin
      Write('history$ ');
      ReadLn(Line);
      if Line > '' then
	Begin
          Len := Length(Line);
          if Line[Len] = #10 then
            SetLength(Line,Len-1);
	End
      else
        Line := 'quit';

      if Line > '' then
        if not DoExpansion(Line) then
          Continue;

      if Line = 'quit' then
	Done := true
      else if Line = 'save' then
	write_history('history_file')
      else if Line = 'read' then
	read_history('history_file')
      else if Line = 'list' then
        DoList
      else if Pos('delete',Line) = 1 then
        DoDelete(Line);
    End;
End.

(* **************************************************************** *)
(*								    *)
(*			Testing Readline			    *)
(*								    *)
(* **************************************************************** *)

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
Program RLTest;
Uses Readline,History,CMem;

Var
  Temp   : PChar;
  Prompt : PChar;
  Done   : Boolean;
  List   : PPHIST_ENTRY;
  I      : Integer;

Begin
  Temp   := Nil;
  Prompt := 'readline$ ';
  Done   := false;

  while not Done do
    Begin
      Temp := Readline.readline(Prompt);

      (* Test for EOF. *)
      if Temp = Nil then
	Halt(1);

      (* If there is anything on the line, print it and remember it. *)
      if Temp[0] <> #0 then
	Begin
	  WriteLn(StdErr,Temp);
	  add_history(Temp);
	End;

      (* Check for `command' that we handle. *)
      if Temp = 'quit' then
	done := true;

      if Temp = 'list' then
	Begin
	  List := history_list();
	  if List <> Nil then
	    Begin
              I := 0;
              While List^[i] <> Nil do
                Begin
                  WriteLn(StdErr,I,': ',List^[I]^.line);
                  Inc(I);
                End;
	    End;
	End;
      Free(Temp);
  End;
End.


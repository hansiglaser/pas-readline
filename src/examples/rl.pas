(*
 * rl - command-line interface to read a line from the standard input
 *      (or another fd) using readline.
 *
 * usage: rl [-p prompt] [-u unit] [-d default] [-n nchars]
 *)

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
Program RL;
Uses SysUtils,GetOpts,BaseUnix,Readline,History,CMem;

{$macro on}
{$ifdef windows}
  {$define extdecl:=stdcall}
{$else}
  {$define extdecl:=cdecl}
{$endif}

{$LINKLIB c}
Type PFILE = Pointer;
function fdopen(__fd:longint; __modes:Pchar):PFILE;extdecl;external name 'fdopen';

Var ProgName : String;
    DefText  : AnsiString;

Function SetDefText : Integer; extdecl;
Begin
  if DefText > '' then
    Begin
      rl_insert_text(PChar(DefText));
      DefText := '';
      rl_startup_hook := Nil;
    End;
  Result := 0;
End;

Procedure Usage;
Begin
  WriteLn(StdErr,ProgName,': usage: ',ProgName,' [-p prompt] [-u unit] [-d default] [-n nchars]');
End;

Var
  Temp   : PChar;
  Prompt : AnsiString;
  Opt    : Char;
  FD     : Integer;
  NCh    : Integer;
  SB     : Stat;
  ifp    : PFILE;

Begin
  ProgName := ExtractFilename(ParamStr(0));

  (* defaults *)
  Prompt := 'readline$ ';
  FD := 0;
  NCh := 0;
  DefText := '';
  
  repeat
    Opt := GetOpt('p:u:d:n:');
    Case Opt of
      'p': Prompt := OptArg;
      'u': Begin
             if not TryStrToInt(OptArg,FD) then
               Begin
                 WriteLn(StdErr,ProgName,': bad file descriptor `',OptArg,'`');
                 Halt(2);
               End;
           End;
      'd': DefText := OptArg;
      'n': Begin
             if not TryStrToInt(OptArg,NCh) then
               Begin
                 WriteLn(StdErr,ProgName,': bad value for -n: `',OptArg,'`');
                 Halt(2);
               End;
           End;
      EndOfOptions: ;  // ignore
    else
      usage;
      Halt(2);
    End;
  Until Opt = EndOfOptions;

  if FD <> 0 then
    Begin
      if FpFStat(FD,SB) < 0 then
        Begin
          WriteLn(StdErr,ProgName,': ',FD,': bad file descriptor');
          Halt(1);
        End;
      ifp := fdopen(FD, 'r');
      rl_instream := ifp;
    End;

  if DefText > '' then
    rl_startup_hook := @SetDefText;

  if NCh > 0 then
    rl_num_chars_to_read := NCh;

  Temp := Readline.readline(PChar(Prompt));

  (* Test for EOF. *)
  if Temp = Nil then
    Halt(1);

  WriteLn(Temp);
End.

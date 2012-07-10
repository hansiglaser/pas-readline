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

(* http://tiswww.case.edu/php/chet/readline/rltop.html
 * http://tiswww.case.edu/php/chet/readline/readline.html
 * http://tiswww.case.edu/php/chet/readline/history.html
 *)
Unit ReadlineOOP;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Readline, HistoryOOP;

Type
  TDynArrString       = Array of String;
  TGetCompletionsFunc = Function(Text:PChar;Start,TheEnd:Integer) : TDynArrString of object;
  TIsMultiLineCompleteFunc = Function(Line:String) : Boolean of object;

  { TReadline }

  TReadline = class
  private
    FName          : String;     // for conditions in the ~/.inputrc file
    FPrompt        : String;
    FPromptContinued : String;
    FQuitCmd       : String;
    FHistory       : THistory;
    FSigIntHandler : Boolean;  // if true a ^C signal handler is set before readline() is called
    FSigIntConvert : Boolean;  // raise EReadlineSigInt exception in signal handler to catch by higher-level
    FGetCompletions : TGetCompletionsFunc;
    FIsMultiLineComplete : TIsMultiLineCompleteFunc;
    Function  GetBasicWordBreakCharacters:String;
    Procedure SetBasicWordBreakCharacters(Const AValue: String);
    Function  GetSpecialPrefixes:String;
    Procedure SetSpecialPrefixes(Const AValue: String);
    Function  GetLineBuffer:PChar;
    Function  InternalRead(APrompt : PChar) : String;
    // TODO: rl_instream, rl_outstream
  public
    Constructor Create(AName:String);
    Destructor  Destroy; override;
    { the main functions }
    Function  Read : String;
    Function  ReadOneLine  (Out Line : String) : Boolean;
    Function  ReadMultiLine(Out Line : String) : Boolean;
    { helper functions }
    Procedure Show(St:String);
    Procedure CRLF; inline;
    { completion helper functions }
    Procedure UseFilenames; inline;
    { properties }
    property QuitCmd       : String   read FQuitCmd       write FQuitCmd;
    property Prompt        : String   read FPrompt        write FPrompt;
    property PromptContinued : String   read FPromptContinued write FPromptContinued;
    property History       : THistory read FHistory       write FHistory;
    property SigIntHandler : Boolean  read FSigIntHandler write FSigIntHandler;
    property SigIntConvert : Boolean  read FSigIntConvert write FSigIntConvert;
    property GetCompletions : TGetCompletionsFunc read FGetCompletions write FGetCompletions;
    property IsMultiLineComplete : TIsMultiLineCompleteFunc read FIsMultiLineComplete write FIsMultiLineComplete;
    { Readline variables }
    property BasicWordBreakCharacters : String read GetBasicWordBreakCharacters write SetBasicWordBreakCharacters;
    property SpecialPrefixes          : String read GetSpecialPrefixes          write SetSpecialPrefixes;
    property LineBuffer               : PChar  read GetLineBuffer;
  End;

  EReadline       = class(Exception);     // errors encountered in TReadline
  EReadlineSigInt = class(EReadline);     // special class raised in the SIGINT handler

Implementation

Uses BaseUnix;

// signelton static variable
Const ReadlineInst : TReadline = Nil;

(*
 * rl_completion_matches() is very simple: It calls the entry_function
 *   repeatedly until it returns Nil and places its return value into a growing
 *   array starting at [1] (growing by 10 to avoid too frequency realloc). Then
 *   it calls the compute_lcd_of_matches() to sort the entries and compute the
 *   least common deniminator of all possible completions. Unfortunately this
 *   function is a private, therefore we can't use it to generate our own LCD.
 *
 * Plan: We set AttemptCompletion as rl_attempted_completion_function. This
 *   calls a user-defined function of object, which returns an Array of String
 *   (trust in reference counting!). This is then stored to the global variable
 *   GlobalCompletions. Then it calls rl_completion_matches() for which we
 *   provide the callback function GetGlobalCompletions. This simply returns one
 *   entry from GlobalCompletions after each other.
 *
 *   AttemptCompletion and therefore the user-defined function get a lot more
 *   information as GetGlobalCompletions, so it is more powerful to choose smart
 *   completions.
 *)

// global variables to store completions
Const GlobalCompletions    : TDynArrString = Nil;
      GlobalCompletionsIdx : Integer = 0;

// rl_completion_matches() wants the strings to be malloc()ed and will be freed
// later, therefore we link to the libc strdup() (which is basically a wrapper
// for malloc()).
Function StrDup(para1:Pchar):Pchar;cdecl;external 'c' name 'strdup';

Function GetGlobalCompletions(TheText : PChar; Matches : CInt) : PChar;  CDecl;
Begin
  // first run: reset the index
  if Matches = 0 then
    GlobalCompletionsIdx := 0;

  // no more completions?
  if GlobalCompletionsIdx >= Length(GlobalCompletions) then
    Exit(Nil);

  // return the string (which must be malloc()ed!)
  Result := StrDup(PChar(GlobalCompletions[GlobalCompletionsIdx]));
  Inc(GlobalCompletionsIdx);
End;

Function AttemptedCompletion(Text:PChar;Start,TheEnd:Integer): PPChar; CDecl;
Begin
  Result := Nil;
  // by default we don't want to complete filenames if we don't have completions
  rl_attempted_completion_over := 1;

  // don't return any completions if no callback is defined
  if not assigned(ReadlineInst.GetCompletions) then
    Exit;

  // get all completions
  GlobalCompletions := ReadlineInst.GetCompletions(Text,Start,TheEnd);

  // make an appropriate array
  Result := rl_completion_matches(Text,@GetGlobalCompletions);
End;

{ TReadline }

Constructor TReadline.Create(AName:String);
Begin
  { readline is not thread-save }
  if assigned(ReadlineInst) then
    raise EReadline.Create('You can only have a single TReadline instance!');
  ReadlineInst := Self;     // set singleton static variable

  inherited Create;

  // set defaults
  FName    := AName;
  FPrompt  := '> ';
  FPromptContinued := '+ ';
  FQuitCmd := 'quit';
  FSigIntHandler := true;

  // setup readline
  rl_readline_name := PChar(FName);
  rl_attempted_completion_function := @AttemptedCompletion;
End;

Destructor TReadline.Destroy;
Begin
  { clean up singleton static variable }
  if ReadlineInst <> Self then
    raise EReadline.Create('Internal Error: ReadlineInst <> Self');
  ReadlineInst := Nil;

  Inherited Destroy;
End;

Procedure SigCtrlC(Signal:Longint;Info:PSigInfo;Context:PSigContext); CDecl;
Begin
  rl_crlf();   // readline only prints '^C' -> print newline

  // rl_done() doesn't work as I'd like: immedately return from readline().
  // Instead the user has to push at least one key to return.
  //
  // But how is bash doing this? They install a setjmp() position before the
  // readline() and then do a longjmp() from within the signal handler.
  //
  // We model this with exceptions and create the dedicated EReadlineSigInt for
  // it.
  //
  // There is another problem: Since this signal handler function is not
  // finished the sigreturn() call (injected by the kernel) is not executed.
  // Somehow the SIGINT stays blocked and no further ^C presses work. To work
  // around this problem, we clear the set of blocked signals directly before
  // readline() (see below).

  // "convert" signal to exception
  if ReadlineInst.FSigIntConvert then
    raise EReadlineSigInt.Create('SIGINT')
  else
    Begin
      // readline() continues to run, so clean information and redraw the screen
      rl_free_line_state();        // free partial history entry, keyboard macro, numeric argument
      rl_replace_line('', 1);      // clear line buffer
      rl_forced_update_display();  // force the line to be updated and redisplayed
    End;
End;

Function TReadline.InternalRead(APrompt:PChar):String;
Var InputLine     : PChar;
    NewAct,OldAct : SigActionRec;
    NewSig,OldSig : TSigSet;
Begin
  // install signal handler to ignore ^C
  // Readline by default catches many signals (including SIGINT), does some
  // terminal cleanup, and resend the signal to the calling application. So we
  // have to catch it ourselves.
  if FSigIntHandler then
    Begin
      NewAct.sa_handler := @SigCtrlC;
      NewAct.sa_flags   := SA_RESTART or SA_SIGINFO;  // SA_SIGINFO required that sa_sigaction(int, siginfo_t*, void*) instead of sa_handler(int) is called
      FpSigEmptySet(NewAct.sa_mask);
      if FpSigAction(SIGINT,@NewAct,@OldAct) <> 0 then
        raise EReadline.CreateFmt('Error installing SIGINT handler: %d (%s)',[FpGetErrno, '']);
      // un-block all signals
      FpSigEmptySet(NewSig);
      if FpSigProcMask(SIG_SETMASK,NewSig,OldSig) <> 0 then
        raise EReadline.CreateFmt('Error unblocking all signals: %d (%s)',[FpGetErrno, '']);
    End;

  // read a line
  InputLine := Readline.readline(APrompt);

  // remove signal handler
  if FSigIntHandler then
    Begin
      if FpSigAction(SIGINT,@OldAct,Nil) <> 0 then
        raise EReadline.CreateFmt('Error installing old SIGINT handler: %d (%s)',[FpGetErrno, '']);
    End;

  // handle ^D
  if InputLine = Nil then
    Begin
      Show(FQuitCmd);
      CRLF;  // print a newline because after pressing ^D there is none
      InputLine := PChar(FQuitCmd);
    End;
  Result := Trim(InputLine);
End;

Function TReadline.Read:String;
Begin
  Result := InternalRead(PChar(FPrompt));
  // if the string was non-empty, append to history
  if (Result > '') and assigned(FHistory) then
    FHistory.Add(Result);
End;

(**
 * Read one line
 *
 * Returns true on success, false if ^C was pressed
 *)
Function TReadline.ReadOneLine(Out Line : String):Boolean;
Begin
  Result := false;
  // get a line
  try
    Line := InternalRead(PChar(FPrompt));
  except
    On E : EReadlineSigInt do
      Begin
        { ^C pressed -> exit }
        Exit;
      End;
  End;

  // if the string was non-empty, append to history
  if (Line > '') and assigned(FHistory) then
    FHistory.Add(Line);

  // success
  Result := true;
End;

(**
 * Read one or more line
 *
 * The virtual method "IsComplete" is used to determine whether the line is
 * complete.
 *
 * Returns true on success, false if ^C was pressed
 *)
Function TReadline.ReadMultiLine(Out Line : String) : Boolean;
Var MPrompt  : String;
    OneLine  : String;
    Complete : Boolean;
Begin
  Result := false;
  // setup correct prompt
  MPrompt  := FPrompt;
  Line     := '';
  Complete := false;
  repeat
    // get a line
    try
      OneLine := InternalRead(PChar(MPrompt));
    except
      On E : EReadlineSigInt do
        Begin
          { ^C pressed -> exit }
          Exit;
        End;
    End;
    // append to full line
    if Line = '' then
      Line := OneLine
    else
      Line := Line + ' ' + OneLine;

    // unassigned IsMultiLineComplete also leads to leave the loop
    Complete := ((not assigned(IsMultiLineComplete)) or IsMultiLineComplete(Line))
                and (not (Line[Length(Line)] = '\'));
    if not Complete then           // incomplete command: special prompt
      MPrompt := FPromptContinued;
  Until Complete;

  // if the string was non-empty, append to history
  if (Line > '') and assigned(FHistory) then
    FHistory.Add(Line);

  // success
  Result := true;
End;

Procedure TReadline.Show(St:String);
Var I : Integer;
Begin
  For I := 1 to Length(St) do
    rl_show_char(Ord(St[I]));
End;

Procedure TReadline.CRLF;Inline;
Begin
  rl_crlf;
End;

Procedure TReadline.UseFilenames; inline;
Begin
  // Called from the user-defined function to return a list of completions.
  // Re-enable filename completions.
  rl_attempted_completion_over := 0;
End;

Function TReadline.GetBasicWordBreakCharacters:String;
Begin
  Result := rl_basic_word_break_characters;
End;

Procedure TReadline.SetBasicWordBreakCharacters(Const AValue: String);
Begin
  rl_basic_word_break_characters := PChar(AValue);
End;

Function TReadline.GetSpecialPrefixes:String;
Begin
  Result := rl_special_prefixes;
End;

Procedure TReadline.SetSpecialPrefixes(Const AValue: String);
Begin
  rl_special_prefixes := PChar(AValue);
End;

Function TReadline.GetLineBuffer:PChar;
Begin
  Result := rl_line_buffer;
End;

End.


Unit CmdLine;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, HistoryOOP, ReadlineOOP;

Type

  TExecuteFunc = Function(Command:String):Boolean of object;  // return true if the command loop should be exited

  { TCmdLine }

  (**
   * Wrapper for Readline and History including a command loop
   *)
  TCmdLine = class
  private
    FHistory  : THistory;
    FReadline : TReadline;
    FExecute  : TExecuteFunc;
  public
    Constructor Create(AHistoryFile:String;AReadlineName:String);
    Destructor  Destroy; override;

    Procedure CommandLoop; virtual;

    property History  : THistory  read FHistory;
    property Readline : TReadline read FReadline;
    property Execute  : TExecuteFunc read FExecute write FExecute;
  End;

Implementation

{ TCmdLine }

Constructor TCmdLine.Create(AHistoryFile:String;AReadlineName:String);
Begin
  inherited Create;

  FHistory := THistory.Create(AHistoryFile);
  FHistory.Read;    // read history

  FReadline := TReadline.Create(AReadlineName);
  FReadline.Prompt                   := '>> ';
  FReadline.PromptContinued          := ' > ';
  FReadline.QuitCmd                  := 'exit';
  FReadline.SigIntConvert            := true;                      // raise exception EReadlineSigInt when ^C is pressed
  FReadline.BasicWordBreakCharacters := ' "\''`$@><=;|&{(['^J^I;   // The basic list of characters that signal a break between words for the completer routine.
  FReadline.SpecialPrefixes          := '@$';                      // The list of characters that are word break characters, but should be left in text when it is passed to the completion function.
  FReadline.History                  := FHistory;
End;

Destructor TCmdLine.Destroy;
Begin
  FHistory.Write;   // write history to file
  FHistory.Free;
  FReadline.Free;

  inherited Destroy;
End;

Procedure TCmdLine.CommandLoop;
Var Command  : String;
    ExitLoop : Boolean;
Begin
  if not assigned(FExecute) then
    raise Exception.Create('You have to assign an execute method');
  // command line
  ExitLoop := false;  // "continue" would break the loop if the "until"-condition is true
  repeat
    if not FReadline.ReadMultiLine(Command) then
      Continue; // don't execute if ^C was pressed
    // don't execute empty command
    if Command = '' then
      Continue;

    // execute
    ExitLoop := FExecute(Command);
    // flush output (this is necessary of the output is redirected)
    Flush(ErrOutput);
    Flush(Output);
    Flush(StdOut);
    Flush(StdErr);
  Until ExitLoop;
End;

End.


Program ExWikipedia;
Uses SysUtils,BaseUnix,Readline,History;

Var
  TheInput    : PChar;
  ShellPrompt : AnsiString;

Begin
  repeat
    // getting the current user and path
    ShellPrompt := GetEnvironmentVariable('USER') + ':' + FpGetCwd + ' $ ';
    // inputing...
    TheInput := Readline.readline(PChar(ShellPrompt));
    // eof (Ctrl-D)
    if TheInput = Nil then Break;
    // path autocompletion when tabulation hit
    rl_bind_key(Ord(^I),@rl_complete);
    // adding the previous input into history
    add_history(Theinput);

    (* do stuff *)

    WriteLn('You entered: "',TheInput,'"');

  until false;
  WriteLn;
End.


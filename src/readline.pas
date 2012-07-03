(**
 * Header translation for the GNU Readline Library
 *
 * http://tiswww.case.edu/php/chet/readline/rltop.html
 *
 * 2011-04-30 Johann Glaser
 *
 *)
{$MODE ObjFpc}
Unit Readline;

(* Readline.h -- the names of functions callable from within readline. *)

(* Copyright (C) 1987-2011 Free Software Foundation, Inc.

   This file is part of the GNU Readline Library (Readline), a library
   for reading lines of text with interactive input and history editing.

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

Interface
Uses CTypes;

{$LINKLIB readline}

//#  include "rlstdc.h"
{$I rltypedefs.inc.pas}
{$I keymaps.inc.pas}
//#  include "tilde.h"
//
//(* Hex-encoded Readline version number. *)
//#define RL_READLINE_VERSION	0x0602		(* Readline 6.2 *)
//#define RL_VERSION_MAJOR	6
//#define RL_VERSION_MINOR	2

(* Readline data structures. *)

(* Maintaining the state of undo.  We remember individual deletes and inserts
   on a chain of things to do. *)

(* The actions that undo knows how to undo.  Notice that UNDO_DELETE means
   to insert some text, and UNDO_INSERT means to delete some text.   I.e.,
   the code tells undo what to undo, not how to undo it. *)

Type
  undo_code = ( UNDO_DELETE, UNDO_INSERT, UNDO_BEGIN, UNDO_END );
  // TODO: is a C-Enum identical to a Pascal Enum?

(* What an element of THE_UNDO_LIST looks like. *)
  Pundo_list = ^Tundo_list;
  Tundo_list = record
    Next         : Pundo_list;
    Start,TheEnd : CInt;        (* Where the change took place. *)
    Thetext      : PChar;       (* The text to insert, if undoing a delete. *)
    what         : undo_code;   (* Delete, Insert, Begin, End. *)
  End;

(* The current undo list for RL_LINE_BUFFER. *)
Var
  rl_undo_list : Pundo_list; external;

(* The data structure for mapping textual names to code addresses. *)
Type
  PPfunmap = ^Pfunmap;
  Pfunmap = ^Tfunmap;
  Tfunmap = record
    Name        : PChar;
    TheFunction : rl_command_func_t;
  End;

Var
  funmap : PPfunmap; cvar; external;

(* **************************************************************** *)
(*								    *)
(*	     Functions available to bind to key sequences	    *)
(*								    *)
(* **************************************************************** *)

(* Bindable commands for numeric arguments. *)
Function rl_digit_argument(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_universal_argument(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* Bindable commands for moving the cursor. *)
Function rl_forward_byte(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_forward_char(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_forward(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_backward_byte(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_backward_char(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_backward(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_beg_of_line(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_end_of_line(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_forward_word(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_backward_word(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_refresh_line(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_clear_screen(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_skip_csi_sequence(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_arrow_keys(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* Bindable commands for inserting and deleting text. *)
Function rl_insert(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_quoted_insert(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_tab_insert(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_newline(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_do_lowercase_version(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_rubout(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_delete(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_rubout_or_delete(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_delete_horizontal_space(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_delete_or_show_completions(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_insert_comment(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* Bindable commands for changing case. *)
Function rl_upcase_word(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_downcase_word(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_capitalize_word(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* Bindable commands for transposing characters and words. *)
Function rl_transpose_words(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_transpose_chars(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* Bindable commands for searching within a line. *)
Function rl_char_search(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_backward_char_search(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* Bindable commands for readline's interface to the command history. *)
Function rl_beginning_of_history(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_end_of_history(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_get_next_history(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_get_previous_history(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* Bindable commands for managing the mark and region. *)
Function rl_set_mark(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_exchange_point_and_mark(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* Bindable commands to set the editing mode (emacs or vi). *)
Function rl_vi_editing_mode(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_emacs_editing_mode(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* Bindable commands to change the insert mode (insert or overwrite) *)
Function rl_overwrite_mode(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* Bindable commands for managing key bindings. *)
Function rl_re_read_init_file(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_dump_functions(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_dump_macros(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_dump_variables(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* Bindable commands for word completion. *)
Function rl_complete(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_possible_completions(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_insert_completions(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_old_menu_complete(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_menu_complete(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_backward_menu_complete(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* Bindable commands for killing and yanking text, and managing the kill ring. *)
Function rl_kill_word(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_backward_kill_word(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_kill_line(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_backward_kill_line(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_kill_full_line(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_unix_word_rubout(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_unix_filename_rubout(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_unix_line_discard(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_copy_region_to_kill(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_kill_region(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_copy_forward_word(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_copy_backward_word(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_yank(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_yank_pop(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_yank_nth_arg(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_yank_last_arg(Count:CInt;Key:CInt) : CInt; cdecl; external;
(* Not available unless __CYGWIN__ is defined. *)
//#ifdef __CYGWIN__
//Function rl_paste_from_clipboard(Count:CInt;Key:CInt) : CInt; cdecl; external;
//#endif

(* Bindable commands for incremental searching. *)
Function rl_reverse_search_history(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_forward_search_history(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* Bindable keyboard macro commands. *)
Function rl_start_kbd_macro(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_end_kbd_macro(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_call_last_kbd_macro(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* Bindable undo commands. *)
Function rl_revert_line(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_undo_command(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* Bindable tilde expansion commands. *)
Function rl_tilde_expand(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* Bindable terminal control commands. *)
Function rl_restart_output(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_stop_output(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* Miscellaneous bindable commands. *)
Function rl_abort(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_tty_status(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* Bindable commands for incremental and non-incremental history searching. *)
Function rl_history_search_forward(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_history_search_backward(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_noninc_forward_search(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_noninc_reverse_search(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_noninc_forward_search_again(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_noninc_reverse_search_again(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* Bindable command used when inserting a matching close character. *)
Function rl_insert_close(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* Not available unless READLINE_CALLBACKS is defined. *)
Procedure rl_callback_handler_install(Prompt:PChar;Callback:rl_vcpfunc_t); cdecl; external;
Procedure rl_callback_read_char; cdecl; external;
Procedure rl_callback_handler_remove; cdecl; external;

(* Things for vi mode. Not available unless readline is compiled -DVI_MODE. *)
(* VI-mode bindable commands. *)
Function rl_vi_redo(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_undo(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_yank_arg(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_fetch_history(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_search_again(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_search(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_complete(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_tilde_expand(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_prev_word(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_next_word(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_end_word(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_insert_beg(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_append_mode(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_append_eol(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_eof_maybe(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_insertion_mode(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_insert_mode(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_movement_mode(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_arg_digit(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_change_case(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_put(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_column(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_delete_to(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_change_to(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_yank_to(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_rubout(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_delete(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_back_to_indent(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_first_print(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_char_search(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_match(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_change_char(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_subst(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_overstrike(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_overstrike_delete(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_replace(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_set_mark(Count:CInt;Key:CInt) : CInt; cdecl; external;
Function rl_vi_goto_mark(Count:CInt;Key:CInt) : CInt; cdecl; external;

(* VI-mode utility functions. *)
Function rl_vi_check : CInt; cdecl; external;
Function rl_vi_domove(x:CInt;Ignore:PCInt) : CInt; cdecl; external;
Function rl_vi_bracktype(Character:CInt) : CInt; cdecl; external;

Procedure rl_vi_start_inserting(Key,TheRepeat,Sign:CInt); cdecl; external;

(* VI-mode pseudo-bindable commands, used as utility functions. *)
Function rl_vi_fUWord(Count:CInt;Key:CInt) : CInt; cdecl; external name 'rl_vi_fWord';
Function rl_vi_bUWord(Count:CInt;Key:CInt) : CInt; cdecl; external name 'rl_vi_bWord';
Function rl_vi_eUWord(Count:CInt;Key:CInt) : CInt; cdecl; external name 'rl_vi_eWord';
Function rl_vi_fLword(Count:CInt;Key:CInt) : CInt; cdecl; external name 'rl_vi_fword';
Function rl_vi_bLword(Count:CInt;Key:CInt) : CInt; cdecl; external name 'rl_vi_bword';
Function rl_vi_eLword(Count:CInt;Key:CInt) : CInt; cdecl; external name 'rl_vi_eword';

(* **************************************************************** *)
(*								    *)
(*			Well Published Functions		    *)
(*								    *)
(* **************************************************************** *)

(* Readline functions. *)
(* Read a line of input.  Prompt with PROMPT.  A NULL PROMPT means none. *)
Function readline        (Prompt:PChar) : PChar; cdecl; external;

Function rl_set_prompt   (Prompt:PChar):cint;cdecl;external;
Function rl_expand_prompt(Prompt:PChar):cint;cdecl;external;

Function rl_initialize:cint;cdecl;external;

(* Undocumented; unused by readline *)
Function rl_discard_argument:cint;cdecl;external;

(* Utility functions to bind keys to readline commands. *)
Function rl_add_defun(Name:PChar; TheFunction:rl_command_func_t; Key:cint):cint;cdecl;external;
Function rl_bind_key(Key:cint; TheFunction:rl_command_func_t):cint;cdecl;external;
Function rl_bind_key_in_map(Key:cint; TheFunction:rl_command_func_t; Map:Keymap):cint;cdecl;external;
Function rl_unbind_key(Key:cint):cint;cdecl;external;
Function rl_unbind_key_in_map(Key:cint; Map:Keymap):cint;cdecl;external;
Function rl_bind_key_if_unbound(Key:cint; TheFunction:rl_command_func_t):cint;cdecl;external;
Function rl_bind_key_if_unbound_in_map(Key:cint; TheFunction:rl_command_func_t; Map:Keymap):cint;cdecl;external;
Function rl_unbind_function_in_map(TheFunction:rl_command_func_t; Map:Keymap):cint;cdecl;external;
Function rl_unbind_command_in_map(Command:PChar; Map:Keymap):cint;cdecl;external;
Function rl_bind_keyseq(KeySeq:PChar; TheFunction:rl_command_func_t):cint;cdecl;external;
Function rl_bind_keyseq_in_map(KeySeq:PChar; TheFunction:rl_command_func_t; Map:Keymap):cint;cdecl;external;
Function rl_bind_keyseq_if_unbound(KeySeq:PChar; TheFunction:rl_command_func_t):cint;cdecl;external;
Function rl_bind_keyseq_if_unbound_in_map(KeySeq:PChar; TheFunction:rl_command_func_t; Map:Keymap):cint;cdecl;external;
Function rl_generic_bind(TheType:cint; KeySeq:PChar; Data:PChar; Map:Keymap):cint;cdecl;external;
Function rl_variable_value(Variable:PChar):PChar;cdecl;external;
Function rl_variable_bind(Variable:PChar; Value:PChar):cint;cdecl;external;

(* Backwards compatibility, use rl_bind_keyseq_in_map instead. *)
Function rl_set_key(KeySeq:PChar; TheFunction:rl_command_func_t; Map:Keymap):cint;cdecl;external;

(* Backwards compatibility, use rl_generic_bind instead. *)
Function rl_macro_bind(KeySeq:PChar; Macro:PChar; Map:Keymap):cint;cdecl;external;

(* Undocumented in the texinfo manual; not really useful to programs. *)
Function rl_translate_keyseq(Seq:PChar; TheArray:PChar; Len:pcint):cint;cdecl;external;
Function rl_untranslate_keyseq(Seq:cint):PChar;cdecl;external;

Function rl_named_function(Name:PChar):rl_command_func_t;cdecl;external;
Function rl_function_of_keyseq(KeySeq:PChar; Map:Keymap; TheType:pcint):rl_command_func_t;cdecl;external;

procedure rl_list_funmap_names;cdecl;external;
Function rl_invoking_keyseqs_in_map(TheFunction:rl_command_func_t; Map:Keymap):PPChar;cdecl;external;
Function rl_invoking_keyseqs(TheFunction:rl_command_func_t):PPChar;cdecl;external;

procedure rl_function_dumper(Readable:cint);cdecl;external;
procedure rl_macro_dumper(Readable:cint);cdecl;external;
procedure rl_variable_dumper(Readable:cint);cdecl;external;

Function rl_read_init_file(Filename:PChar):cint;cdecl;external;
Function rl_parse_and_bind(Line:PChar):cint;cdecl;external;

// duplicate definition in keymaps.inc.pas
//(* Functions for manipulating keymaps. *)
//Function rl_make_bare_keymap:Keymap;cdecl;external;
//Function rl_copy_keymap(Map:Keymap):Keymap;cdecl;external;
//Function rl_make_keymap:Keymap;cdecl;external;
//procedure rl_discard_keymap(Map:Keymap);cdecl;external;
//
//Function rl_get_keymap_by_name(Name:PChar):Keymap;cdecl;external;
//Function rl_get_keymap_name(Map:Keymap):PChar;cdecl;external;
//procedure rl_set_keymap(Map:Keymap);cdecl;external;
//Function rl_get_keymap:Keymap;cdecl;external;
(* Undocumented; used internally only. *)
procedure rl_set_keymap_from_edit_mode;cdecl;external;
Function rl_get_keymap_name_from_edit_mode:PChar;cdecl;external;

(* Functions for manipulating the funmap, which maps command names to functions. *)
Function rl_add_funmap_entry(Name:PChar; TheFunction:rl_command_func_t):cint;cdecl;external;
Function rl_funmap_names:PPChar;cdecl;external;
(* Undocumented, only used internally -- there is only one funmap, and this
   function may be called only once. *)
procedure rl_initialize_funmap;cdecl;external;

(* Utility functions for managing keyboard macros. *)
procedure rl_push_macro_input(Macro:PChar);cdecl;external;

(* Functions for undoing, from undo.c *)
procedure rl_add_undo(What:undo_code; Start:cint; TheEnd:cint; TheText:PChar);cdecl;external;
procedure rl_free_undo_list;cdecl;external;
Function rl_do_undo:cint;cdecl;external;
Function rl_begin_undo_group:cint;cdecl;external;
Function rl_end_undo_group:cint;cdecl;external;
Function rl_modifying(Start:cint; TheEnd:cint):cint;cdecl;external;

(* Functions for redisplay. *)
procedure rl_redisplay;cdecl;external;
Function rl_on_new_line:cint;cdecl;external;
Function rl_on_new_line_with_prompt:cint;cdecl;external;
Function rl_forced_update_display:cint;cdecl;external;
Function rl_clear_message:cint;cdecl;external;
Function rl_reset_line_state:cint;cdecl;external;
Function rl_crlf:cint;cdecl;external;

//#if defined (USE_VARARGS) && defined (PREFER_STDARG)
Function rl_message(Format:PChar;Params:Array of Const) : CInt; cdecl; external;
//#else
//extern int rl_message ();
//#endif

Function rl_show_char(C:cint):cint;cdecl;external;

(* Undocumented in texinfo manual. *)
Function rl_character_len(C:cint; Pos:cint):cint;cdecl;external;

(* Save and restore internal prompt redisplay information. *)
procedure rl_save_prompt;cdecl;external;
procedure rl_restore_prompt;cdecl;external;

(* Modifying text. *)
procedure rl_replace_line(TheText:PChar; ClearUndo:cint);cdecl;external;
Function rl_insert_text(TheText:PChar):cint;cdecl;external;
Function rl_delete_text(Start:cint; TheEnd:cint):cint;cdecl;external;
Function rl_kill_text(Start:cint; TheEnd:cint):cint;cdecl;external;
Function rl_copy_text(Start:cint; TheEnd:cint):PChar;cdecl;external;

(* Terminal and tty mode management. *)
procedure rl_prep_terminal(MetaFlag:cint);cdecl;external;
procedure rl_deprep_terminal;cdecl;external;
procedure rl_tty_set_default_bindings(Map:Keymap);cdecl;external;
procedure rl_tty_unset_default_bindings(Map:Keymap);cdecl;external;

Function rl_reset_terminal(TerminalName:PChar):cint;cdecl;external;
procedure rl_resize_terminal;cdecl;external;
procedure rl_set_screen_size(Rows:cint; Cols:cint);cdecl;external;
procedure rl_get_screen_size(Rows:pcint; Cols:pcint);cdecl;external;
procedure rl_reset_screen_size;cdecl;external;

Function rl_get_termcap(Cap:PChar):PChar;cdecl;external;

(* Functions for character input. *)
Function rl_stuff_char(C:cint):cint;cdecl;external;
Function rl_execute_next(C:cint):cint;cdecl;external;
Function rl_clear_pending_input:cint;cdecl;external;
Function rl_read_key:cint;cdecl;external;
Function rl_getc(Stream:PCFILE):cint;cdecl;external;
Function rl_set_keyboard_input_timeout(U:cint):cint;cdecl;external;

(* `Public' utility functions . *)
procedure rl_extend_line_buffer(Len:cint);cdecl;external;
Function rl_ding:cint;cdecl;external;
Function rl_alphabetic(C:cint):cint;cdecl;external;
procedure rl_free(Mem:pointer);cdecl;external;

(* Readline signal handling, from signals.c *)
Function rl_set_signals:cint;cdecl;external;
Function rl_clear_signals:cint;cdecl;external;
procedure rl_cleanup_after_signal;cdecl;external;
procedure rl_reset_after_signal;cdecl;external;
procedure rl_free_line_state;cdecl;external;

procedure rl_echo_signal_char(Sig:cint);cdecl;external;

Function rl_set_paren_blink_timeout(U:cint):cint;cdecl;external;

(* Undocumented. *)
Function rl_maybe_save_line:cint;cdecl;external;
Function rl_maybe_unsave_line:cint;cdecl;external;
Function rl_maybe_replace_line:cint;cdecl;external;

(* Completion functions. *)
Function rl_complete_internal(WhatToDo:cint):cint;cdecl;external;
procedure rl_display_match_list(Matches:PPChar; Len:cint; Max:cint);cdecl;external;

Function rl_completion_matches(TheText:PChar; EntryFunc:rl_compentry_func_t):PPChar;cdecl;external;
Function rl_username_completion_function(TheText:PChar; State:cint):PChar;cdecl;external;
Function rl_filename_completion_function(TheText:PChar; State:cint):PChar;cdecl;external;

Function rl_completion_mode(CFunc:rl_command_func_t):cint;cdecl;external;

//#if 0
//(* Backwards compatibility (compat.c).  These will go away sometime. *)
//extern void free_undo_list PARAMS((void));
//extern int maybe_save_line PARAMS((void));
//extern int maybe_unsave_line PARAMS((void));
//extern int maybe_replace_line PARAMS((void));
//
//extern int ding PARAMS((void));
//extern int alphabetic PARAMS((int));
//extern int crlf PARAMS((void));
//
//extern char **completion_matches PARAMS((char *, rl_compentry_func_t *));
//extern char *username_completion_function PARAMS((const char *, int));
//extern char *filename_completion_function PARAMS((const char *, int));
//#endif

(* **************************************************************** *)
(*								    *)
(*			Well Published Variables		    *)
(*								    *)
(* **************************************************************** *)

Var
(* The version of this incarnation of the readline library. *)
  rl_library_version : PChar;cvar;external;   (* e.g., "4.2" *)
  rl_readline_version : cint;cvar;external;   (* e.g., 0x0402 *)

(* True if this is real GNU readline. *)
  rl_gnu_readline_p : cint;cvar;external;

(* Flags word encapsulating the current readline state. *)
  rl_readline_state : cint;cvar;external;

(* Says which editing mode readline is currently using.  1 means emacs mode;
   0 means vi mode. *)
  rl_editing_mode : cint;cvar;external;

(* Insert or overwrite mode for emacs mode.  1 means insert mode; 0 means
   overwrite mode.  Reset to insert mode on each input line. *)
  rl_insert_mode : cint;cvar;external;

(* The name of the calling program.  You should initialize this to
   whatever was in argv[0].  It is used when parsing conditionals. *)
  rl_readline_name : PChar;cvar;external;

(* The prompt readline uses.  This is set from the argument to
   readline (), and should not be assigned to directly. *)
  rl_prompt : PChar;cvar;external;

(* The prompt string that is actually displayed by rl_redisplay.  Public so
   applications can more easily supply their own redisplay functions. *)
  rl_display_prompt : PChar;cvar;external;

(* The line buffer that is in use. *)
  rl_line_buffer : PChar;cvar;external;

(* The location of point, and end. *)
  rl_point : cint;cvar;external;
  rl_end : cint;cvar;external;

(* The mark, or saved cursor position. *)
  rl_mark : cint;cvar;external;

(* Flag to indicate that readline has finished with the current input
   line and should return it. *)
  rl_done : cint;cvar;external;

(* If set to a character value, that will be the next keystroke read. *)
  rl_pending_input : cint;cvar;external;

(* Non-zero if we called this function from _rl_dispatch().  It's present
   so functions can find out whether they were called from a key binding
   or directly from an application. *)
  rl_dispatching : cint;cvar;external;

(* Non-zero if the user typed a numeric argument before executing the
   current function. *)
  rl_explicit_arg : cint;cvar;external;

(* The current value of the numeric argument specified by the user. *)
  rl_numeric_arg : cint;cvar;external;

(* The address of the last command function Readline executed. *)
  rl_last_func : rl_command_func_t;cvar;external;

(* The name of the terminal to use. *)
  rl_terminal_name : PChar;cvar;external;

(* The input and output streams. *)
  rl_instream  : PCFILE;cvar;external;
  rl_outstream : PCFILE;cvar;external;

(* If non-zero, Readline gives values of LINES and COLUMNS from the environment
   greater precedence than values fetched from the kernel when computing the
   screen dimensions. *)
  rl_prefer_env_winsize : cint;cvar;external;

(* If non-zero, then this is the address of a function to call just
   before readline_internal () prints the first prompt. *)
  rl_startup_hook : rl_hook_func_t;cvar;external;

(* If non-zero, this is the address of a function to call just before
   readline_internal_setup () returns and readline_internal starts
   reading input characters. *)
  rl_pre_input_hook : rl_hook_func_t;cvar;external;

(* The address of a function to call periodically while Readline is
   awaiting character input, or NULL, for no event handling. *)
  rl_event_hook : rl_hook_func_t;cvar;external;

(* The address of the function to call to fetch a character from the current
   Readline input stream *)
  rl_getc_function : rl_getc_func_t;cvar;external;

  rl_redisplay_function : rl_voidfunc_t;cvar;external;

  rl_prep_term_function : rl_vintfunc_t;cvar;external;
  rl_deprep_term_function : rl_voidfunc_t;cvar;external;

(* Dispatch variables. *)
  rl_executing_keymap : Keymap;cvar;external;
  rl_binding_keymap : Keymap;cvar;external;

(* Display variables. *)
(* If non-zero, readline will erase the entire line, including any prompt,
   if the only thing typed on an otherwise-blank line is something bound to
   rl_newline. *)
  rl_erase_empty_line : cint;cvar;external;

(* If non-zero, the application has already printed the prompt (rl_prompt)
   before calling readline, so readline should not output it the first time
   redisplay is done. *)
  rl_already_prompted : cint;cvar;external;

(* A non-zero value means to read only this many characters rather than
   up to a character bound to accept-line. *)
  rl_num_chars_to_read : cint;cvar;external;

(* The text of a currently-executing keyboard macro. *)
  rl_executing_macro : PChar;cvar;external;

(* Variables to control readline signal handling. *)
(* If non-zero, readline will install its own signal handlers for
   SIGINT, SIGTERM, SIGQUIT, SIGALRM, SIGTSTP, SIGTTIN, and SIGTTOU. *)
  rl_catch_signals : cint;cvar;external;

(* If non-zero, readline will install a signal handler for SIGWINCH
   that also attempts to call any calling application's SIGWINCH signal
   handler.  Note that the terminal is not cleaned up before the
   application's signal handler is called; use rl_cleanup_after_signal()
   to do that. *)
  rl_catch_sigwinch : cint;cvar;external;

(* Completion variables. *)
(* Pointer to the generator function for completion_matches ().
   NULL means to use rl_filename_completion_function (), the default
   filename completer. *)
  rl_completion_entry_function : rl_compentry_func_t;cvar;external;

(* Optional generator for menu completion.  Default is
   rl_completion_entry_function (rl_filename_completion_function). *)
  rl_menu_completion_entry_function : rl_compentry_func_t;cvar;external;

(* If rl_ignore_some_completions_function is non-NULL it is the address
   of a function to call after all of the possible matches have been
   generated, but before the actual completion is done to the input line.
   The function is called with one argument; a NULL terminated array
   of (char * ).  If your function removes any of the elements, they
   must be free()'ed. *)
  rl_ignore_some_completions_function : rl_compignore_func_t;cvar;external;

(* Pointer to alternative function to create matches.
   Function is called with TEXT, START, and END.
   START and END are indices in RL_LINE_BUFFER saying what the boundaries
   of TEXT are.
   If this function exists and returns NULL then call the value of
   rl_completion_entry_function to try to match, otherwise use the
   array of strings returned. *)
  rl_attempted_completion_function : rl_completion_func_t;cvar;external;

(* The basic list of characters that signal a break between words for the
   completer routine.  The initial contents of this variable is what
   breaks words in the shell, i.e. "n\"\\'`@$> . *)
  rl_basic_word_break_characters : PChar;cvar;external;

(* The list of characters that signal a break between words for
   rl_complete_internal.  The default list is the contents of
   rl_basic_word_break_characters.  *)
  rl_completer_word_break_characters : PChar;cvar;external;

(* Hook function to allow an application to set the completion word
   break characters before readline breaks up the line.  Allows
   position-dependent word break characters. *)
  rl_completion_word_break_hook : rl_cpvfunc_t;cvar;external;

(* List of characters which can be used to quote a substring of the line.
   Completion occurs on the entire substring, and within the substring
   rl_completer_word_break_characters are treated as any other character,
   unless they also appear within this list. *)
  rl_completer_quote_characters : PChar;cvar;external;

(* List of quote characters which cause a word break. *)
  rl_basic_quote_characters : PChar;cvar;external;

(* List of characters that need to be quoted in filenames by the completer. *)
  rl_filename_quote_characters : PChar;cvar;external;

(* List of characters that are word break characters, but should be left
   in TEXT when it is passed to the completion function.  The shell uses
   this to help determine what kind of completing to do. *)
  rl_special_prefixes : PChar;cvar;external;

(* If non-zero, then this is the address of a function to call when
   completing on a directory name.  The function is called with
   the address of a string (the current directory name) as an arg.  It
   changes what is displayed when the possible completions are printed
   or inserted.  The directory completion hook should perform
   any necessary dequoting.  This function should return 1 if it modifies
   the directory name pointer passed as an argument.  If the directory
   completion hook returns 0, it should not modify the directory name
   pointer passed as an argument. *)
  rl_directory_completion_hook : rl_icppfunc_t;cvar;external;

(* If non-zero, this is the address of a function to call when completing
   a directory name.  This function takes the address of the directory name
   to be modified as an argument.  Unlike rl_directory_completion_hook, it
   only modifies the directory name used in opendir(2), not what is displayed
   when the possible completions are printed or inserted.  If set, it takes
   precedence over rl_directory_completion_hook.  The directory rewrite
   hook should perform any necessary dequoting.  This function has the same
   return value properties as the directory_completion_hook.

   I'm not happy with how this works yet, so it's undocumented.  I'm trying
   it in bash to see how well it goes. *)
  rl_directory_rewrite_hook : rl_icppfunc_t;cvar;external;

(* If non-zero, this is the address of a function to call when reading
   directory entries from the filesystem for completion and comparing
   them to the partial word to be completed.  The function should
   either return its first argument (if no conversion takes place) or
   newly-allocated memory.  This can, for instance, convert filenames
   between character sets for comparison against what's typed at the
   keyboard.  The returned value is what is added to the list of
   matches.  The second argument is the length of the filename to be
   converted. *)
  rl_filename_rewrite_hook : rl_dequote_func_t;cvar;external;

(* Backwards compatibility with previous versions of readline. *)
//#define rl_symbolic_link_hook rl_directory_completion_hook

(* If non-zero, then this is the address of a function to call when
   completing a word would normally display the list of possible matches.
   This function is called instead of actually doing the display.
   It takes three arguments: (char **matches, int num_matches, int max_length)
   where MATCHES is the array of strings that matched, NUM_MATCHES is the
   number of strings in that array, and MAX_LENGTH is the length of the
   longest string in that array. *)
  rl_completion_display_matches_hook : rl_compdisp_func_t;cvar;external;

(* Non-zero means that the results of the matches are to be treated
   as filenames.  This is ALWAYS zero on entry, and can only be changed
   within a completion entry finder function. *)
  rl_filename_completion_desired : cint;cvar;external;

(* Non-zero means that the results of the matches are to be quoted using
   double quotes (or an application-specific quoting mechanism) if the
   filename contains any characters in rl_word_break_chars.  This is
   ALWAYS non-zero on entry, and can only be changed within a completion
   entry finder function. *)
  rl_filename_quoting_desired : cint;cvar;external;

(* Set to a function to quote a filename in an application-specific fashion.
   Called with the text to quote, the type of match found (single or multiple)
   and a pointer to the quoting character to be used, which the function can
   reset if desired. *)
  rl_filename_quoting_function : rl_quote_func_t;cvar;external;

(* Function to call to remove quoting characters from a filename.  Called
   before completion is attempted, so the embedded quotes do not interfere
   with matching names in the file system. *)
  rl_filename_dequoting_function : rl_dequote_func_t;cvar;external;

(* Function to call to decide whether or not a word break character is
   quoted.  If a character is quoted, it does not break words for the
   completer. *)
  rl_char_is_quoted_p : rl_linebuf_func_t;cvar;external;

(* Non-zero means to suppress normal filename completion after the
   user-specified completion function has been called. *)
  rl_attempted_completion_over : cint;cvar;external;

(* Set to a character describing the type of completion being attempted by
   rl_complete_internal; available for use by application completion
   functions. *)
  rl_completion_type : cint;cvar;external;

(* Set to the last key used to invoke one of the completion functions *)
  rl_completion_invoking_key : cint;cvar;external;

(* Up to this many items will be displayed in response to a
   possible-completions call.  After that, we ask the user if she
   is sure she wants to see them all.  The default value is 100. *)
  rl_completion_query_items : cint;cvar;external;

(* Character appended to completed words when at the end of the line.  The
   default is a space.  Nothing is added if this is '\0'. *)
  rl_completion_append_character : cint;cvar;external;

(* If set to non-zero by an application completion function,
   rl_completion_append_character will not be appended. *)
  rl_completion_suppress_append : cint;cvar;external;

(* Set to any quote character readline thinks it finds before any application
   completion function is called. *)
  rl_completion_quote_character : cint;cvar;external;

(* Set to a non-zero value if readline found quoting anywhere in the word to
   be completed; set before any application completion function is called. *)
  rl_completion_found_quote : cint;cvar;external;

(* If non-zero, the completion functions don't append any closing quote.
   This is set to 0 by rl_complete_internal and may be changed by an
   application-specific completion function. *)
  rl_completion_suppress_quote : cint;cvar;external;

(* If non-zero, readline will sort the completion matches.  On by default. *)
  rl_sort_completion_matches : cint;cvar;external;

(* If non-zero, a slash will be appended to completed filenames that are
   symbolic links to directory names, subject to the value of the
   mark-directories variable (which is user-settable).  This exists so
   that application completion functions can override the user's preference
   (set via the mark-symlinked-directories variable) if appropriate.
   It's set to the value of _rl_complete_mark_symlink_dirs in
   rl_complete_internal before any application-specific completion
   function is called, so without that function doing anything, the user's
   preferences are honored. *)
  rl_completion_mark_symlink_dirs : cint;cvar;external;

(* If non-zero, then disallow duplicates in the matches. *)
  rl_ignore_completion_duplicates : cint;cvar;external;

(* If this is non-zero, completion is (temporarily) inhibited, and the
   completion character will be inserted as any other. *)
  rl_inhibit_completion : cint;cvar;external;

Const
(* Input error; can be returned by ( *rl_getc_function) if readline is reading
   a top-level command (RL_ISSTATE (RL_STATE_READCMD)). *)
  READERR = -2;

(* Definitions available for use by readline clients. *)
  RL_PROMPT_START_IGNORE = #$01;
  RL_PROMPT_END_IGNORE = #$02;

(* Possible values for do_replace argument to rl_filename_quoting_function,
   called by rl_complete_internal. *)
  NO_MATCH     = 0;
  SINGLE_MATCH = 1;
  MULT_MATCH   = 2;

(* Possible state values for rl_readline_state *)
  RL_STATE_NONE           = $000000;   (* no state; before first call *)

  RL_STATE_INITIALIZING   = $0000001;   (* initializing *)
  RL_STATE_INITIALIZED    = $0000002;   (* initialization done *)
  RL_STATE_TERMPREPPED    = $0000004;   (* terminal is prepped *)
  RL_STATE_READCMD        = $0000008;   (* reading a command key *)
  RL_STATE_METANEXT       = $0000010;   (* reading input after ESC *)
  RL_STATE_DISPATCHING    = $0000020;   (* dispatching to a command *)
  RL_STATE_MOREINPUT      = $0000040;   (* reading more input in a command function *)
  RL_STATE_ISEARCH        = $0000080;   (* doing incremental search *)
  RL_STATE_NSEARCH        = $0000100;   (* doing non-inc search *)
  RL_STATE_SEARCH         = $0000200;   (* doing a history search *)
  RL_STATE_NUMERICARG     = $0000400;   (* reading numeric argument *)
  RL_STATE_MACROINPUT     = $0000800;   (* getting input from a macro *)
  RL_STATE_MACRODEF       = $0001000;   (* defining keyboard macro *)
  RL_STATE_OVERWRITE      = $0002000;   (* overwrite mode *)
  RL_STATE_COMPLETING     = $0004000;   (* doing completion *)
  RL_STATE_SIGHANDLER     = $0008000;   (* in readline sighandler *)
  RL_STATE_UNDOING        = $0010000;   (* doing an undo *)
  RL_STATE_INPUTPENDING   = $0020000;   (* rl_execute_next called *)
  RL_STATE_TTYCSAVED      = $0040000;   (* tty special chars saved *)
  RL_STATE_CALLBACK       = $0080000;   (* using the callback interface *)
  RL_STATE_VIMOTION       = $0100000;   (* reading vi motion arg *)
  RL_STATE_MULTIKEY       = $0200000;   (* reading multiple-key command *)
  RL_STATE_VICMDONCE      = $0400000;   (* entered vi command mode at least once *)
  RL_STATE_REDISPLAYING   = $0800000;   (* updating terminal display *)

  RL_STATE_DONE           = $1000000;   (* done; accepted line *)

(* #define RL_SETSTATE(x)        (rl_readline_state |= (x))   *)
(* #define RL_UNSETSTATE(x)      (rl_readline_state &= ~(x))  *)
(* #define RL_ISSTATE(x)         (rl_readline_state & (x))    *)
Procedure RL_SETSTATE(x : cint);
Procedure RL_UNSETSTATE(x : cint);
Function  RL_ISSTATE(x : cint) : Boolean;

Type
  Preadline_state = ^readline_state;
  readline_state = record
    (* line state *)
    point : cint;
    Theend : cint;
    mark : cint;
    buffer : PChar;
    buflen : cint;
    ul : PUNDO_LIST;
    prompt : PChar;

    (* global state *)
    rlstate : cint;
    done : cint;
    kmap : Keymap;
    lastfunc : rl_command_func_t;

    (* input state *)
    insmode : cint;
    edmode : cint;
    kseqlen : cint;
    inf : PCFILE;
    outf : PCFILE;
    pendingin : cint;
    macro : PChar;

    (* signal state *)
    catchsigs : cint;
    catchsigwinch : cint;

    (* search state *)

    (* completion state *)

    (* options state *)

    (* reserved for future expansion, so the struct size doesn't change *)
    reserved : array[0..63] of cchar;
  end;

Function rl_save_state(sp:Preadline_state):cint;cdecl;external;
Function rl_restore_state(sp:Preadline_state):cint;cdecl;external;

Implementation

(* #define RL_SETSTATE(x)        (rl_readline_state |= (x))   *)
(* #define RL_UNSETSTATE(x)      (rl_readline_state &= ~(x))  *)
(* #define RL_ISSTATE(x)         (rl_readline_state & (x))    *)
Procedure RL_SETSTATE(x : cint);
Begin
  rl_readline_state := rl_readline_state or x;
End;
Procedure RL_UNSETSTATE(x : cint);
Begin
  rl_readline_state := rl_readline_state and (not x);
End;
Function RL_ISSTATE(x : cint) : Boolean;
Begin
  Result := ((rl_readline_state and x) <> 0);
End;

End.

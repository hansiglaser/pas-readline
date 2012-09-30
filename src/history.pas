(**
 * Header translation for the GNU History Library
 *
 * http://tiswww.case.edu/php/chet/readline/history.html
 *
 * 2011-04-30 Johann Glaser
 *
 *)
{$MODE ObjFpc}
Unit History;

(* history.h -- the names of functions that you can call in history. *)

(* Copyright (C) 1989-2009 Free Software Foundation, Inc.

   This file contains the GNU History Library (History), a set of
   routines for managing the text of previously typed lines.

   History is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   History is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with History.  If not, see <http://www.gnu.org/licenses/>.
*)

Interface
Uses CTypes,UnixType;

{$macro on}
{$ifdef windows}
  {$define extdecl:=stdcall}
{$else}
  {$define extdecl:=cdecl}
{$endif}

{$LINKLIB readline}

//#if defined READLINE_LIBRARY
//#  include "rlstdc.h"
{$I rltypedefs.inc.pas}
//#else
//#  include <readline/rlstdc.h>
//#  include <readline/rltypedefs.h>
//#endif

type
  Phistdata_t = ^histdata_t;
  histdata_t = PChar;

(* The structure used to store a history entry. *)
  P_hist_entry = ^_hist_entry;
  _hist_entry = record
      line      : PChar;
      timestamp : PChar;  (* char * rather than time_t for read/write *)
      data      : histdata_t;
    end;
  HIST_ENTRY = _hist_entry;
  PHIST_ENTRY = ^HIST_ENTRY;
  APHIST_ENTRY = Array[0..65535] of PHIST_ENTRY;
  PPHIST_ENTRY = ^APHIST_ENTRY;

(* Size of the history-library-managed space in history entry HS. *)
(* #define HISTENT_BYTES(hs)	(strlen ((hs)->line) + strlen ((hs)->timestamp)) *)
function HISTENT_BYTES(hs : PHIST_ENTRY) : longint;

(* A structure used to pass the current state of the history stuff around. *)
Type
  P_hist_state = ^_hist_state;
  _hist_state = record
      entries : ^PHIST_ENTRY;  (* Pointer to the entries themselves. *)
      offset  : cint;          (* The location pointer within this array. *)
      length  : cint;          (* Number of elements within this array. *)
      size    : cint;          (* Number of slots allocated to this array. *)
      flags   : cint;
    end;
  HISTORY_STATE = _hist_state;
  PHISTORY_STATE = ^HISTORY_STATE;

(* Flag values for the `flags' member of HISTORY_STATE. *)
Const
  HS_STIFLED = $01;

(* Initialization and state management. *)

(* Begin a session in which the history functions might be used.  This
   just initializes the interactive variables. *)
procedure using_history;extdecl;external;

(* Return the current HISTORY_STATE of the history. *)
function history_get_history_state:PHISTORY_STATE;extdecl;external;

(* Set the state of the current history array to STATE. *)
procedure history_set_history_state(State:PHISTORY_STATE);extdecl;external;

(* Manage the history list. *)

(* Place STRING at the end of the history list.
   The associated data field (if any) is set to NULL. *)
procedure add_history(TheString:PChar);extdecl;external;

(* Change the timestamp associated with the most recent history entry to
   STRING. *)
procedure add_history_time(TheString:PChar);extdecl;external;

(* A reasonably useless function, only here for completeness.  WHICH
   is the magic number that tells us which element to delete.  The
   elements are numbered from 0. *)
function remove_history(Which:cint):PHIST_ENTRY;extdecl;external;

(* Free the history entry H and return any application-specific data
   associated with it. *)
function free_history_entry(HistEnt:PHIST_ENTRY):histdata_t;extdecl;external;

(* Make the history entry at WHICH have LINE and DATA.  This returns
   the old entry so you can dispose of the data.  In the case of an
   invalid WHICH, a NULL pointer is returned. *)
function replace_history_entry(Which:cint; Line:PChar; Data:histdata_t):PHIST_ENTRY;extdecl;external;

(* Clear the history list and start over. *)
procedure clear_history;extdecl;external;

(* Stifle the history list, remembering only MAX number of entries. *)
procedure stifle_history(Max:cint);extdecl;external;

(* Stop stifling the history.  This returns the previous amount the
   history was stifled by.  The value is positive if the history was
   stifled, negative if it wasn't. *)
function unstifle_history:cint;extdecl;external;

(* Return 1 if the history is stifled, 0 if it is not. *)
function history_is_stifled:cint;extdecl;external;

(* Information about the history list. *)

(* Return a NULL terminated array of HIST_ENTRY which is the current input
   history.  Element 0 of this list is the beginning of time.  If there
   is no history, return NULL. *)
function history_list:PPHIST_ENTRY;extdecl;external;

(* Returns the number which says what history element we are now
   looking at.  *)
function where_history:cint;extdecl;external;

(* Return the history entry at the current position, as determined by
   history_offset.  If there is no entry there, return a NULL pointer. *)
function current_history:PHIST_ENTRY;extdecl;external;

(* Return the history entry which is logically at OFFSET in the history
   array.  OFFSET is relative to history_base. *)
function history_get(Offset:cint):PHIST_ENTRY;extdecl;external;

(* Return the timestamp associated with the HIST_ENTRY * passed as an
   argument *)
function history_get_time(Entry:PHIST_ENTRY):time_t;extdecl;external;

(* Return the number of bytes that the primary history entries are using.
   This just adds up the lengths of the_history->lines. *)
function history_total_bytes:cint;extdecl;external;

(* Moving around the history list. *)

(* Set the position in the history list to POS. *)
function history_set_pos(Pos:cint):cint;extdecl;external;

(* Back up history_offset to the previous history entry, and return
   a pointer to that entry.  If there is no previous entry, return
   a NULL pointer. *)
function previous_history:PHIST_ENTRY;extdecl;external;

(* Move history_offset forward to the next item in the input_history,
   and return the a pointer to that entry.  If there is no next entry,
   return a NULL pointer. *)
function next_history:PHIST_ENTRY;extdecl;external;

(* Searching the history list. *)

(* Search the history for STRING, starting at history_offset.
   If DIRECTION < 0, then the search is through previous entries,
   else through subsequent.  If the string is found, then
   current_history () is the history entry, and the value of this function
   is the offset in the line of that history entry that the string was
   found in.  Otherwise, nothing is changed, and a -1 is returned. *)
function history_search(TheString:PChar; Direction:cint):cint;extdecl;external;

(* Search the history for STRING, starting at history_offset.
   The search is anchored: matching lines must begin with string.
   DIRECTION is as in history_search(). *)
function history_search_prefix(TheString:PChar; Direction:cint):cint;extdecl;external;

(* Search for STRING in the history list, starting at POS, an
   absolute index into the list.  DIR, if negative, says to search
   backwards from POS, else forwards.
   Returns the absolute index of the history element where STRING
   was found, or -1 otherwise. *)
function history_search_pos(TheString:PChar; Direction:cint; Pos:cint):cint;extdecl;external;

(* Managing the history file. *)

(* Add the contents of FILENAME to the history list, a line at a time.
   If FILENAME is NULL, then read from ~/.history.  Returns 0 if
   successful, or errno if not. *)
function read_history(Filename:PChar):cint;extdecl;external;

(* Read a range of lines from FILENAME, adding them to the history list.
   Start reading at the FROM'th line and end at the TO'th.  If FROM
   is zero, start at the beginning.  If TO is less than FROM, read
   until the end of the file.  If FILENAME is NULL, then read from
   ~/.history.  Returns 0 if successful, or errno if not. *)
function read_history_range(Filename:PChar; From:cint; TheTo:cint):cint;extdecl;external;

(* Write the current history to FILENAME.  If FILENAME is NULL,
   then write the history list to ~/.history.  Values returned
   are as in read_history ().  *)
function write_history(Filename:PChar):cint;extdecl;external;

(* Append NELEMENT entries to FILENAME.  The entries appended are from
   the end of the list minus NELEMENTs up to the end of the list. *)
function append_history(NElements:cint; Filename:PChar):cint;extdecl;external;

(* Truncate the history file, leaving only the last NLINES lines. *)
function history_truncate_file(Filename:PChar; NLines:cint):cint;extdecl;external;

(* History expansion. *)

(* Expand the string STRING, placing the result into OUTPUT, a pointer
   to a string.  Returns:

   0) If no expansions took place (or, if the only change in
      the text was the de-slashifying of the history expansion
      character)
   1) If expansions did take place
  -1) If there was an error in expansion.
   2) If the returned line should just be printed.

  If an error ocurred in expansion, then OUTPUT contains a descriptive
  error message. *)
function history_expand(TheString:PChar; Output:PPChar):cint;extdecl;external;

(* Extract a string segment consisting of the FIRST through LAST
   arguments present in STRING.  Arguments are broken up as in
   the shell. *)
function history_arg_extract(First:cint; Last:cint; TheString:PChar):PChar;extdecl;external;

(* Return the text of the history event beginning at the current
   offset into STRING.  Pass STRING with *INDEX equal to the
   history_expansion_char that begins this specification.
   DELIMITING_QUOTE is a character that is allowed to end the string
   specification for what to search for in addition to the normal
   characters `:', ` ', `\t', `\n', and sometimes `?'. *)
function get_history_event(TheString:PChar; CIndex:pcint; QChar:cint):PChar;extdecl;external;

(* Return an array of tokens, much as the shell might.  The tokens are
   parsed out of STRING. *)
function history_tokenize(TheSTring:PChar):PPChar;extdecl;external;

(* Exported history variables. *)
Var
  history_base : cint;cvar;external;
  history_length : cint;cvar;external;
  history_max_entries : cint;cvar;external;
  history_expansion_char : cchar;cvar;external;
  history_subst_char : cchar;cvar;external;
  history_word_delimiters : PChar;cvar;external;
  history_comment_char : cchar;cvar;external;
  history_no_expand_chars : PChar;cvar;external;
  history_search_delimiter_chars : PChar;cvar;external;
  history_quotes_inhibit_expansion : cint;cvar;external;

  history_write_timestamps : cint;cvar;external;

(* Backwards compatibility *)
  max_input_history : cint;cvar;external;

(* If set, this function is called to decide whether or not a particular
   history expansion should be treated as a special case for the calling
   application and not expanded. *)
  history_inhibit_expansion_function : rl_linebuf_func_t;cvar;external;

Implementation

(* was #define dname(params) para_def_expr*)
(* argument types are unknown*)
(* return type might be wrong*)   
Function HISTENT_BYTES(hs : PHIST_ENTRY) : longint;
Begin
  Result := (strlen(hs^.line))+(strlen(hs^.timestamp));
End;

End.

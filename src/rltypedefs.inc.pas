(* rltypedefs.h -- Type declarations for readline functions. *)

(* Copyright (C) 2000-2009 Free Software Foundation, Inc.

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

(* Old-style *)

//#if !defined (_FUNCTION_DEF)
//#  define _FUNCTION_DEF
//
//typedef CInt Function ();
//typedef void VFunction ();
//typedef PChar CPFunction ();
//typedef PPChar CPPFunction ();
//
//#endif (* _FUNCTION_DEF *)

(* New style. *)

//#if !defined (_RL_FUNCTION_TYPEDEF)
//#  define _RL_FUNCTION_TYPEDEF
Type
  PCFILE = Pointer;   // C FILE*

Type
  (* Bindable functions *)
  rl_command_func_t = Function(Count:CInt;Key:CInt) : CInt; extdecl;

  (* Typedefs for the completion system *)
  rl_compentry_func_t   = Function(TheText : PChar; Matches : CInt) : PChar;  extdecl;
  rl_completion_func_t  = Function(TheText : PChar; Start, TheEnd : CInt) : PPChar;  extdecl;

  rl_quote_func_t    = Function(TheText : PChar; Match : CInt; QuoteChar : PChar) : PChar;   extdecl;
  rl_dequote_func_t  = Function(TheText : PChar; Length : CInt) : PChar;   extdecl;

  rl_compignore_func_t  = Function(Completions : PPChar) : CInt;   extdecl;

  rl_compdisp_func_t  = Procedure(Matches:PPChar; NumMatches:CInt; MaxLength:CInt);   extdecl;

  (* Type for input and pre-read hook functions like rl_event_hook *)
  rl_hook_func_t  = Function : CInt;  extdecl;

  (* Input function type *)
  rl_getc_func_t  = Function(F:PCFILE) : CInt;  extdecl;

  (* Generic function that takes a character buffer (which could be the readline
     line buffer) and an index into it (which could be rl_point) and returns
     an int. *)
  rl_linebuf_func_t  = Function(CharBuf:PChar; Index:CInt) : CInt;   extdecl;

  (* `Generic' function pointer typedefs *)
  rl_intfunc_t    = Function(P1:CInt)   : CInt;  extdecl;
  rl_ivoidfunc_t  = rl_hook_func_t;
  rl_icpfunc_t    = Function(P1:PChar)  : CInt;  extdecl;
  rl_icppfunc_t   = Function(P1:PPChar) : CInt;   extdecl;

  rl_voidfunc_t   = Procedure;             extdecl;
  rl_vintfunc_t   = Procedure(P1:CInt);     extdecl;
  rl_vcpfunc_t    = Procedure(P1:PChar);     extdecl;
  rl_vcppfunc_t   = Procedure(P1:PPChar);    extdecl;

  rl_cpvfunc_t   = Function            : PChar;  extdecl;
  rl_cpifunc_t   = Function(P1:CInt)   : PChar;  extdecl;
  rl_cpcpfunc_t  = Function(P1:PChar)  : PChar;  extdecl;
  rl_cpcppfunc_t = Function(P1:PPChar) : PChar;  extdecl;

//#endif (* _RL_FUNCTION_TYPEDEF *)


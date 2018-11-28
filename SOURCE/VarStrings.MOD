(**************************************************************************)
(*                                                                        *)
(*  PMOS library                                                          *)
(*  Copyright (C) 2018   Peter Moylan                                     *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>. *)
(*                                                                        *)
(*  To contact author:   http://www.pmoylan.org   peter@pmoylan.org       *)
(*                                                                        *)
(**************************************************************************)

IMPLEMENTATION MODULE VarStrings;

        (********************************************************)
        (*                                                      *)
        (*                Variable-length strings               *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        2 October 2018                  *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)


FROM STextIO IMPORT
    (* proc *)  WriteChar, WriteLn;

(************************************************************************)

CONST
    Nul = CHR(0);  LF = CHR(10);  CR = CHR(13);

(************************************************************************)

PROCEDURE WriteVarString (VAR (*IN*) str: ARRAY OF CHAR;  N: CARDINAL);

    (* Write character string of length N to standard output.   *)
    (* Convention: we treat LF as the newline character, and    *)
    (* ignore any CR encountered.                               *)

    VAR j: CARDINAL;
        ch: CHAR;

    BEGIN
        j := 0;
        WHILE j < N DO
            ch := str[j];  INC(j);
            IF ch = Nul THEN
                (* Premature termination *)
                j := N;
            ELSIF ch = CR THEN
                (* do nothing *);
            ELSIF ch = LF THEN
                WriteLn;
            ELSE
                WriteChar (ch);
            END (*IF*);
        END (*WHILE*);
    END WriteVarString;

(************************************************************************)

END VarStrings.


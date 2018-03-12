(**************************************************************************)
(*                                                                        *)
(*  Web server                                                            *)
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

IMPLEMENTATION MODULE Environment;

        (********************************************************)
        (*                                                      *)
        (*           Maintaining an environment string          *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            4 March 2018                    *)
        (*  Last edited:        4 March 2018                    *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  Copy;

FROM Names IMPORT
    (* type *)  ArgPointer;

(************************************************************************)

CONST
    Nul = CHR(0);

(************************************************************************)
(*                   UPDATING AN ENVIRONMENT STRING                     *)
(************************************************************************)

PROCEDURE AddToEnvironmentString (VAR (*INOUT*) p: EnvRecordPtr;
                                   parameter, value: ARRAY OF CHAR);

    (* Adds a new parameter=value entry to the environment string. *)

    VAR oldlength, newlength, j, k: CARDINAL;
        q: ArgPointer;
        ch: CHAR;

    BEGIN
        IF p = NIL THEN
            NEW (p);
            p^.content := NIL;
            p^.length := 0;
        END (*IF*);
        IF p^.content = NIL THEN
            oldlength := 0;
        ELSE
            oldlength := p^.length;
        END (*IF*);
        newlength := oldlength + LENGTH(parameter) + LENGTH(value) + 2;
        ALLOCATE (q, newlength + 1);
        IF oldlength > 0 THEN
            Copy (p^.content, q, oldlength);
        END (*IF*);

        (* Note the extra one character to hold the terminating Nul. *)

        k := oldlength;  j := 0;
        LOOP
            ch := parameter[j];  INC(j);
            IF ch = Nul THEN
                EXIT (*LOOP*);
            END (*IF*);
            q^[k] := ch;  INC(k);
        END (*LOOP*);
        q^[k] := '=';  INC(k);
        j := 0;
        LOOP
            ch := value[j];  INC(j);
            IF ch = Nul THEN
                EXIT (*LOOP*);
            END (*IF*);
            q^[k] := ch;  INC(k);
        END (*LOOP*);
        q^[k] := Nul;  INC(k);

        (* Now the final terminator, after which we can *)
        (* deallocate the original string.              *)

        q^[k] := Nul;
        IF p^.content <> NIL THEN
            DEALLOCATE (p^.content, oldlength + 1);
        END (*IF*);
        p^.content := q;
        p^.length := newlength;

    END AddToEnvironmentString;

(************************************************************************)

PROCEDURE DiscardEnvironmentString (VAR (*INOUT*) p: EnvRecordPtr);

    (* Discards the memory claimed by p. *)

    BEGIN
        IF p <> NIL THEN
            IF p^.content <> NIL THEN
                DEALLOCATE (p^.content, p^.length + 1);
            END (*IF*);
            DISPOSE (p);
        END (*IF*);
    END DiscardEnvironmentString;

(************************************************************************)

END Environment.


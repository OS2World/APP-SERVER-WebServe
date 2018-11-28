(**************************************************************************)
(*                                                                        *)
(*  Transport Layer Security                                              *)
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

IMPLEMENTATION MODULE TLSBase;

        (********************************************************)
        (*                                                      *)
        (*                Transport Layer Security              *)
        (*      This module contains definitions needed by      *)
        (*         several levels of the TLS software.          *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            2 April 2018                    *)
        (*  Last edited:        6 October 2018                  *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT CARD8, CARD16, LOC, CAST;

IMPORT Strings;

FROM STextIO IMPORT                 (* for debugging *)
    (* proc *)  WriteChar, WriteString, WriteLn;

FROM VarStrings IMPORT
    (* type *)  ByteStringPtr;

FROM Inet2Misc IMPORT
    (* proc *)  Swap2;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  LogTransaction;

FROM LowLevel IMPORT
    (* proc *)  Copy;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST Nul = CHR(0);

(************************************************************************)
(*                              DEBUGGING STUFF                         *)
(************************************************************************)

PROCEDURE WriteHexChar (N: CARD8);

    (* Write a single hexadecimal digit.  *)

    BEGIN
        IF N > 9 THEN
            WriteChar (CHR(ORD('A') + (N - 10)));
        ELSE
            WriteChar (CHR(ORD('0') + N));
        END (*IF*);
    END WriteHexChar;

(************************************************************************)

PROCEDURE WriteHex (N: CARD8);

    (* Write N in hexadecimal.  *)

    BEGIN
        WriteHexChar (N DIV 16);
        WriteHexChar (N MOD 16);
    END WriteHex;

(************************************************************************)

PROCEDURE WriteHexString (VAR (*IN*) buf: ARRAY OF LOC;  pos, N: CARDINAL;
                                                    space: BOOLEAN);

    (* Write N bytes starting at buf[pos].  *)

    VAR j: CARDINAL;

    BEGIN
        FOR j := 0 TO N-1 DO
            WriteHex (CAST(CARD8, buf[pos]));  INC(pos);
            IF space THEN
                WriteChar (' ');
            END (*IF*);
        END (*FOR*);
    END WriteHexString;

(************************************************************************)

PROCEDURE AppendHexChar (N: CARD8;  VAR (*INOUT*) message: ARRAY OF CHAR;
                                            VAR (*INOUT*) pos: CARDINAL);

    (* Puts one-digit hexadecimal code for N at message[pos], updates pos.  *)

    VAR ch: CHAR;

    BEGIN
        IF N > 9 THEN
            ch := CHR(ORD('A') + (N - 10));
        ELSE
            ch := CHR(ORD('0') + N);
        END (*IF*);
        message[pos] := ch;  INC(pos);
    END AppendHexChar;

(************************************************************************)

PROCEDURE AppendHex (N: CARD8;  VAR (*INOUT*) message: ARRAY OF CHAR;
                                            VAR (*INOUT*) pos: CARDINAL);

    (* Puts two-digit hexadecimal code for N at message[pos], updates pos.  *)

    BEGIN
        AppendHexChar (N DIV 16, message, pos);
        AppendHexChar (N MOD 16, message, pos);
    END AppendHex;

(************************************************************************)

PROCEDURE AppendHexString (VAR (*IN*) buf: ARRAY OF LOC;  pos, N: CARDINAL;
                        space: BOOLEAN;  VAR (*INOUT*) message: ARRAY OF CHAR);

    (* Append N bytes starting at buf[pos] to message.  *)

    VAR j, outpos: CARDINAL;

    BEGIN
        outpos := LENGTH (message);
        FOR j := 0 TO N-1 DO
            AppendHex (CAST(CARD8, buf[pos]), message, outpos);
            INC(pos);
            IF space THEN
                message[outpos] := ' ';  INC (outpos);
            END (*IF*);
        END (*FOR*);
        message[outpos] := Nul;
    END AppendHexString;

(************************************************************************)

PROCEDURE WriteAscString (VAR (*IN*) buf: ARRAY OF CARD8;  pos, N: CARDINAL);

    (* Like WriteHexString, but we interpret the bytes as characters.  *)

    VAR j: CARDINAL;

    BEGIN
        FOR j := 0 TO N-1 DO
            WriteChar (CHR(buf[pos]));  INC(pos);
        END (*FOR*);
    END WriteAscString;

(************************************************************************)

PROCEDURE WriteCard (N: CARDINAL);

    (* Write N in decimal.  *)

    BEGIN
        IF N > 9 THEN
            WriteCard (N DIV 10);
            N := N MOD 10;
        END (*IF*);
        WriteChar (CHR(ORD('0') + N));
    END WriteCard;

(************************************************************************)
(*                              MISC UTILITIES                          *)
(************************************************************************)

PROCEDURE BEeval (N: CARD16): CARD16;

    (* Returns the value of a bigendian number. *)

    BEGIN
        RETURN Swap2 (N);
    END BEeval;

(************************************************************************)

PROCEDURE NYI (logID: TransactionLogID;  name: ARRAY OF CHAR);

    (* Puts out a "not yet implemented" message. *)

    VAR message: ARRAY [0..95] OF CHAR;

    BEGIN
        Strings.Assign (name, message);
        Strings.Append (" is not yet implemented.", message);
        LogTransaction (logID, message);
    END NYI;

(************************************************************************)

PROCEDURE Get3Bytes (VAR (*IN*) A: ARRAY OF CARD8;
                              VAR (*INOUT*) pos: CARDINAL): CARDINAL;

    (* Picks up a three-byte value, in BigEndian format, at A[pos], and *)
    (* updates pos.                                                     *)

    VAR j, val: CARDINAL;

    BEGIN
        val := 0;
        FOR j := 1 TO 3 DO
            val := 256*val + A[pos];
            INC (pos);
        END (*FOR*);
        RETURN val;
    END Get3Bytes;

(************************************************************************)

PROCEDURE Put3Bytes (VAR (*OUT*) A: ARRAY OF CARD8;  val: CARDINAL);

    (* Stores val, in BigEndian format, as a three-byte value at A.    *)

    VAR j: CARDINAL;

    BEGIN
        FOR j := 2 TO 0 BY -1 DO
            A[j] := val MOD 256;
            val := val DIV 256;
        END (*FOR*);
    END Put3Bytes;

(************************************************************************)

PROCEDURE Resize (VAR (*INOUT*) p: ByteStringPtr;
                     VAR (*INOUT*) recsize: CARDINAL;  newsize: CARDINAL);

    (* On entry, p^ has recsize bytes of space allocated.  We       *)
    (* contract or expand the record, if necessary, to size newsize.*)

    VAR q: ByteStringPtr;

    BEGIN
        IF newsize <> recsize THEN
            ALLOCATE (q, newsize);
            IF newsize > recsize THEN
                Copy (p, q, recsize);
            ELSE
                Copy (p, q, newsize);
            END (*IF*);
            DEALLOCATE (p, recsize);
            p := q;
            recsize := newsize;
        END (*IF*);
    END Resize;

(************************************************************************)

PROCEDURE Checksize (VAR (*INOUT*) p: ByteStringPtr;
                     VAR (*INOUT*) recsize: CARDINAL;
                                    length, incr: CARDINAL);

    (* On entry, p^ has recsize bytes of space allocated, and           *)
    (* length bytes actually used.  We expand the record, if necessary, *)
    (* to allow at least an additional incr bytes.  For efficiency we   *)
    (* usually expand by more than was requested; so, after you've      *)
    (* finished building this record, you must call Resize to contract  *)
    (* back to the final size.                                          *)

    CONST DefaultIncrement = 128;

    BEGIN
        IF length+incr > recsize THEN
            IF incr < DefaultIncrement THEN
                incr := DefaultIncrement;
            END (*IF*);
            Resize (p, recsize, length+incr);
        END (*IF*);
    END Checksize;

(************************************************************************)

END TLSBase.


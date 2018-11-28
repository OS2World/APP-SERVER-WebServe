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

IMPLEMENTATION MODULE TLSRecord;

        (********************************************************)
        (*                                                      *)
        (*                Transport Layer Security              *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            2 April 2018                    *)
        (*  Last edited:        12 October 2018                 *)
        (*  Status:             no encryption                   *)
        (*                                                      *)
        (********************************************************)


FROM STextIO IMPORT                 (* for debugging *)
    (* proc *)  WriteChar, WriteString, WriteLn;

FROM TLSBase IMPORT                 (* also for debugging *)
    (* proc *)  NYI, WriteCard, WriteHexString, AppendHexString, BEeval;

FROM SYSTEM IMPORT CARD8, CARD16, ADR, LOC;

IMPORT Strings;

FROM TLSCrypt IMPORT
    (* type *)  EncrRule,
    (* proc *)  InitEncryptionRule, CloseEncryptionRule, Encrypt;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  LogTransaction, LogTransactionL;

FROM Arith64 IMPORT
    (* type *)  CARD64,
    (* const*)  Zero64,
    (* proc *)  INC64;

FROM MiscFuncs IMPORT
    (* proc *)  AppendCard;

FROM Inet2Misc IMPORT
    (* proc *)  Swap2;

FROM NetStream IMPORT
    (* proc *)  PutBytes;

FROM Sockets IMPORT
    (* type *)  Socket,
    (* proc *)  send, recv, soclose;

FROM LowLevel IMPORT
    (* proc *)  Copy, EVAL;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

TYPE
    RLState = POINTER TO
                    RECORD
                        dummy: CARDINAL;
                        sock: Socket;
                        seqnum: CARD64;
                        encrrule: EncrRule;
                    END (*RECORD*);

VAR Trace: BOOLEAN;

(************************************************************************)
(*                             MISC UTILITIES                           *)
(************************************************************************)

PROCEDURE FirstNBytes (logID: TransactionLogID;  prefix: ARRAY OF CHAR;
                                        buffer: TextType;  N: CARDINAL);

    (* Writes out the first N bytes of buffer, including the header. *)

    VAR amount: CARDINAL;
        message: ARRAY [0..255] OF CHAR;

    BEGIN
        Strings.Assign (prefix, message);
        Strings.Append ("first ", message);
        AppendCard (N, message);
        Strings.Append (" bytes of buffer are ", message);
        IF N >= 5 THEN
            amount := 5;
        ELSE
            amount := N;
        END (*IF*);
        AppendHexString (buffer, 0, amount, TRUE, message);

        IF (N > 5) AND (buffer.fragment <> NIL) THEN
            AppendHexString (buffer.fragment^, 0, N-5, TRUE, message);
        END (*IF*);
        LogTransaction (logID, message);

    END FirstNBytes;

(************************************************************************)

PROCEDURE RecordTraceOn;

    (* For testing/debugging.  Logs data sent or received until         *)
    (* RecordTraceOff is called.  This is a module-wide setting rather  *)
    (* that being specific to one thread, so it can be used only for    *)
    (* simple tests.  (And should probably be deleted in the long run.) *)

    BEGIN
        Trace := TRUE;
    END RecordTraceOn;

(************************************************************************)

PROCEDURE RecordTraceOff;

    (* Stops logging of data sent or received.  *)

    BEGIN
        Trace := FALSE;
    END RecordTraceOff;

(************************************************************************)
(*                      OPENING AND CLOSING A SESSION                   *)
(************************************************************************)

PROCEDURE TLSRecordInit (S: Socket): RLState;

    (* Initialises the record for a TLS session. *)

    VAR result: RLState;

    BEGIN
        NEW (result);
        result^.sock := S;
        result^.seqnum := Zero64;
        result^.encrrule := InitEncryptionRule();
        RETURN result;
    END TLSRecordInit;

(************************************************************************)

PROCEDURE TLSRecordClose (VAR (*INOUT*) rlstate: RLState);

    (* Closes this record layer state. *)

    BEGIN
        CloseEncryptionRule (rlstate^.encrrule);
        DISPOSE (rlstate);
    END TLSRecordClose;

(************************************************************************)
(*                          RECORD LAYER - INPUT                        *)
(************************************************************************)

PROCEDURE GetFragment (sess: RLState;  logID: TransactionLogID;
                                VAR (*OUT*) buffer: TextType): BOOLEAN;

    (* Receives one record from the sender, in raw form.  It is up to   *)
    (* the caller to work out what to do with this record.  Note that   *)
    (* the first five bytes are considered to be header detail that is  *)
    (* not copied into buffer.                                          *)

    (* Remark: we store buffer.length in BigEndian format, even though  *)
    (* the caller will almost certainly convert it back to native       *)
    (* format.  This minor inefficiency is accepted for the sake of     *)
    (* ensuring that buffer.length is treated consistently by every     *)
    (* procedure in this module.                                        *)

    CONST todump = 46;

    VAR size, pos, part: CARDINAL;
        message: ARRAY [0..255] OF CHAR;

    BEGIN
        size := recv (sess^.sock, buffer, 5, 0);
        IF size = MAX(CARDINAL) THEN
            LogTransactionL (logID, "recv error in GetFragment.");
            size := 0;
        END (*IF*);
        IF size = 0 THEN
            IF Trace THEN
                LogTransactionL (logID, "No data received");
            END (*IF*);
            buffer.length := 0;
            buffer.fragment := NIL;
        ELSE
            (* At this stage we have received the header in buffer, *)
            (* but not yet the fragment data.                       *)

            IF (buffer.majversion <> 3) OR (buffer.minversion <> 3) THEN
                LogTransactionL (logID, "Version error in GetFragment");
            END (*IF*);

            size := Swap2 (buffer.length);
            IF Trace THEN
                Strings.Assign ("Protocol version ", message);
                AppendCard (buffer.majversion, message);
                Strings.Append ('.', message);
                AppendCard (buffer.minversion, message);
                LogTransaction (logID, message);
                Strings.Assign ("Fragment length is ", message);
                AppendCard (size, message);
                LogTransaction (logID, message);
            END (*IF*);
            ALLOCATE (buffer.fragment, size);
            pos := 0;
            REPEAT
                part := recv (sess^.sock, buffer.fragment^[pos], size, 0);
                IF part = MAX(CARDINAL) THEN
                    pos := MAX(CARDINAL);
                ELSE
                    INC (pos, part);
                END (*IF*);
            UNTIL pos >= size;
            IF size = MAX(CARDINAL) THEN
                LogTransactionL (logID, "recv error in GetFragment.");
                buffer.length := 0;
                DEALLOCATE (buffer.fragment, size);
            END (*IF*);
            IF Trace THEN
                Strings.Assign ("Received fragment of size ", message);
                AppendCard (size, message);
                LogTransaction (logID, message);
                IF size < todump THEN
                    part := size + 5;
                ELSE
                    part := todump + 5;
                END (*IF*);
                FirstNBytes (logID, "In GetFragment, ", buffer, part);
            END (*IF*);
            RETURN TRUE;
        END (*IF*);
        RETURN FALSE;
    END GetFragment;

(************************************************************************)

PROCEDURE DiscardFragment (VAR (*INOUT*) buffer: TextType);

    (* Releases memory held by this record. *)

    BEGIN
        IF buffer.length <> 0 THEN
            DEALLOCATE (buffer.fragment, Swap2(buffer.length));
            buffer.length := 0;
            buffer.fragment := NIL;
        END (*IF*);
    END DiscardFragment;

(************************************************************************)
(*                         RECORD LAYER - OUTPUT                        *)
(************************************************************************)

(*
PROCEDURE Encapsulate (sess: RLState;  VAR (*INOUT*) buffer: TextType;  type: CARD8);

    (* Converts buffer to a record layer buffer.    *)

    VAR p: FragmentType;
        oldlength, newlength: CARDINAL;

    BEGIN
        oldlength := Swap2(buffer.length);
        newlength := oldlength + 5;
        ALLOCATE (p, newlength);
        Copy (ADR(buffer), p, 5);
        IF buffer.length > 0 THEN
            Copy (buffer.fragment, ADR(p^[5]), oldlength);
            DEALLOCATE (buffer.fragment, oldlength);
        END (*IF*);
        buffer.contenttype := type;
        buffer.length := Swap2(newlength);
        buffer.fragment := p;
    END Encapsulate;
*)

(************************************************************************)

PROCEDURE PutFragment (sess: RLState;  logID: TransactionLogID;
                            VAR (*IN*) buffer: TextType;  type: CARD8);

    (* Sends buffer to the recipient.  This includes any necessary      *)
    (* encryption and compression.                                      *)

    CONST
        todump = 32 - 5;

    VAR size, part: CARDINAL;
        message: ARRAY [0..255] OF CHAR;

    BEGIN
        (*Encapsulate (sess, buffer, type);*)

        IF (buffer.majversion <> 3) OR (buffer.minversion <> 3) THEN
            LogTransactionL (logID, "Version error in PutFragment");
        END (*IF*);

        (* First compress the supplied data.  The current version       *)
        (* supports only "no compression", so we do nothing here.       *)

        (* Should probably compute the MAC at this stage, rather than   *)
        (* do MAC operations as part of the Encrypt procedure.          *)

        (* Now encrypt the data. *)

        buffer.length := Encrypt (sess^.encrrule, logID,
                                        buffer.fragment, buffer.length);

        (* Send the five record header bytes.  *)

        send (sess^.sock, buffer, 5, 0);

        (* Finally, send the encrypted data. *)

        size := Swap2 (buffer.length);
        IF Trace THEN
            Strings.Assign ("Sending fragment of size ", message);
            AppendCard (size, message);
            LogTransaction (logID, message);
            IF size < todump THEN
                part := size + 5;
            ELSE
                part := todump + 5;
            END (*IF*);
            FirstNBytes (logID, "In PutFragment, ", buffer, part);
        END (*IF**);

        IF buffer.fragment <> NIL THEN
            send (sess^.sock, buffer.fragment^, size, 0);
        END (*IF*);

        INC64 (sess^.seqnum);

    END PutFragment;

(************************************************************************)

BEGIN
    Trace := FALSE;
END TLSRecord.


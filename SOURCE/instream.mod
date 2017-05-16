(**************************************************************************)
(*                                                                        *)
(*  Web server input stream handler                                       *)
(*  Copyright (C) 2016   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE InStream;

        (********************************************************)
        (*                                                      *)
        (*          Handler for input from a socket             *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            1 March 2015                    *)
        (*  Last edited:        28 August 2016                  *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM Sockets IMPORT
    (* type *)  Socket,
    (* proc *)  recv;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    BufferSize = 256;
    Nul = CHR(0);  CR = CHR(13);  LF = CHR(10);

TYPE
    IStream = POINTER TO RECORD
                             sock: Socket;
                             bufpos, buflength: CARDINAL;
                             buffer: ARRAY [0..BufferSize-1] OF CHAR;
                         END (*RECORD*);

(************************************************************************)

PROCEDURE OpenIStream (S: Socket): IStream;

    (* Opens a new IStream object that gets incoming socket data. *)

    VAR result: IStream;

    BEGIN
        NEW (result);
        WITH result^ DO
            sock := S;
            bufpos := 0;
            buflength := 0;
        END (*WITH*);
        RETURN result;
    END OpenIStream;

(************************************************************************)

PROCEDURE CloseIStream (VAR (*INOUT*) stream: IStream);

    (* Deletes the stream. *)

    BEGIN
        IF stream <> NIL THEN
            DISPOSE (stream);
        END (*IF*);
    END CloseIStream;

(************************************************************************)

PROCEDURE GetLine (stream: IStream;  VAR (*OUT*) line: ARRAY OF CHAR): BOOLEAN;

    (* Gets one line from the specified stream.  Returns a FALSE        *)
    (* result, with no data, at end of stream.                          *)

    VAR length: CARDINAL;

    (********************************************************************)

    PROCEDURE StoreChar (ch: CHAR);

        (* Appends ch to line, except where this would cause overflow.  *)

        BEGIN
            IF length < HIGH(line) THEN
                line[length] := ch;  INC(length);
            END (*IF*);
        END StoreChar;

    (********************************************************************)

    VAR ch: CHAR;  FoundCR: BOOLEAN;

    BEGIN
        length := 0;  FoundCR := FALSE;

        LOOP
            WITH stream^ DO
                IF bufpos >= buflength THEN
                    buflength := recv (sock, buffer, BufferSize, 0);
                    IF (buflength = MAX(CARDINAL)) OR (buflength = 0) THEN
                        RETURN FALSE;
                    END (*IF*);
                    bufpos := 0;
                END (*IF*);
                ch := buffer[bufpos];  INC(bufpos);
            END (*WITH*);

            (* Our main job here is to find the line terminator.  *)

            IF FoundCR THEN
                IF ch = CR THEN
                    StoreChar(CR);
                ELSE
                    FoundCR := FALSE;
                    IF ch = LF THEN
                        StoreChar (Nul);
                        RETURN TRUE;
                    ELSE
                        StoreChar(ch);
                    END (*IF*);
                END (*IF*);
            ELSIF ch = CR THEN
                FoundCR := TRUE;
            ELSE
                IF ch = LF THEN
                    StoreChar (Nul);
                    RETURN TRUE;
                ELSE
                    StoreChar (ch);
                END (*IF*);
            END (*IF*);

        END (*LOOP*);

    END GetLine;

(************************************************************************)

END InStream.


(**************************************************************************)
(*                                                                        *)
(*  Decoding an ASN.1 coded file                                          *)
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

IMPLEMENTATION MODULE ASN1;

        (********************************************************)
        (*                                                      *)
        (*   Extracting information from an ASN.1 coded file    *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            29 September 2018               *)
        (*  Last edited:        15 October 2018                 *)
        (*  Status:             Working so far                  *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT CARD8, ADR;

IMPORT BigNum;

FROM BigNum IMPORT BN;

FROM VarStrings IMPORT
    (* type *)  VarStringPtr, ByteStringPtr;

FROM LowLevel IMPORT
    (* proc *)  IAND, Copy, EVAL;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);
    LF = CHR(10);

TYPE
    (* A record type to keep track of a variable-sized string.  The     *)
    (* size field gives actual string length, and allocated shows how   *)
    (* much space has been allocated.                                   *)

    LongString = RECORD
                    size, allocated: CARDINAL;
                    pval: VarStringPtr;
                 END (*RECORD*);

(************************************************************************)
(*                   OPERATIONS ON TYPE LongString                      *)
(************************************************************************)

PROCEDURE Initialise (VAR (*INOUT*) str: LongString);

    (* Sets str to an empty string. *)

    BEGIN
        str.size := 0;
        str.allocated := 0;
        str.pval := NIL;
    END Initialise;

(************************************************************************)

PROCEDURE Resize (VAR (*INOUT*) str: LongString;  newsize: CARDINAL);

    (* We contract or expand the allocated space, if necessary, *)
    (* to size newsize.                                         *)

    VAR q: VarStringPtr;

    BEGIN
        IF newsize <> str.allocated THEN
            ALLOCATE (q, newsize);
            IF str.size > 0 THEN
                IF newsize > str.size THEN
                    Copy (str.pval, q, str.size);
                ELSE
                    Copy (str.pval, q, newsize);
                END (*IF*);
            END (*IF*);
            IF str.allocated > 0 THEN
                DEALLOCATE (str.pval, str.allocated);
            END (*IF*);
            str.pval := q;
            str.allocated := newsize;
        END (*IF*);
    END Resize;

(************************************************************************)

PROCEDURE Checksize (VAR (*INOUT*) str: LongString;  incr: CARDINAL);

    (* We expand str, if necessary, to allow at least an additional     *)
    (* incr bytes.  For efficiency we usually expand by more than was   *)
    (* requested; so, after you've finished building str, you must call *)
    (* Finalise to contract back to the final size.                     *)

    CONST DefaultIncrement = 256;

    BEGIN
        IF str.size + incr > str.allocated THEN
            IF incr < DefaultIncrement THEN
                incr := DefaultIncrement;
            END (*IF*);
            Resize (str, str.size + incr);
        END (*IF*);
    END Checksize;

(************************************************************************)

PROCEDURE Finalise (VAR (*INOUT*) str: LongString);

    (* Removes any unused but allocated memory. *)

    BEGIN
        Resize (str, str.size+1);
        str.pval^[str.size] := Nul;
    END Finalise;

(************************************************************************)

PROCEDURE AddChar (VAR (*INOUT*) str: LongString;  ch: CHAR);

    (* Appends ch to str. *)

    BEGIN
        Checksize (str, 1);
        str.pval^[str.size] := ch;
        INC (str.size);
    END AddChar;

(************************************************************************)

PROCEDURE AddString (VAR (*INOUT*) str: LongString;  toadd: ARRAY OF CHAR);

    (* Appends toadd to str. *)

    VAR j, N: CARDINAL;

    BEGIN
        N := LENGTH(toadd);
        IF N > 0 THEN
            Checksize (str, N);
            FOR j := 0 TO N-1 DO
                AddChar (str, toadd[j]);
            END (*FOR*);
        END (*IF*);
    END AddString;

(************************************************************************)
(*                     MISCELLANOUS USEFUL PROCEDURES                   *)
(************************************************************************)

PROCEDURE Spaces (VAR (*INOUT*) str: LongString;  amount: CARDINAL);

    (* Appends amount space characters. *)

    VAR j: CARDINAL;

    BEGIN
        FOR j := 1 TO amount DO
            AddChar (str, ' ');
        END (*FOR*);
    END Spaces;

(********************************************************************************)

PROCEDURE AddHexChar (VAR (*INOUT*) str: LongString;  N: CARD8);

    (* Appends a single hexadecimal digit to str.  *)

    BEGIN
        IF N > 9 THEN
            AddChar (str, CHR(ORD('A') + (N - 10)));
        ELSE
            AddChar (str, CHR(ORD('0') + N));
        END (*IF*);
    END AddHexChar;

(************************************************************************)

PROCEDURE AddHex (VAR (*INOUT*) str: LongString;  N: CARD8);

    (* Appends N in hexadecimal.  *)

    BEGIN
        AddHexChar (str, N DIV 16);
        AddHexChar (str, N MOD 16);
    END AddHex;

(************************************************************************)

PROCEDURE AddHexString (VAR (*INOUT*) str: LongString;
                        VAR (*IN*) buf: ARRAY OF CARD8;  pos, N: CARDINAL;
                                         initialindent, indent: CARDINAL);

    (* Append N bytes from buf[pos].  *)

    VAR j, canfit: CARDINAL;

    BEGIN
        canfit := (80 - initialindent) DIV 3;
        IF N <= canfit THEN
            FOR j := 0 TO N-1 DO
                AddHex (str, buf[pos]);  INC(pos);
                AddChar (str, ' ');
            END (*FOR*);
        ELSE
            AddChar (str, LF);
            canfit := (80 - indent) DIV 3;
            WHILE N > 0 DO
                Spaces (str, indent);
                j := 0;
                REPEAT
                    AddHex (str, buf[pos]);  INC(pos);
                    AddChar (str, ' ');
                    DEC (N);  INC (j);
                UNTIL (N = 0) OR (j = canfit);
                IF N > 0 THEN
                    AddChar (str, LF);
                END (*IF*);
            END (*WHILE*);
        END (*IF*);
    END AddHexString;

(************************************************************************)

PROCEDURE AddAscString (VAR (*INOUT*) str: LongString;
                        VAR (*IN*) buf: ARRAY OF CARD8;  pos, N: CARDINAL);

    (* Like AddHexString, but we interpret the bytes as characters.  *)

    VAR j: CARDINAL;

    BEGIN
        FOR j := 0 TO N-1 DO
            AddChar (str, CHR(buf[pos]));  INC(pos);
        END (*FOR*);
    END AddAscString;

(************************************************************************)

PROCEDURE AddCharString (VAR (*INOUT*) str: LongString;
                                VAR (*IN*) buf: ARRAY OF CARD8;
                                                    pos, N: CARDINAL);

    (* Append character string, N bytes starting from buffer[pos].      *)
    (*    option = 1        ASCII                                       *)
    (*    option = 2        UTF8                                        *)

    BEGIN
        (* For now I'm ignoring the option. *)

        IF N > 0 THEN
            AddAscString (str, buf, pos, N);
        END (*IF*);

    END AddCharString;

(************************************************************************)

PROCEDURE AddCard (VAR (*INOUT*) str: LongString;  N: CARDINAL);

    (* Append N in decimal.  *)

    BEGIN
        IF N > 9 THEN
            AddCard (str, N DIV 10);
            N := N MOD 10;
        END (*IF*);
        AddChar (str, CHR(ORD('0') + N));
    END AddCard;

(************************************************************************)

PROCEDURE AddBignum (VAR (*INOUT*) str: LongString;  N: BN);

    (* Append N in decimal.  *)

    VAR buffer: ARRAY [0..2047] OF CHAR;
        pos: CARDINAL;

    BEGIN
        pos := 0;
        BigNum.ToDecimal (N, buffer, pos);
        AddString (str, buffer);
    END AddBignum;

(************************************************************************)

PROCEDURE UpdateNum (VAR (*INOUT*) A: BN;  base, nextdigit: CARDINAL);

    (* A := base*A + nextdigit *)

    VAR B, C, D, E: BN;

    BEGIN
        B := BigNum.MakeBignum(base);
        C := BigNum.Prod (A, B);
        D := BigNum.MakeBignum (nextdigit);
        E := BigNum.Sum (C, D);
        BigNum.Discard (A);
        BigNum.Discard (B);
        BigNum.Discard (C);
        BigNum.Discard (D);
        A := E;
    END UpdateNum;

(************************************************************************)
(*                TRANSLATE BINARY ASN.1 TO HUMAN-READABLE              *)
(************************************************************************)

PROCEDURE ParseASN1 (VAR (*IN*) buffer: ARRAY OF CARD8;
                        VAR (*INOUT*) pos: CARDINAL;  N, indent: CARDINAL;
                                  VAR (*INOUT*) str: LongString); FORWARD;

    (* Translates ASN.1 code starting at buffer[pos], total N bytes.    *)
    (* We update pos.                                                   *)

(************************************************************************)

PROCEDURE AddInt (VAR (*INOUT*) str: LongString;  val: BN;
                                            length, indent: CARDINAL);

    (* Appends a decimal integer value to str. *)

    (* I have not yet pretty-formatted the result in the case of line   *)
    (* overflow.  It might turn out that the input parameter length     *)
    (* is irrelevant.                                                   *)

    VAR amount, pos: CARDINAL;
        p: VarStringPtr;

    BEGIN
        amount := BigNum.Digits(val) + 4;       (* safety margin! *)
        Checksize (str, amount);
        ALLOCATE (p, amount);
        pos := 0;
        BigNum.ToDecimal (val, p^, pos);
        IF length <= 16 THEN
            Copy (p, ADR(str.pval^[str.size]), pos);
            INC (str.size, pos);
        ELSE
            Copy (p, ADR(str.pval^[str.size]), pos);
            INC (str.size, pos);
        END (*IF*);
        DEALLOCATE (p, amount);
    END AddInt;

(************************************************************************)

PROCEDURE ParseSEQ (VAR (*IN*) buffer: ARRAY OF CARD8;
                        VAR (*INOUT*) pos: CARDINAL;  N, indent: CARDINAL;
                        VAR (*INOUT*) str: LongString);

    (* Translates a sequence.    *)

    VAR nextpos: CARDINAL;

    BEGIN
        nextpos := pos + N;
        Spaces (str, indent);  AddChar (str, '{');  AddChar (str, LF);
        WHILE pos < nextpos DO
            ParseASN1 (buffer, pos, N, indent, str);
        END (*WHILE*);
        Spaces (str, indent);  AddChar (str, '}');  AddChar (str, LF);
    END ParseSEQ;

(************************************************************************)

PROCEDURE ParseSET (VAR (*IN*) buffer: ARRAY OF CARD8;
                        VAR (*INOUT*) pos: CARDINAL;  N, indent: CARDINAL;
                        VAR (*INOUT*) str: LongString);

    (* Translates a set.    *)

    VAR nextpos: CARDINAL;

    BEGIN
        nextpos := pos + N;
        Spaces (str, indent);  AddChar (str, '{');  AddChar (str, LF);
        WHILE pos < nextpos DO
            ParseASN1 (buffer, pos, N, indent, str);
        END (*WHILE*);
        Spaces (str, indent);  AddChar (str, '}');  AddChar (str, LF);
    END ParseSET;

(********************************************************************)

PROCEDURE Fetch (VAR (*IN*) buffer: ARRAY OF CARD8;
                               VAR (*INOUT*) pos: CARDINAL): CARDINAL;

    (* Gets next byte from buffer, updates pos. *)

    VAR val: CARD8;

    BEGIN
        val := buffer[pos];  INC(pos);
        RETURN val;
    END Fetch;

(************************************************************************)

PROCEDURE GetInt (VAR (*IN*) buffer: ARRAY OF CARD8;
                         VAR (*INOUT*) pos: CARDINAL;  length: BN): BN;

    (* Picks up an integer of length bytes.  Remark: I should allow *)
    (* for negative numbers in twos complement notation, but so far *)
    (* I haven't done this.                                         *)

    VAR result: BN;

    (********************************************************************)

    PROCEDURE AppendNextByte (VAR (*INOUT*) A: BN);

        (* A := 256*A + Fetch() *)

        BEGIN
            UpdateNum (A, 256, Fetch(buffer, pos));
        END AppendNextByte;

    (********************************************************************)

    BEGIN
        result := BigNum.Zero();
        WHILE BigNum.Sign(length) > 0 DO
            AppendNextByte (result);
            BigNum.Decr(length);
        END (*WHILE*);
        RETURN result;
    END GetInt;

(************************************************************************)

PROCEDURE TypeAndLength (VAR (*IN*) buffer: ARRAY OF CARD8;
                                        VAR (*INOUT*) pos: CARDINAL;
                         VAR (*OUT*) type, class: CARDINAL;
                         VAR (*OUT*) constructed, varlength: BOOLEAN;
                         VAR (*OUT*) length: BN): CARDINAL;

    (* Fetches the initial type/length values at buffer[pos], updates   *)
    (* pos.  This is the initial two bytes in the simplest case, but    *)
    (* more in some cases.  The type value is broken down into type,    *)
    (* class, and constructed for the convenience of the caller.   In   *)
    (* the special case varlength=TRUE the value of length should be    *)
    (* ignored, and the caller must look for two successive zero bytes  *)
    (* as the value terminator.                                         *)

    (* The function result is the number of bytes consumed. *)

    (********************************************************************)

    PROCEDURE AppendNextByte (VAR (*INOUT*) A: BN);

        (* A := 256*A + Fetch() *)

        BEGIN
            UpdateNum (A, 256, Fetch(buffer, pos));
        END AppendNextByte;

    (********************************************************************)

    VAR pos0, j, shortlength: CARDINAL;
        temp: CARD8;
        last: BOOLEAN;

    BEGIN
        pos0 := pos;

        (* First byte has type information.  In ASN.1 notation this is  *)
        (* called a tag.  Universal tags have numbers in the range 1 to *)
        (* 30 and class=0.  Other values are application-dependent.     *)

        type := Fetch(buffer, pos);
        class := type DIV 64;
        constructed := IAND (type, 32) <> 0;
        type := IAND (type, 31);
        IF type = 31 THEN
            type := 0;
            REPEAT
                temp := Fetch(buffer, pos);
                last := IAND(temp, 128) = 0;
                type := 128*type + IAND(temp,127);
            UNTIL last;
        END (*IF*);

        (* Second byte has length, or code to indicate long length.     *)
        (* The special case where varlength = TRUE means indefinite     *)
        (* length, in which case the value is terminated by two         *)
        (* consecutive zero bytes.                                      *)

        shortlength := Fetch(buffer, pos);
        varlength := shortlength = 128;
        length := BigNum.Zero();
        IF shortlength < 128 THEN
            length := BigNum.MakeBignum (shortlength);
        ELSIF NOT varlength THEN
            (* Long form for length. *)
            temp := shortlength - 128;
            FOR j := 0 TO temp-1 DO
                AppendNextByte (length);
            END (*FOR*);
        END (*IF*);

        RETURN pos-pos0;

    END TypeAndLength;

(************************************************************************)

PROCEDURE ParseASN1 (VAR (*IN*) buffer: ARRAY OF CARD8;
                        VAR (*INOUT*) pos: CARDINAL;  N, indent: CARDINAL;
                        VAR (*INOUT*) str: LongString);

    (* Translates ASN.1 code starting at buffer[pos], total N bytes.    *)
    (* We update pos.                                                   *)

    (********************************************************************)

    PROCEDURE AppendNextByte (VAR (*INOUT*) A: BN);

        (* A := 256*A + Fetch() *)

        BEGIN
            UpdateNum (A, 256, Fetch(buffer, pos));
        END AppendNextByte;

    (********************************************************************)

    PROCEDURE GetInt (length: BN): BN;

        (* Picks up an integer of length bytes.  Remark: I should allow *)
        (* for negative numbers in twos complement notation, but so far *)
        (* I haven't done this.                                         *)

        VAR result: BN;

        BEGIN
            result := BigNum.Zero();
            WHILE BigNum.Sign(length) > 0 DO
                AppendNextByte (result);
                BigNum.Decr(length);
            END (*WHILE*);
            RETURN result;
        END GetInt;

    (********************************************************************)

    PROCEDURE Skip (length: CARDINAL;  variable: BOOLEAN);

        (* Jumps over a value we don't want to interpret.  This should  *)
        (* be relevant only for types we don't yet recognise.           *)

        VAR val: CARD8;

        BEGIN
            IF variable THEN
                LOOP
                    val := buffer[pos];  INC (pos);  DEC (N);
                    IF val = 0 THEN
                        val := buffer[pos];  INC (pos);  DEC (N);
                        IF val = 0 THEN EXIT (*LOOP*) END(*IF*);
                    END (*IF*);
                END (*LOOP*);
            ELSE
                INC (pos, length);  DEC (N, length);
            END (*IF*);
        END Skip;

    (********************************************************************)

    VAR type, class, shortlength, j, indentp3: CARDINAL;
        val, length: BN;
        temp: CARD8;
        constructed, varlength, ShowDetails: BOOLEAN;

    BEGIN
        indentp3 := indent + 3;

        DEC (N, TypeAndLength (buffer, pos, type, class,
                                        constructed, varlength, length));

        Spaces (str, indent);
        IF class = 0 THEN
            CASE type OF
                    1..6, 12, 16..17, 19, 22..23:
                            ShowDetails := FALSE;
                    ELSE    ShowDetails := TRUE;
            END (*IF*);
        ELSE
            ShowDetails := FALSE;
        END (*IF*);

        IF ShowDetails THEN
            (* Say what we have so far. *)

            AddString (str, "class=");  AddCard (str, class);
            AddString (str, ", type=");  AddCard (str, type);
            IF constructed THEN AddString (str, " constructed") END(*IF*);
            AddString (str, ", length=");
            IF varlength THEN
                AddString (str, "indefinite");
            ELSE
                AddBignum (str, length);
            END (*IF*);
            AddChar (str, ' ');
        END (*IF*);

        shortlength := BigNum.Card(length);
        IF class = 1 THEN
            AddString (str, "AppType ");  AddCard (str, type);
            AddString (str, ":  ");
            AddHexString (str, buffer, pos, shortlength, indent+12, indentp3);
            INC (pos, shortlength);  DEC (N, shortlength);  AddChar (str, LF);
        ELSIF class = 2 THEN
            AddChar (str, '[');  AddCard (str, type);
            AddString (str, "] {");  AddChar (str, LF);
            ParseASN1 (buffer, pos, shortlength, indentp3, str);
            Spaces (str, indentp3);  AddChar (str, '}');  AddChar (str, LF);
        ELSIF class = 3 THEN
            AddString (str, "PrivateType ");  AddCard (str, type);
            AddString (str, ":  ");
            AddHexString (str, buffer, pos, shortlength, indent+16, indentp3);
            INC (pos, shortlength);  DEC (N, shortlength);  AddChar (str, LF);
        ELSE            (* class 0 is Universal, the most common class *)

            (* Note that most numeric types are expressed in hexadecimal,   *)
            (* but we do choose to convert INTEGER to decimal.              *)

            CASE type OF
                |   1:  (* BOOLEAN *)
                        AddString (str, "BOOLEAN ");
                        IF Fetch(buffer, pos) = 0 THEN
                            AddString (str, "FALSE");
                        ELSE
                            AddString (str, "TRUE");
                        END (*IF*);
                        AddChar (str, LF);

                |   2:  (* INTEGER *)
                        AddString (str, "INTEGER ");
                        val := GetInt(length);  AddInt (str, val, shortlength, indent);
                        AddChar (str, LF);
                        BigNum.Discard (val);

                |   3:  (* BIT STRING *)
                        (* N.B. constructed encoding has to be handled              *)
                        (* differently, but this should not occur in DER encoding.  *)
                        AddString (str, "BIT STRING ");  AddCard (str, Fetch(buffer, pos));
                        DEC (shortlength);  AddString (str, " unused bits");
                        AddHexString (str, buffer, pos, shortlength, indent+12, indentp3);
                        Skip (shortlength, FALSE);  AddChar (str, LF);

                |   4:  (* OCTET STRING *)
                        (* N.B. constructed encoding has to be handled              *)
                        (* differently, but this should not occur in DER encoding.  *)
                        AddString (str, "OCTET STRING ");
                        AddHexString (str, buffer, pos, shortlength, indent+13, indentp3);
                        Skip (shortlength, FALSE);  AddChar (str, LF);

                |   5:  (* NULL *)
                        AddString (str, "NULL");  AddChar (str, LF);
                        (* Zero content, zero length. *)

                |   6:  (* OBJECT IDENTIFIER *)
                        AddString (str, "OBJECT IDENTIFIER {");
                        temp := Fetch(buffer, pos);  AddCard(str, temp DIV 40);  AddChar (str, ' ');
                        AddCard (str, temp MOD 40);
                        DEC (shortlength);
                        WHILE shortlength > 0 DO
                            val := BigNum.Zero();
                            LOOP
                                j := Fetch(buffer, pos);
                                DEC(shortlength);
                                UpdateNum (val, 128, IAND (j, 7FH));
                                IF IAND (j, 80H) = 0 THEN
                                    EXIT (*LOOP*);
                                END (*IF*);
                            END (*LOOP*);
                            AddChar (str, ' ');  AddBignum (str, val);
                        END (*IF*);
                        AddChar (str, '}');  AddChar (str, LF);

                |   7:  (* ObjectDescriptor, not yet handled *)
                        AddString (str, "ObjectDescriptor, val=");
                        AddString (str, "not yet implemented");  AddChar (str, LF);
                        Skip (shortlength, varlength);

                |   8:  (* INSTANCE OF, EXTERNAL, not yet handled *)
                        AddString (str, "INSTANCE OF, EXTERNAL, val=");
                        AddString (str, "not yet implemented");  AddChar (str, LF);
                        Skip (shortlength, varlength);

                |   9:  (* REAL, not yet handled *)
                        AddString (str, "REAL, val=");
                        AddString (str, "not yet implemented");  AddChar (str, LF);
                        Skip (shortlength, varlength);

                |  10:  (* ENUMERATED, not yet handled *)
                        AddString (str, "ENUMERATED, val=");
                        AddString (str, "not yet implemented");  AddChar (str, LF);
                        Skip (shortlength, varlength);

                |  11:  (* EMBEDDED PDV, not yet handled *)
                        AddString (str, "EMBEDDED PDV, val=");
                        AddString (str, "not yet implemented");  AddChar (str, LF);
                        Skip (shortlength, varlength);

                |  12:  (* UTF8String *)
                        AddString (str, 'UTF8String "');
                        AddCharString (str, buffer, pos, 2);
                        AddChar (str, '"');  AddChar (str, LF);
                        Skip (shortlength, FALSE);

                |  13:  (* RELATIVE-OID, not yet handled *)
                        AddString (str, "RELATIVE-OID, val=");
                        AddString (str, "not yet implemented");  AddChar (str, LF);
                        Skip (shortlength, varlength);

                |  16:  AddString (str, "SEQUENCE");  AddChar (str, LF);
                        ParseSEQ (buffer, pos, shortlength, indentp3, str);

                |  17:  AddString (str, "SET");  AddChar (str, LF);
                        ParseSET (buffer, pos, shortlength, indentp3, str);

                |  18:  (* NumericString, not yet handled *)
                        AddString (str, "NumericString, val=");
                        AddString (str, "not yet implemented");  AddChar (str, LF);
                        Skip (shortlength, varlength);

                |  19:  (* PrintableString *)
                        (* N.B. constructed encoding has to be handled              *)
                        (* differently, but this should not occur in DER encoding.  *)
                        AddString (str, 'PrintableString "');
                        AddCharString (str, buffer, pos, 1);
                        AddChar (str, '"');  AddChar (str, LF);
                        Skip (shortlength, FALSE);

                |  20:  (* TeletexString, not yet handled *)
                        AddString (str, "TeletexString, val=");
                        AddString (str, "not yet implemented");  AddChar (str, LF);
                        Skip (shortlength, varlength);

                |  21:  (* VideotexString, not yet handled *)
                        AddString (str, "VideotexString, val=");
                        AddString (str, "not yet implemented");  AddChar (str, LF);
                        Skip (shortlength, varlength);

                |  22:  (* IA5String *)
                        (* N.B. constructed encoding has to be handled              *)
                        (* differently, but this should not occur in DER encoding.  *)
                        AddString (str, 'IA5String "');
                        AddCharString (str, buffer, pos, 1);
                        AddChar (str, '"');  AddChar (str, LF);
                        Skip (shortlength, FALSE);

                |  23:  (* UTCTime *)
                        (* N.B. constructed encoding has to be handled              *)
                        (* differently, but this should not occur in DER encoding.  *)
                        AddString (str, 'UTCTime ');
                        AddCharString (str, buffer, pos, 1);
                        AddChar (str, LF);
                        Skip (shortlength, FALSE);

                |  24:  (* GeneralizedTime, not yet handled *)
                        AddString (str, "GeneralizedTime, val=");
                        AddString (str, "not yet implemented");  AddChar (str, LF);
                        Skip (shortlength, varlength);

                |  25:  (* GraphicString, not yet handled *)
                        AddString (str, "GraphicString, val=");
                        AddString (str, "not yet implemented");  AddChar (str, LF);
                        Skip (shortlength, varlength);

                |  26:  (* VisibleString, not yet handled *)
                        AddString (str, "VisibleString, val=");
                        AddString (str, "not yet implemented");  AddChar (str, LF);
                        Skip (shortlength, varlength);

                |  27:  (* GeneralString, not yet handled *)
                        AddString (str, "GeneralString, val=");
                        AddString (str, "not yet implemented");  AddChar (str, LF);
                        Skip (shortlength, varlength);

                |  28:  (* UniversalString, not yet handled *)
                        AddString (str, "UniversalString, val=");
                        AddString (str, "not yet implemented");  AddChar (str, LF);
                        Skip (shortlength, varlength);

                |  29:  (* CHARACTER STRING, not yet handled *)
                        AddString (str, "CHARACTER STRING, val=");
                        AddString (str, "not yet implemented");  AddChar (str, LF);
                        Skip (shortlength, varlength);

                |  30:  (* BMPString, not yet handled *)
                        AddString (str, "BMPString, val=");
                        AddString (str, "not yet implemented");  AddChar (str, LF);
                        Skip (shortlength, varlength);
                ELSE
                        AddChar (str, LF);  AddString (str, "UNKNOWN TYPE ");
                        AddCard (str, type);  AddChar (str, LF);
                        Skip (shortlength, varlength);
            END (*CASE*);
        END (*IF*);
        BigNum.Discard (length);

    END ParseASN1;

(************************************************************************)
(*                  TRANSLATING ASN.1 TO HUMAN-READABLE                 *)
(************************************************************************)

PROCEDURE ASN1toTxt (VAR (*IN*) bindata: ARRAY OF CARD8;  N: CARDINAL;
                            VAR (*OUT*) textptr: VarStringPtr): CARDINAL;

    (* Translates N binary bytes in ASN.1 format to text.  Returns  *)
    (* the number of text characters.                               *)

    CONST BufSize = 65536;

    VAR str: LongString;
        pos: CARDINAL;

    BEGIN
        pos := 0;
        Initialise (str);
        ParseASN1 (bindata, pos, N, 0, str);
        Finalise (str);
        textptr := str.pval;
        RETURN str.size;
    END ASN1toTxt;

(************************************************************************)
(*            EXTRACTING PARTS OF AN ASN.1 BINARY STRING                *)
(************************************************************************)

PROCEDURE ExtractParts (VAR (*IN*) bindata: ARRAY OF CARD8;
                        VAR (*OUT*) result: ARRAY OF ASN1part): CARDINAL;

    (* If bindata represents a SEQUENCE or a SET, result[k] contains    *)
    (* a copy of the kth component of the sequence or set.  The         *)
    (* function result is the number of components extracted.           *)
    (* Otherwise, no result is extracted, we return 0 as the function   *)
    (* result.  Note that some components will not be extracted if the  *)
    (* result array is not big enough.                                  *)

    (* In this version we don't make a copy for the result; we simply   *)
    (* point into the original bindata array.                           *)

    VAR pos0, pos, type, class, count, hsize, M, N: CARDINAL;
        constructed, varlength, found: BOOLEAN;
        length: BN;

    BEGIN
        (* Read the overall type/length information. *)

        count := 0;
        pos := 0;
        EVAL (TypeAndLength (bindata, pos, type, class,
                                        constructed, varlength, length));

        IF (type < 16) OR (type > 17) THEN
            (* Not a sequence or set. *)
            RETURN 0;
        END (*IF*);

        N := BigNum.Card(length);       (* bytes left to go *)
        M := 0;

        WHILE N > M DO

            DEC (N, M);

            (* Get the next component. *)

            pos0 := pos;
            hsize := TypeAndLength (bindata, pos, type, class,
                                        constructed, varlength, length);

            IF varlength THEN
                (* Scan forward looking for double-zero terminator. *)
                M := hsize;
                found := FALSE;
                WHILE (NOT found) AND (M < N) DO
                    IF bindata[pos] = 0 THEN
                        INC (pos);  INC (M);
                        IF bindata[pos] = 0 THEN
                            INC (pos);  INC (M);
                            found := TRUE;
                        END (*IF*);
                    ELSE
                        INC (pos);  INC (M);
                    END (*IF*);
                END (*WHILE*);
            ELSE
                M := BigNum.Card(length);
                INC (pos, M);
                INC (M, hsize);
            END (*IF*);

            WITH result[count] DO
                length := M;
                val := ADR(bindata[pos0]);
            END (*WITH*);
            INC (count);

        END (*WHILE*);

        RETURN count;

    END ExtractParts;

(************************************************************************)

PROCEDURE INTEGERtoBN (VAR (*IN*) bindata: ARRAY OF CARD8;  pos: CARDINAL): BN;

    (* Converts an ASN.1 BIT STRING to a Bignum.  Returns zero if an    *)
    (* INTEGER does not start at bindata[pos].                          *)

    VAR type, class: CARDINAL;
        constructed, varlength: BOOLEAN;
        length: BN;

    BEGIN
        EVAL (TypeAndLength (bindata, pos, type, class,
                                        constructed, varlength, length));
        IF type <> 2 THEN
            (* Not an integer. *)
            RETURN BigNum.Zero();
        END (*IF*);
        RETURN GetInt (bindata, pos, length);
    END INTEGERtoBN;

(************************************************************************)

PROCEDURE BitStringToBN (VAR (*IN*) bindata: ARRAY OF CARD8;  pos: CARDINAL): BN;

    (* Converts an ASN.1 BIT STRING to a Bignum.  Returns zero if a BIT *)
    (* STRING does not start at bindata[pos].                           *)

    VAR type, class, shortlength, unused: CARDINAL;
        constructed, varlength: BOOLEAN;
        length, val: BN;

    BEGIN
        EVAL (TypeAndLength (bindata, pos, type, class,
                                        constructed, varlength, length));
        IF type <> 3 THEN
            (* Not a bit string. *)
            RETURN BigNum.Zero();
        END (*IF*);
        shortlength := BigNum.Card(length);
        unused := bindata[pos];  INC(pos);  DEC (shortlength);
        val := BigNum.BinToBN (bindata, pos, shortlength);
        IF unused > 0 THEN
            BigNum.RSBN (val, unused);
        END (*IF*);
        RETURN val;
    END BitStringToBN;

(************************************************************************)

PROCEDURE GetObjID (VAR (*IN*) bindata: ARRAY OF CARD8;  pos: CARDINAL;
                           VAR (*OUT*) result: ARRAY OF CARDINAL): CARDINAL;

    (* Converts an ASN.1 BIT STRING to an OBJECT ID.  Returns the       *)
    (* number of components, or zero if an object ID does not start at  *)
    (* bindata[pos].                                                    *)

    VAR type, class, shortlength, val, temp, j, N: CARDINAL;
        constructed, varlength: BOOLEAN;
        length: BN;

    BEGIN
        EVAL (TypeAndLength (bindata, pos, type, class,
                                        constructed, varlength, length));
        IF type <> 6 THEN
            (* Not an object ID. *)
            RETURN 0;
        END (*IF*);

        shortlength := BigNum.Card(length);
        temp := Fetch(bindata, pos);
        DEC (shortlength);
        result[0] := temp DIV 40;
        result[1] := temp MOD 40;
        N := 2;
        WHILE shortlength > 0 DO
            val := 0;
            LOOP
                j := Fetch(bindata, pos);
                DEC (shortlength);
                val := 128 * val + IAND(j, 127);
                IF IAND (j, 80H) = 0 THEN
                    EXIT (*LOOP*);
                END (*IF*);
            END (*LOOP*);
            result[N] := val;  INC(N);
        END (*WHILE*);
        RETURN N;
    END GetObjID;

(************************************************************************)

END ASN1.


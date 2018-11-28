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

IMPLEMENTATION MODULE TLS;

        (********************************************************)
        (*                                                      *)
        (*                Transport Layer Security              *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            2 April 2018                    *)
        (*  Last edited:        26 October 2018                 *)
        (*  Status:             Still seriously incomplete      *)
        (*                                                      *)
        (********************************************************)

(************************************************************************)
(*  NOTE ABOUT VERSIONS                                                 *)
(*  Version numbering continues on from the SSL version numbers.        *)
(*  Thus we have:                                                       *)
(*          3.0 or lower    SSL versions pre-TLS                        *)
(*          3.1             TSL version 1.0                             *)
(*          3.2             TSL version 1.1                             *)
(*          3.3             TSL version 1.2                             *)
(*  The present code supports only 3.3.                                 *)
(************************************************************************)


FROM STextIO IMPORT                 (* for debugging *)
    (* proc *)  WriteChar, WriteString, WriteLn;

IMPORT Strings;

FROM SYSTEM IMPORT CAST, CARD8, CARD16, CARD32, ADR, LOC;

FROM BigNum IMPORT
    (* type *)  BN;

FROM VarStrings IMPORT
    (* type *)  ByteStringPtr;

FROM TLSBase IMPORT
    (* proc *)  NYI, WriteHex, WriteHexString, AppendHexString,
                Get3Bytes, Put3Bytes,
                Checksize, Resize;

FROM TLSRecord IMPORT
    (* type *)  RLState, TextType,
    (* proc *)  TLSRecordInit, TLSRecordClose, FirstNBytes, GetFragment, DiscardFragment, PutFragment,
                RecordTraceOn, RecordTraceOff;

FROM CipherSuites IMPORT
    (* const*)  NullSuite,
    (* type *)  CipherSuite,
    (* proc *)  NameOfSuite, AcceptCipherSuite, CodeToCipherSuite, CipherSuiteToCode;

FROM TLScompression IMPORT
    (* type *)  CompressionMethod,
    (* proc *)  CompressionSupported;

FROM Certificates IMPORT
    (* proc *)  AssembleOurCertificates, AcceptCertificates;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  LogTransaction, LogTransactionL;

FROM FileOps IMPORT
    (* type *)  ChanId,
    (* proc *)  OpenOldFile, CloseFile, ReadRaw, ReadLine;

FROM TimeConv IMPORT
    (* proc *)  time;

FROM RandCard IMPORT
    (* proc *)  RandCardinal;

FROM MiscFuncs IMPORT
    (* proc *)  AppendCard;

FROM Inet2Misc IMPORT
    (* proc *)  Swap2;

FROM Sockets IMPORT
    (* type *)  Socket,
    (* proc *)  recv, soclose;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, Obtain, Release;

FROM LowLevel IMPORT
    (* proc *)  Copy, EVAL, AddOffset;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

TYPE
    Byte32 = ARRAY [0..31] OF CARD8;
    Byte46 = ARRAY [0..45] OF CARD8;    (* used? possibly not *)
    Byte48 = ARRAY [0..47] OF CARD8;    (* used only by client premaster *)

CONST
    Nul = CHR(0);
    CtrlZ = CHR(26);
    SSLversionMajor = 3;
    SSLversionMinor = 3;

    Zero32 = Byte32 {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

    Zero48 = Byte48 {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

    ListCipherSuites = TRUE;

    maxIDsize = 32;

TYPE
    (* The standard defines a type "opaque" for both fixed-length and   *)
    (* variable-length arrays.  In the variable-length case only, such  *)
    (* an array is passed in messages as a length followed by the data. *)
    (* The number of bytes in the length depends on the maximum array   *)
    (* length; we therefore define two types, "opaque short" and        *)
    (* "opaque medium".  These are used only for variable-length byte   *)
    (* strings.  For fixed-length, we just use a fixed-length data type.*)

    OpaqueS =   RECORD
                    length: CARD8;
                    string: ByteStringPtr;
                END (*RECORD*);

    OpaqueM =   RECORD
                    length: CARD16;
                    string: ByteStringPtr;
                END (*RECORD*);

TYPE
    sessIDtype = ARRAY [0..maxIDsize] OF CARD8;

    TLSsession = POINTER TO SessionState;

    PRFAlgorithm = (tls_prf_sha256);
    BulkCipherAlgorithm = (nocipher, rc4, tripledes, aes);
    CipherType = (stream, block, aead);
    MACAlgorithm = (null, hmac_md5, hmac_sha1, hmac_sha256, hmac_sha384,
                                 hmac_sha512);

    SecurityParameters =
            RECORD
                prf_algorithm:          PRFAlgorithm;      (* constant? *)
                bulk_cipher_algorithm:  BulkCipherAlgorithm;  (* used? *)
                cipher_type:            CipherType;        (* used? *)
                enc_key_length:         CARD8;             (* used? *)
                block_length:           CARD8;             (* used? *)
                fixed_iv_length:        CARD8;             (* used? *)
                record_iv_length:       CARD8;             (* used? *)
                mac_algorithm:          MACAlgorithm;      (* used? *)
                mac_length:             CARD8;             (* used? *)
                mac_key_length:         CARD8;             (* used? *)
                compression_algorithm:  CompressionMethod; (* set during handshake *)
                master_secret:          Byte48;            (* used? *)
                client_random:          Byte32;            (* supplied by client. used? *)
                server_random:          Byte32;            (* generated, sent to client *)
                client_premaster:       Byte48;            (* supplied by client *)

                (* Extra fields not mentioned in the standard. *)

                pending, ciphersuite:   CipherSuite;
                (*encrrule:               EncrRule;*)

            END (*RECORD*);

    SessionState =  RECORD
                        IsServer:   BOOLEAN;
                        sock:       Socket;
                        logID:      TransactionLogID;
                        sessID:     sessIDtype;
                        security:   SecurityParameters;
                        rlstate:    RLState;
                    END (*RECORD*);

(************************************************************************)

PROCEDURE DoHandshake (sess: TLSsession): BOOLEAN;   FORWARD;

PROCEDURE DoHandshake (sess: TLSsession): BOOLEAN;   FORWARD;

(************************************************************************)
(*                              DEBUGGING STUFF                         *)
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

PROCEDURE BEval  (VAR (*IN*) buf: ARRAY OF CARD8;  pos, N: CARDINAL): CARDINAL;

    (* Treats the next N bytes, starting at buf[pos], as bigendian.  *)

    VAR result, k: CARDINAL;

    BEGIN
        result := 0;
        FOR k := 0 TO N-1 DO
            result := 16*result + buf[pos];
            INC (pos);
        END (*FOR*);
        RETURN result;
    END BEval;

(************************************************************************)
(*                             SESSION ID                               *)
(* For a full implementation, the client supplies a session ID during   *)
(* a handshake, and the server either accepts that ID or generates a    *)
(* new ID to indicate that it does not agree to resume the previous     *)
(* session.  In my present ID I'm not cacheing old sessions, so I       *)
(* generate a unique ID every time.                                     *)
(************************************************************************)

VAR NextID: sessIDtype;
    NextIDLock: Lock;

(************************************************************************)

PROCEDURE ResetNextID;

    VAR j: CARDINAL;

    BEGIN
        Obtain (NextIDLock);
        NextID[0] := maxIDsize;
        FOR j := 1 TO maxIDsize DO
            NextID[j] := 0;
        END (*FOR*);
        Release (NextIDLock);
    END ResetNextID;

(************************************************************************)

PROCEDURE IncrNextID (k: CARDINAL);

    (* Increments NextID[1..k].  We assume that the caller has          *)
    (* exclusive access to NextID.                                      *)

    BEGIN
        IF NextID[k] = MAX(CARD8) THEN
            NextID[k] := 0;
            IF k > 1 THEN
                IncrNextID (k-1);
            END (*IF*);
        ELSE
            INC (NextID[k]);
        END (*IF*);
    END IncrNextID;

(************************************************************************)

PROCEDURE GenerateSessionID (VAR (*OUT*) ID: sessIDtype);

    BEGIN
        Obtain (NextIDLock);
        ID := NextID;
        IncrNextID (maxIDsize);
        Release (NextIDLock);
    END GenerateSessionID;

(************************************************************************)
(*                        GENERATE RANDOM NUMBER                        *)
(************************************************************************)

PROCEDURE GenerateRandom (N: CARDINAL;  VAR (*OUT*) result: ARRAY OF CARD8);

    (* Puts an N-byte "random" field in result.  In the case N=32, the  *)
    (* first four bytes are Unix time, and we follow this with 28       *)
    (* random bytes.  For other values of N, all N bytes are random.    *)

    VAR pos: CARDINAL;

    (********************************************************************)

    PROCEDURE PutWord (A: CARDINAL);

        (* Stores the next 4 bytes of the result. *)

        VAR val: RECORD
                    CASE :BOOLEAN OF
                        FALSE:  byte: ARRAY [0..3] OF CARD8;
                        |
                        TRUE:   word: CARD32;
                    END (*CASE*);
                 END (*RECORD*);
            j: CARDINAL;

        BEGIN
            val.word := A;
            FOR j := 0 TO 3 DO
                result[pos] := val.byte[j];
                INC (pos);
            END (*FOR*);
        END PutWord;

    (********************************************************************)

    VAR j, k: CARDINAL;

    BEGIN
        pos := 0;
        IF N = 32 THEN
            PutWord (time());
            N := 28;
        END (*IF*);
        FOR j := 1 TO N DIV 4 DO
            PutWord (RandCardinal());
        END (*FOR*);
        N := N MOD 4;
        IF N > 0 THEN
            k := RandCardinal();
            FOR j := 0 TO N-1 DO
                result[pos] := k MOD 256;
                k := k DIV 256;
                INC (pos);
            END (*FOR*);
        END (*IF*);
    END GenerateRandom;

(************************************************************************)
(*                      OPENING AND CLOSING A SESSION                   *)
(************************************************************************)

PROCEDURE CreateSessionRecord (S: Socket;  as_server: BOOLEAN;
                                    logID: TransactionLogID): TLSsession;

    (* Sets up the initial TLS state.    *)

    VAR result: TLSsession;

    BEGIN
        NEW (result);
        result^.IsServer := as_server;
        result^.sock := S;
        result^.logID := logID;
        WITH result^.security DO
            prf_algorithm           := tls_prf_sha256;
            bulk_cipher_algorithm   := nocipher;
            cipher_type             := stream;
            enc_key_length          := 0;
            block_length            := 0;
            fixed_iv_length         := 0;
            record_iv_length        := 0;
            mac_algorithm           := null;
            mac_length              := 0;
            mac_key_length          := 0;
            compression_algorithm   := nocompression;
            master_secret           := Zero48;
            client_random           := Zero32;
            server_random           := Zero32;
            pending                 := NullSuite;
            ciphersuite             := NullSuite;
        END (*WITH*);
        RETURN result;
    END CreateSessionRecord;

(************************************************************************)

PROCEDURE OpenTLSsession (S: Socket;  as_server: BOOLEAN;  logID: TransactionLogID): TLSsession;

    (* Opens a new connection.  Returns NIL if the operation failed.    *)

    VAR result: TLSsession;

    BEGIN
        result := CreateSessionRecord (S, as_server, logID);
        result^.rlstate := TLSRecordInit(S);
        IF as_server THEN
            IF DoHandshake(result) THEN
            ELSE
                DISPOSE (result);
            END (*IF*);
        ELSIF NOT DoHandshake(result) THEN
            DISPOSE (result);
        END (*IF*);
        RETURN result;
    END OpenTLSsession;

(************************************************************************)

PROCEDURE CloseTLSsession (VAR (*INOUT*) sess: TLSsession);

    (* Closes the session, deletes the session data. *)

    BEGIN
        (* There are probably some formalities I have to do here, but   *)
        (* I'll come back to that later.                                *)

        IF sess <> NIL THEN
            TLSRecordClose (sess^.rlstate);
            soclose (sess^.sock);
            DISPOSE (sess);
        END (*IF*);
    END CloseTLSsession;

(************************************************************************)
(*                    SENDING DATA TO RECORD LAYER                      *)
(************************************************************************)

PROCEDURE TLSsend (sess: TLSsession;  VAR (*IN*) message: ARRAY OF LOC;
                                                length: CARDINAL): CARDINAL;

    (* Sends a message of "length" bytes.  The returned value is the    *)
    (* number of bytes sent, or MAX(CARDINAL) if there was an error.    *)

    BEGIN
        (* NOT YET IMPLEMENTED *)
        RETURN MAX(CARDINAL);
    END TLSsend;

(************************************************************************)
(*                              ALERTS                                  *)
(************************************************************************)

PROCEDURE LogAlert (ID: TransactionLogID;  preamble: ARRAY OF CHAR;
                                                    buffer: TextType);

    (* If we receive an alert, we can't do much more than log it. *)

    VAR type, descr: CARD8;  success: BOOLEAN;
        message: ARRAY [0..127] OF CHAR;

    BEGIN
        Strings.Assign (preamble, message);
        IF buffer.fragment = NIL THEN
            Strings.Append ("buffer.fragment = NIL", message);
        ELSE
            type := buffer.fragment^[0];
            descr := buffer.fragment^[1];
            success := type = 1;    (* warning rather than fatal *)
            IF success THEN Strings.Append ("Warning", message);
            ELSE Strings.Append ("Fatal", message);
            END (*IF*);
            IF descr = 10 THEN
                Strings.Append (" error: unexpected message", message);
            ELSE
                Strings.Append (" alert ", message);
                AppendCard (descr, message);
                CASE descr OF
                    | 40:   Strings.Append (" = handshake failure", message);
                ELSE
                    (* Don't need explanations of other cases so far. *)
                END (*CASE*);
            END (*IF*);
        END (*IF*);
        LogTransaction (ID, message);
    END LogAlert;

(************************************************************************)
(*                            HANDSHAKES                                *)
(************************************************************************)

PROCEDURE SendHandshake (sess: TLSsession;  type: CARD8;
                                VAR (*INOUT*) content: ByteStringPtr;
                                  length: CARDINAL);

    (* A handshake record starts with code 22 to mean "handshake", then *)
    (* we have a two-byte version code and a two-byte overall length    *)
    (* Then we have the "fragment" field with                           *)
    (*      HandshakeType (1 byte)                                      *)
    (*      length        (3 bytes)                                     *)
    (*      further data  as supplied by the caller                     *)
    (* For efficiency the caller must provide the space for the four    *)
    (* initial bytes, which we will fill in, in the content^ record.    *)
    (* The length parameter supplied by the called includes counting    *)
    (* these initial four bytes.                                        *)
    (* We deallocate the caller data before return.                     *)

    VAR buffer: TextType;
        j, L0: CARDINAL;

    BEGIN

        buffer.contenttype := 22;   (* type 22 means "handshake" *)
        buffer.majversion := SSLversionMajor;
        buffer.minversion := SSLversionMinor;

        (* To embed the handshake record into something suitable to     *)
        (* pass the record layer, we have to prepend a one-byte         *)
        (* handshake type and a 3-byte length field.  At present I'm    *)
        (* doing this by requiring the caller to leave the first four   *)
        (* bytes in content^ free for our use.  There must be a better  *)
        (* way to handle a layered protocol, but for now I'll use the   *)
        (* crude obvious approach.                                      *)

        buffer.length := Swap2 (length);        (* convert to BigEndian *)
        L0 := length - 4;
        content^[0] := type;
        FOR j := 3 TO 1 BY -1 DO
            content^[j] := L0 MOD 256;
            L0 := L0 DIV 256;
        END (*FOR*);

        (*
        WriteString ("In SendHandshake, first few bytes of output are ");
        WriteHexString (content^, 0, 32, TRUE);
        WriteLn;
        *)

        buffer.fragment := content;
        PutFragment (sess^.rlstate, sess^.logID, buffer, 22);
        DiscardFragment (buffer);

    END SendHandshake;

(************************************************************************)
(*             HANDSHAKE PROTOCOL -- ClientHello EXTENSIONS             *)
(*                                                                      *)
(* Browsers that I have checked use the following extensions (hex):     *)
(*      Firefox 38.8.0    0000 0005 000A 000B 000D 0010 0023 3374 FF01  *)
(*      Firefox 59.0.2    0005 000A 000B 0010 0017 0023 0033 FF01       *)
(*                          (list possibly incomplete, there's a bug)   *)
(*      M$Edge            0005 000A 000B 000D 0010 0017 0023            *)
(*      Chrome            0005 000A 000B 000D 0010 0012 0017            *)
(*                          0023 1A1A 3A3A 7550 FF01                    *)
(*                                                                      *)
(************************************************************************)

PROCEDURE Extension0 (VAR (*IN*) record: ByteStringPtr;
                                      offset, size: CARDINAL);

    (* The server_name extension - probably not needed. *)

    VAR namelength: CARDINAL;
        nametype: CARD8;

    BEGIN
        WriteString ("  server_name");
        WHILE size > 0 DO

            WriteLn;

            (* In each subrecord the length is redundantly given    *)
            (* twice, so we can just skip the outer length spec.    *)

            INC (offset, 2);  DEC (size, 2);
            nametype := record^[0];
            INC (offset);  DEC (size);
            namelength := BEval(record^, offset, 2);
            INC (offset, 2);  DEC (size, 2);
            WriteString ("    ");
            WriteCard (nametype);  WriteChar (' ');
            WriteAscString (record^, offset, namelength);
            INC (offset, namelength);  DEC (size, namelength);
        END (*WHILE*);
    END Extension0;

(************************************************************************)

PROCEDURE Extension13 (VAR (*IN*) record: ByteStringPtr;
                                      offset, size: CARDINAL);

    (* The signature_algorithms extension, definitely important. *)

    VAR code: CARD8;

    BEGIN
        WriteString ("  signature_algorithms");
        (*
        WriteLn;
        WriteString ("The next few bytes in the record are: ");  WriteLn;
        WriteHexString (record^, offset, 32); WriteLn;
        *)

        size := BEval(record^, offset, 2);  INC (offset, 2);
        WriteLn;

        WHILE size > 0 DO

            WriteString ("  ");

            (* Read a pair of values, one for hash algorithm and    *)
            (* one for signature algorithm.                         *)

            code := record^[offset];
            INC (offset);  DEC (size);
            CASE code OF
                    0:  WriteString ("none");
                |   1:  WriteString ("md5");
                |   2:  WriteString ("sha1");
                |   3:  WriteString ("sha224");
                |   4:  WriteString ("sha256");
                |   5:  WriteString ("sha384");
                |   6:  WriteString ("sha512");
                ELSE    WriteString ("???");
            END (*CASE*);
            WriteChar ('_');
            code := record^[offset];
            INC (offset);  DEC (size);
            CASE code OF
                    0:  WriteString ("anon");
                |   1:  WriteString ("rsa");
                |   2:  WriteString ("dsa");
                |   3:  WriteString ("ecdsa");
                ELSE    WriteString ("???");
            END (*CASE*);
        END (*WHILE*);
    END Extension13;

(************************************************************************)
(*                              CLIENT HELLO                            *)
(************************************************************************)

PROCEDURE ParseHandshakeExtensions (VAR (*IN*) record: ByteStringPtr;
                                      offset, length: CARDINAL);

    (* Parses the extension fields in a ClientHello, but for now does   *)
    (* nothing with them.                                               *)

    VAR size, type: CARDINAL;

    BEGIN
        length := BEval (record^, offset, 2);
        INC (offset, 2);
        WHILE length >= 4 DO

            (* Each extension begins with four bytes: an extension  *)
            (* type and a length.                                   *)

            WriteString ("Extension type ");
            type := BEval (record^, offset, 2);
            WriteHexString (record^, offset, 2, FALSE);
            INC (offset, 2);
            size := BEval (record^, offset, 2);
            INC (offset, 2);
            WriteString ("  length ");  WriteCard (size);
            IF type = 0 THEN
                Extension0 (record, offset, size);
            ELSIF type = 13 THEN
                Extension13 (record, offset, size);
            END (*IF*);
            WriteLn;
            INC (offset, size);
            DEC (length, size+4);
        END (*WHILE*);
    END ParseHandshakeExtensions;

(************************************************************************)

PROCEDURE HandleClientHello (sess: TLSsession;  VAR (*IN*) record: ByteStringPtr;
                                      offset, length: CARDINAL): BOOLEAN;

    (* The data for this level start at record[offset].    *)

    VAR sublength, unixtime, j, k: CARDINAL;
        sessionID: ARRAY [0..31] OF CARD8;
        name: ARRAY [0..31] OF CHAR;
        code1, code2: CARD8;
        suite, newsuite: CipherSuite;
        method, newmethod: CompressionMethod;
        message: ARRAY [0..127] OF CHAR;

    BEGIN
        (*
        WriteString ("In HandleClientHello, first few bytes of input are ");
        WriteHexString (record^, 0, 32, TRUE);
        WriteLn;
        *)
        (*
        message := "In HandleClientHello, initially length = ";
        AppendCard (length, message);
        LogTransaction (sess^.logID, message);
        *)

        (* VERSION - which we are currently not checking for compatibility *)

        (* The first two bytes should be the version.  Note that I  *)
        (* am updating length to show how many bytes are left in    *)
        (* this Hello message.                                      *)

        (*
        WriteString ("Protocol version ");  WriteCard (record^[offset]);
        WriteChar ('.');  WriteCard (record^[offset+1]);  WriteLn;
        *)
        DEC (length, 2);

        (* CLIENT RANDOM SEED *)

        (* Copy client random seed. *)

        Copy (ADR(record^[offset+2]), ADR(sess^.security.client_random), 32);
        (*
        Strings.Assign ("client_random = ", message);
        AppendHexString (sess^.security.client_random, 0, 32, FALSE, message);
        LogTransaction (sess^.logID, message);
        *)
        DEC (length, 32);

        (* First four bytes of seed should be Unix time. *)

        (*
        unixtime := 0;
        FOR j := 0 TO 3 DO
            unixtime := 256*unixtime + sess^.security.client_random[j];
        END (*FOR*);
        WriteString ("Unix time = ");  WriteCard (unixtime);  WriteLn;
        *)

        (* SESSION ID - ignored in this version *)

        (* Client's session ID.  The first byte is the length.  *)

        j := offset+34;
        sublength := record^[j];  INC (j);
        DEC (length, sublength+1);
        k := 0;
        WHILE sublength > 0 DO
            sessionID[k] := record^[j];  INC(j);  INC(k);
            DEC (sublength);
        END (*WHILE*);
        IF k < 32 THEN
            sessionID[k] := 0;
        END (*IF*);
        Strings.Assign ("Session ID = ", message);
        IF k = 0 THEN
            Strings.Append ("''", message);
        ELSE
            AppendHexString (sessionID, 0, k, FALSE, message);
        END (*IF*);
        LogTransaction (sess^.logID, message);

        (* CIPHER SUITES *)

        (* Now we have to get a list of the cipher suites acceptable to the client. *)
        (* Each cipher suite code is two bytes long. *)

        suite := NullSuite;
        sublength := 256*record^[j] + record^[j+1];
        INC (j, 2);
        DEC (length, sublength + 2);
        sublength := sublength DIV 2;
        IF ListCipherSuites THEN
            WriteString ("Client will accept cipher suites:");  WriteLn;
        END (*IF*);
        FOR k := 0 TO sublength-1 DO
            code1 := record^[j];  INC (j);
            code2 := record^[j];  INC (j);
            IF ListCipherSuites THEN
                WriteString ("    ");
                WriteHex (code1);  WriteChar (' ');  WriteHex (code2);
            END (*IF*);
            IF (suite = NullSuite) OR ListCipherSuites THEN
                newsuite := AcceptCipherSuite(code1, code2);
                IF (suite = NullSuite) AND (newsuite <> NullSuite) THEN
                    suite := newsuite;
                END (*IF*);
                IF ListCipherSuites THEN
                    IF newsuite <> NullSuite THEN
                        WriteString ("   accepted");
                    ELSE
                        WriteString ("   rejected");
                    END (*IF*);
                    WriteLn;
                END (*IF*);
            END (*IF*);
        END (*FOR*);
        Strings.Assign ("ciphersuite = ", message);
        NameOfSuite (suite, name);
        Strings.Append (name, message);
        LogTransaction (sess^.logID, message);

        (*
        message := "Just before getting compression algorithms, j = ";
        AppendCard (j, message);
        Strings.Append (" and length = ", message);
        AppendCard (length, message);
        LogTransaction (sess^.logID, message);
        *)

        (* COMPRESSION ALGORITHMS *)

        (* And then the compression methods. *)

        sublength := record^[j];
        INC (j);
        DEC (length, sublength+1);

        method := nocompression;
        FOR k := 1 TO sublength DO
            code1 := record^[j];  INC (j);
            newmethod := CompressionSupported(code1);
            IF (method = nocompression) AND (newmethod <> nocompression) THEN
                method := newmethod;
            END (*IF*);
        END (*FOR*);
        WriteLn;
        sess^.security.compression_algorithm := method;

        (* EXTENSIONS *)

        IF length > 0 THEN

            ParseHandshakeExtensions (record, j, length);

        END (*IF*);

        RETURN TRUE;

    END HandleClientHello;

(************************************************************************)

PROCEDURE SendClientHello (sess: TLSsession): BOOLEAN;

    (* Send the client hello to the server.         *)
    (* The data for this level start at record[0].  *)

    CONST
        InitialSize = 128;
        client_hello_code = 1;

    VAR recsize, length: CARDINAL;
        name: ARRAY [0..31] OF CHAR;
        record: ByteStringPtr;
        suite: CipherSuite;
        message: ARRAY [0..127] OF CHAR;
        code1, code2: CARD8;

    BEGIN
        LogTransactionL (sess^.logID, "Sending client hello");
        recsize := InitialSize;
        ALLOCATE (record, recsize);
        length := 4;            (* Initial space reserved for SendHandshake *)

        (* VERSION *)

        (* The first two bytes should be the version.  *)

        Checksize (record, recsize, length, 2);
        record^[length] := SSLversionMajor;  INC (length);
        record^[length] := SSLversionMinor;  INC (length);

        (* CLIENT RANDOM SEED *)

        (* Create and send client random seed. *)

        GenerateRandom (32, sess^.security.client_random);
        Checksize (record, recsize, length, 32);
        Copy (ADR(sess^.security.client_random), ADR(record^[length]), 32);
        (*
        Strings.Assign ("client_random = ", message);
        AppendHexString (sess^.security.client_random, 0, 32, FALSE, message);
        LogTransaction (sess^.logID, message);
        *)
        INC (length, 32);

        (* SESSION ID - just set to 0 in this version *)

        (* Client's session ID.  The first byte is the length.  *)

        Checksize (record, recsize, length, 2);
        record^[length] := 1;  INC (length);
        record^[length] := 0;  INC (length);

        (* CIPHER SUITES *)

        (* Now we have to get a list of the cipher suites acceptable to the client. *)
        (* Each cipher suite code is two bytes long. *)
        (* In this version I'll just list one suite, for testing.    *)

        (*suite := NullSuite;*)
        (*suite := TLS_RSA_WITH_AES_256_CBC_SHA256;*)
        suite := TLS_RSA_WITH_3DES_EDE_CBC_SHA;       (* google.com accepts this one *)
        CipherSuiteToCode (suite, code1, code2);
        Checksize (record, recsize, length, 4);
        record^[length] := 0;  INC (length);        (* length of list (2 bytes) *)
        record^[length] := 2;  INC (length);
        record^[length] := code1;  INC (length);
        record^[length] := code2;  INC (length);
        Strings.Assign ("ciphersuite = ", message);
        NameOfSuite (suite, name);
        Strings.Append (name, message);
        LogTransaction (sess^.logID, message);

        (* COMPRESSION ALGORITHMS *)

        (* And then the compression methods. *)
        (* For now we support only "no compression". *)

        sess^.security.compression_algorithm := nocompression;
        Checksize (record, recsize, length, 2);
        record^[length] := 1;  INC (length);        (* length of list (1 byte) *)
        record^[length] := ORD(sess^.security.compression_algorithm);  INC (length);

        (* EXTENSIONS *)

        (* No extensions supported in this version.  *)

        Resize (record, recsize, length);

        message := "Sending client hello, ";
        AppendCard (length, message);
        Strings.Append (" bytes.", message);
        LogTransaction (sess^.logID, message);
        (*
        WriteString ("First few bytes of output are ");
        WriteHexString (record^, 0, 51, TRUE);
        WriteLn;
        *)

        SendHandshake (sess, client_hello_code, record, length);

        RETURN TRUE;

    END SendClientHello;

(************************************************************************)
(*                            KEY EXCHANGE                              *)
(************************************************************************)

PROCEDURE HandleClientKeyExchange (sess: TLSsession;  VAR (*IN*) record: ByteStringPtr;
                                      offset, length: CARDINAL): BOOLEAN;

    (* The data for this level start at record[offset].                 *)
    (* In the present version the only supported key exchange algorithm *)
    (* is RSA, so what the client sends is the encrypted premaster      *)
    (* secret.  This is a two-byte version, followed by a 46-byte       *)
    (* random value.  The version is part of the premaster secret.      *)

    VAR success: BOOLEAN;

    BEGIN
        success := TRUE;

        (* CLIENT PREMASTER SECRET *)

        (* Copy client random seed. *)

        Copy (ADR(record^[offset]), ADR(sess^.security.client_premaster), 48);
        WriteString ("client_premaster field is ");
        WriteHexString (sess^.security.client_premaster, 0, 48, TRUE);
        WriteLn;
        DEC (length, 48);

        (* VERSION *)

        (* The first two bytes should be the version.  *)

        WriteString ("Premaster protocol version ");  WriteCard (record^[offset]);
        WriteChar ('.');  WriteCard (record^[offset+1]);  WriteLn;

        RETURN success;

    END HandleClientKeyExchange;

(************************************************************************)

PROCEDURE SendClientKey (sess: TLSsession): BOOLEAN;

    (* Send the client premaster information to the server.             *)
    (* In the present version the only supported key exchange algorithm *)
    (* is RSA, so what the client sends is the encrypted premaster      *)
    (* secret.  This is a two-byte version, followed by a 46-byte       *)
    (* random value which is the premaster secret.                      *)

    CONST
        InitialSize = 128;
        client_keyexchange_code = 16;

    VAR recsize, length: CARDINAL;
        name: ARRAY [0..31] OF CHAR;
        record: ByteStringPtr;
        suite: CipherSuite;
        message: ARRAY [0..127] OF CHAR;
        code1, code2: CARD8;

    BEGIN
        LogTransactionL (sess^.logID, "Sending client hello");
        recsize := InitialSize;
        ALLOCATE (record, recsize);
        length := 4;            (* Initial space reserved for SendHandshake *)

        (* VERSION *)

        (* The first two bytes should be the version.  *)

        Checksize (record, recsize, length, 2);
        record^[length] := SSLversionMajor;  INC (length);
        record^[length] := SSLversionMinor;  INC (length);

        (* CLIENT PREMASTER SECRET *)

        (* For RSA, this is a two-byte version followed by 46 random bytes. *)

        GenerateRandom (48, sess^.security.client_premaster);
        Checksize (record, recsize, length, 48);
        Copy (ADR(sess^.security.client_premaster), ADR(record^[length]), 48);

        (* VERSION *)

        (* The first two bytes should be the version.  *)

        record^[length] := SSLversionMajor;
        record^[length+1] := SSLversionMinor;
        INC (length, 48);

        SendHandshake (sess, client_keyexchange_code, record, length);

        RETURN TRUE;

    END SendClientKey;

(************************************************************************)
(*                             SERVER HELLO                             *)
(************************************************************************)

PROCEDURE CopyBytes (VAR (*IN*) src: ARRAY OF LOC;  N: CARDINAL;
                      record: ByteStringPtr;  VAR (*INOUT*) pos: CARDINAL);

    (* Copies N bytes from src to record^[pos], updates pos. *)

    VAR j: CARDINAL;

    BEGIN
        FOR j := 0 TO N-1 DO
            record^[pos] := CAST (CARD8,src[j]);
            INC (pos);
        END (*FOR*);
    END CopyBytes;

(************************************************************************)

PROCEDURE AssembleServerHello (sess: TLSsession;
                        VAR (*OUT*) record: ByteStringPtr): CARDINAL;

    (* Puts together the content of a server hello, returns its length. *)
    (* This content consists of                                         *)
    (*      overhead        (4 bytes)                                   *)
    (*      version         (2 bytes)                                   *)
    (*      random seed     (32 bytes)                                  *)
    (*      session ID      (1-byte length, then up to 32 bytes of ID)  *)
    (*      ciphersuite     (2 bytes)                                   *)
    (*      compression method  (1 byte)                                *)
    (* That adds up to 42 plus the session ID length.                   *)
    (* Extensions can follow, but for now I'll leave those out.         *)

    VAR pos, amount: CARDINAL;
        code1, code2: CARD8;

    BEGIN
        amount := 42 + sess^.sessID[0];
        ALLOCATE (record, amount);
        record^[4] := SSLversionMajor;  record^[5] := SSLversionMinor;
        pos := 6;
        CopyBytes (sess^.security.server_random, 32, record, pos);
        CopyBytes (sess^.sessID, sess^.sessID[0]+1, record, pos);
        CipherSuiteToCode (sess^.security.pending, code1, code2);
        record^[pos] := code1;  INC(pos);
        record^[pos] := code2;  INC(pos);
        record^[pos] := ORD(nocompression);  INC(pos);
        IF pos <> amount THEN
            WriteString ("Allocation error in AssembleServerHello.");
            WriteLn;
        END (*IF*);

        (* Only signature algorithm I can support at present is RSA     *)
        (* with SHA256, because that's all the certificate has.         *)

        RETURN amount;

    END AssembleServerHello;

(************************************************************************)

PROCEDURE SendServerHello (sess: TLSsession);

    (* A handshake record starts with code 22 to mean "handshake", then *)
    (* we have a two-byte version code and a two-byte overall length    *)
    (* Then we have the "fragment" field with                           *)
    (*      HandshakeType (1 byte, value 2 for server hello)            *)
    (*      length        (3 bytes)                                     *)
    (*      content       (depends on handshake type)                   *)

    CONST server_hello_code = 2;

    VAR p: ByteStringPtr;
        length: CARDINAL;
        message: ARRAY [0..127] OF CHAR;

    BEGIN

        GenerateSessionID (sess^.sessID);
        GenerateRandom (32, sess^.security.server_random);
        (*
        Strings.Assign ("server_random = ", message);
        AppendHexString (sess^.security.server_random, 0, 32, FALSE, message);
        LogTransaction (sess^.logID, message);
        *)
        length := AssembleServerHello (sess, p);
        SendHandshake (sess, server_hello_code, p, length);

    END SendServerHello;

(************************************************************************)

PROCEDURE ReceiveServerHello (sess: TLSsession;  record: ByteStringPtr;
                                    offset, length: CARDINAL): BOOLEAN;

    (* Receive the server_hello handshake.                              *)

    (*      overhead        (4 bytes, handled by caller)                *)
    (*      version         (2 bytes)                                   *)
    (*      random seed     (32 bytes)                                  *)
    (*      session ID      (1-byte length, then up to 32 bytes of ID)  *)
    (*      ciphersuite     (2 bytes)                                   *)
    (*      compression method  (1 byte)                                *)

    VAR message: ARRAY [0..127] OF CHAR;
        name: ARRAY [0..31] OF CHAR;
        j, k, sublength: CARDINAL;
        code1, code2: CARD8;
        sessionID: ARRAY [0..31] OF CARD8;

    BEGIN
        LogTransactionL (sess^.logID, "Receiving server hello");

        (* VERSION *)

        (* The first two bytes should be the version.  Note that I  *)
        (* am updating length to show how many bytes are left in    *)
        (* this Hello message.                                      *)

        (*
        WriteString ("Protocol version ");  WriteCard (record^[offset]);
        WriteChar ('.');  WriteCard (record^[offset+1]);  WriteLn;
        *)
        DEC (length, 2);

        (* SERVER RANDOM SEED *)

        (* Accept server random seed. *)

        Copy (ADR(record^[offset+2]), ADR(sess^.security.server_random), 32);
        (*
        Strings.Assign ("server_random = ", message);
        AppendHexString (sess^.security.server_random, 0, 32, FALSE, message);
        LogTransaction (sess^.logID, message);
        *)
        DEC (length, 32);

        (* Client's session ID.  The first byte is the length.  *)

        j := offset+34;
        sublength := record^[j];  INC (j);
        DEC (length, sublength+1);
        k := 0;
        WHILE sublength > 0 DO
            sessionID[k] := record^[j];  INC(j);  INC(k);
            DEC (sublength);
        END (*WHILE*);
        IF k < 32 THEN
            sessionID[k] := 0;
        END (*IF*);
        Strings.Assign ("Session ID = ", message);
        IF k = 0 THEN
            Strings.Append ("''", message);
        ELSE
            AppendHexString (sessionID, 0, k, FALSE, message);
        END (*IF*);
        LogTransaction (sess^.logID, message);
        offset := j;

        (* CIPHER SUITE *)

        code1 := record^[offset];  INC (offset);  DEC(length);
        code2 := record^[offset];  INC (offset);  DEC(length);
        Strings.Assign ("ciphersuite = ", message);
        sess^.security.pending := CodeToCipherSuite (code1, code2);
        NameOfSuite (sess^.security.pending, name);
        Strings.Append (name, message);
        LogTransaction (sess^.logID, message);

        (* COMPRESSION *)

        (* The compression method code is a single byte. *)

        code1 := record^[offset];  INC (offset);  DEC (length);
        sess^.security.compression_algorithm := CompressionSupported (code1);

        IF length = 0 THEN
            LogTransactionL (sess^.logID, "Finished processing server hello");
            RETURN TRUE;
        ELSE
            LogTransactionL (sess^.logID, "Unexpected record length in ReceiveServerHello");
            RETURN FALSE;
        END (*IF*);

    END ReceiveServerHello;

(************************************************************************)
(*                      SERVER HELLO DONE                               *)
(************************************************************************)

PROCEDURE ReceiveServerHelloDone (sess: TLSsession): BOOLEAN;

    (* Receive the server_hello_done handshake. *)

    BEGIN
        (* There is nothing to do here apart from checking that the     *)
        (* server_hello_done message has arrived, and the caller has    *)
        (* seen that; otherwise we wouldn't have been called.           *)

        RETURN TRUE;

    END ReceiveServerHelloDone;

(************************************************************************)

PROCEDURE SendServerHelloDone (sess: TLSsession);

    (* A handshake record starts with code 22 to mean "handshake", then *)
    (* we have a two-byte version code and a two-byte overall length    *)
    (* Then we have the "fragment" field with                           *)
    (*      HandshakeType (1 byte, value 14 for server hello done)      *)
    (*      length        (3 bytes)                                     *)
    (*      content       (empty, in the hello_done case)               *)

    CONST server_hello_done_code = 14;

    VAR p: ByteStringPtr;

    BEGIN

        ALLOCATE (p, 4);
        RecordTraceOn;
        SendHandshake (sess, server_hello_done_code, p, 4);
        RecordTraceOff;

    END SendServerHelloDone;

(************************************************************************)
(*                         CERTIFICATE EXCHANGE                         *)
(************************************************************************)

PROCEDURE SendServerCertificate (sess: TLSsession);

    (* Sends our certificate to the client.  In this version I use a    *)
    (* test certificate with a fixed filename, and there is only one    *)
    (* certificate in the chain.                                        *)

    (* The required format is a 3-byte total length; then, for each     *)
    (* certificate, a 3-byte length followed by the certificate itself. *)
    (* That suggests that I need to define a new data type that allows  *)
    (* linking of nested structures, but for now I can't think of an    *)
    (* elegant way to do this.                                          *)

    VAR pdata: ByteStringPtr;
        amount: CARDINAL;

    CONST certificate_code = 11;

    BEGIN
        AssembleOurCertificates (amount, pdata);
        RecordTraceOn;
        SendHandshake (sess, certificate_code, pdata, amount);
        RecordTraceOff;
    END SendServerCertificate;

(************************************************************************)
(*                 HANDSHAKING ON INITIAL CONNECTION                    *)
(************************************************************************)

PROCEDURE ReceivePartnerHandshake (sess: TLSsession;
                                    VAR (*OUT*) done: BOOLEAN): BOOLEAN;

    VAR type, descr: CARD8;  success: BOOLEAN;
        j, length: CARDINAL;
        buffer: TextType;
        message: ARRAY [0..127] OF CHAR;
        modulus, exponent: BN;

    (* A handshake record starts with code 22 to mean "handshake", then *)
    (* we have a two-byte version code and a two-byte length.  Those    *)
    (* initial five bytes are loaded into the appropriate fields of     *)
    (* buffer by procedure GetFragment.  After this we have the         *)
    (* "fragment" field with                                            *)
    (*      HandshakeType (1 byte)                                      *)
    (*      length        (3 bytes)                                     *)
    (*      content       (depends on handshake type)                   *)

    (* The expected sequence, as seen by the client end, is:            *)
    (*      send client Hello                                           *)
    (*      receive server Hello                                        *)
    (*      (maybe) receive server certificate                          *)
    (*      (maybe) receive server key exchange                         *)
    (*      (maybe) receive certificate request from server             *)
    (*      receive server HelloDone                                    *)
    (*      (maybe) send client certificate                             *)
    (*      send client KeyExchange                                     *)
    (*      (maybe) send CertificateVerify                              *)
    (*      send ChangeCipherSpec (not a handshake message)             *)
    (*      send Finished                                               *)
    (*      receive ChangeCipherSpec                                    *)
    (*      receive Finished                                            *)
    (* After this, application data can flow.                           *)

    BEGIN
        done := FALSE;
        (*RecordTraceOn (sess^.logID);*)
        success := GetFragment (sess^.rlstate, sess^.logID, buffer);
        RecordTraceOff;
        length := Swap2 (buffer.length);

        IF buffer.contenttype = 20 THEN

            (* Change cipher spec. *)

            NYI (sess^.logID, "change_cipher_spec");
            success := FALSE;

        ELSIF buffer.contenttype = 21 THEN

            (* Alert *)

            LogAlert (sess^.logID, "In ReceivePartnerHandshake, ", buffer);

        ELSIF buffer.contenttype = 22 THEN

            (* Handshake *)

            IF buffer.fragment = NIL THEN
                type := 0;
                length := 0;
            ELSE
                type := buffer.fragment^[0];
    
                (* Interpret the three-byte length. *)
    
                j := 1;
                length := Get3Bytes (buffer.fragment^, j);
            END (*IF*);

            CASE type OF

                | 1:    (* Client Hello received, so this is server code. *)

                        (*
                        message := "Just before calling HandleClientHello, length = ";
                        AppendCard (length, message);
                        LogTransaction (sess^.logID, message);
                        *)
                        success := HandleClientHello (sess, buffer.fragment, 4, length);

                        LogTransactionL (sess^.logID, "Sending server hello");
                        SendServerHello (sess);
                        LogTransactionL (sess^.logID, "Sending server certificate");
                        SendServerCertificate (sess);
                        LogTransactionL (sess^.logID, "Sending hello done");
                        SendServerHelloDone (sess);

                        (* At this stage we expect the client   *)
                        (* to send more handshake records:      *)
                        (*  certificate (optional)              *)
                        (*  client key exchange                 *)
                        (*  certificate verify (optional)       *)
                        (*  change cipher spec                  *)
                        (* (That last one is not a handshake    *)
                        (* message, but a different content     *)
                        (* type.)                               *)
                        (* Then we expect a "finished"          *)
                        (* handshake message.                   *)

                        REPEAT
                            success := ReceivePartnerHandshake (sess, done);
                        UNTIL done OR NOT success;

                |  2:   (* Server hello *)

                        success := ReceiveServerHello (sess, buffer.fragment, 4, length);

                (*  3..10 do not appear in RFC 5246 *)

                | 11:   (* Certificate *)

                        success := AcceptCertificates (sess^.logID,
                                      buffer.fragment, 4, length, modulus, exponent);

                (* 12 = server key exchange, not implemented here. *)
                (* 13 = certificate request, not implemented here. *)

                | 14:   (* Server Hello done *)

                        LogTransactionL (sess^.logID, "server_hello_done received");
                        success := ReceiveServerHelloDone (sess);
                        done := TRUE;

                (* 15 = certificate verify, not implemented here. *)

                | 16:
                        success := HandleClientKeyExchange(sess, buffer.fragment, 4, length);

                (* 20 = finished, which I definitely need to implement. *)

                |
                ELSE
                        WriteString ("Unhandled handshake type");  WriteLn;
                        WriteCard (type);  WriteLn;
                        success := FALSE;
            END (*CASE*);
        ELSE
            Strings.Assign ("Received record, type ", message);
            AppendCard (buffer.contenttype, message);
            Strings.Append (", is not a handshake record", message);
            LogTransaction (sess^.logID, message);
            success := FALSE;
        END (*IF*);

        IF NOT success THEN
            DiscardFragment (buffer);
        END (*IF*);
        RETURN success;

    END ReceivePartnerHandshake;

(************************************************************************)

PROCEDURE DoHandshake (sess: TLSsession): BOOLEAN;

    (* A handshake record starts with code 22 (16H) to mean "handshake",*)
    (* then we have a two-byte version code and a two-byte length.      *)
    (* Those initial five bytes are loaded into the appropriate fields  *)
    (* of buffer by procedure GetFragment.  After this we have the      *)
    (* "fragment" field with                                            *)
    (*      HandshakeType (1 byte)                                      *)
    (*      length        (3 bytes)                                     *)
    (*      content       (depends on handshake type)                   *)

    (* The expected sequence, as seen by the server end, is:            *)
    (*      receive client Hello                                        *)
    (*      send server Hello                                           *)
    (*      (maybe) send server certificate                             *)
    (*      (maybe) send server key exchange                            *)
    (*      (maybe) send certificate request from server                *)
    (*      send server HelloDone                                       *)
    (*      (maybe) receive client certificate                          *)
    (*      receive client KeyExchange                                  *)
    (*      (maybe) receive CertificateVerify                           *)
    (*      receive ChangeCipherSpec (not a handshake message)          *)
    (*      receive Finished                                            *)
    (*      send ChangeCipherSpec                                       *)
    (*      send Finished                                               *)
    (* After this, application data can flow.                           *)

    (* From the viewpoint of the client, this is:                       *)
    (*      send client Hello                                           *)
    (*      receive server Hello                                        *)
    (*      (maybe) receive server certificate                          *)
    (*      (maybe) receive server key exchange                         *)
    (*      (maybe) receive certificate request from server             *)
    (*      receive server HelloDone                                    *)
    (*      (maybe) send client certificate                             *)
    (*      send client KeyExchange                                     *)
    (*      (maybe) send CertificateVerify                              *)
    (*      send ChangeCipherSpec (not a handshake message)             *)
    (*      send Finished                                               *)
    (*      receive ChangeCipherSpec                                    *)
    (*      receive Finished                                            *)
    (* After this, application data can flow.                           *)

    VAR done, success: BOOLEAN;

    BEGIN
        done := FALSE;
        IF sess^.IsServer THEN
            success := TRUE;
        ELSE
            success := SendClientHello (sess);
        END (*IF*);
        WHILE success AND NOT done DO
            success := ReceivePartnerHandshake (sess, done);
        END (*WHILE*);
        RETURN success;
    END DoHandshake;

(************************************************************************)

BEGIN
    CreateLock (NextIDLock);
    ResetNextID;
END TLS.


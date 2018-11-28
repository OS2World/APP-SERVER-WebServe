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

IMPLEMENTATION MODULE Certificates;

        (********************************************************)
        (*                                                      *)
        (*                   TLS certificates                   *)
        (*      For now I'm handling only RSA certificates      *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            5 September 2018                *)
        (*  Last edited:        16 October 2018                 *)
        (*  Status:             Partly done                     *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT LOC, CARD8, ADR;

FROM STextIO IMPORT
    (* proc *)  WriteString, WriteLn;

FROM ASN1 IMPORT
    (* type *)  ASN1part,
    (* proc *)  ExtractParts, INTEGERtoBN, BitStringToBN, GetObjID;

IMPORT Strings, BigNum;

FROM VarStrings IMPORT
    (* type *)  ByteStringPtr;

FROM TLSBase IMPORT
    (* proc *)  NYI, Get3Bytes, Put3Bytes, Checksize, Resize, AppendHexString;

FROM BigNum IMPORT
    (* type *)  BN;

FROM MiscFuncs IMPORT
    (* proc *)  AppendCard;

FROM FileOps IMPORT
    (* type *)  ChanId,
    (* proc *)  OpenOldFile, OpenNewFile, CloseFile, ReadRaw, WriteRaw, ReadLine,
                Exists, DeleteFile;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  LogTransaction;

FROM LowLevel IMPORT
    (* proc *)  EVAL, Copy, AddOffset;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)
(* In this module we represent a certificate or a certificate chain by  *)
(* a byte string and the length of that string.  The first ten bytes    *)
(* are overhead, as follows:                                            *)
(*      (a) first 4 bytes   reserved for handshake header               *)
(*      (b) next 3 bytes    total remaining length                      *)
(*      (c) next 3 bytes    length of the certificate itself            *)
(* In the present implementation the certificate chain is only one      *)
(* certificate long, therefore (b) is just (c) plus 3, and the string   *)
(* length is (b) plus 7.  We accept this redundancy since the caller    *)
(* need only know about the total length.  Also, (b) will change once   *)
(* we allow chains of more than one certificate.                        *)
(************************************************************************)

CONST
    CtrlZ = CHR(26);

(* From RFC 3280, page 15. *)
(*
The X.509 v3 certificate basic syntax is as follows. For signature
calculation, the data that is to be signed is encoded using the ASN.1
distinguished encoding rules (DER) [X.690]. ASN.1 DER encoding is a
tag, length, value encoding system for each element. *)

(*
Certificate ::= SEQUENCE {
                         tbsCertificate: TBSCertificate,
                         signatureAlgorithm: AlgorithmIdentifier,
                         signatureValue: BIT STRING }
TBSCertificate ::= SEQUENCE {
                            version: [0] EXPLICIT Version DEFAULT v1,
                            serialNumber: CertificateSerialNumber,
                            signature : AlgorithmIdentifier,
                            issuer: Name,
                            validity: Validity,
                            subject: Name,
                            subjectPublicKeyInfo: SubjectPublicKeyInfo,
                            issuerUniqueID: [1] IMPLICIT UniqueIdentifier OPTIONAL,
                            -- If present, version MUST be v2 or v3
                            subjectUniqueID: [2] IMPLICIT UniqueIdentifier OPTIONAL,
                            -- If present, version MUST be v2 or v3
                            extensions: [3] EXPLICIT Extensions OPTIONAL
                            -- If present, version MUST be v3
                            }
Version ::= INTEGER {v1(0), v2(1), v3(2)}
CertificateSerialNumber ::= INTEGER
Validity ::= SEQUENCE {
                      notBefore: Time,
                      notAfter: Time }
Time ::= CHOICE {
                utcTime: UTCTime,
                generalTime: GeneralizedTime }
UniqueIdentifier ::= BIT STRING
SubjectPublicKeyInfo ::= SEQUENCE {
                                  algorithm: AlgorithmIdentifier,
                                  subjectPublicKey: BIT STRING }
Extensions ::= SEQUENCE SIZE (1..MAX) OF Extension
Extension ::= SEQUENCE {
                       extnID: OBJECT IDENTIFIER,
                       critical: BOOLEAN DEFAULT FALSE,
                       extnValue: OCTET STRING
                       }
*)

(************************************************************************)
(*                              TEST CODE                               *)
(************************************************************************)

PROCEDURE DumpASN (N: CARDINAL;  length: CARDINAL;  p: ByteStringPtr);

    VAR cid: ChanId;
        fname: ARRAY [0..15] OF CHAR;

    BEGIN
        fname := "ASNpart";
        fname[7] := CHR(N+ORD('0'));
        fname[8] := CHR(0);
        Strings.Append (".bin", fname);
        IF Exists (fname) THEN
            DeleteFile(fname);
        END (*IF*);
        cid := OpenNewFile (fname, TRUE);
        WriteRaw (cid, p^, length);
        CloseFile (cid);
    END DumpASN;

(************************************************************************)
(*                       RECEIVING A CERTIFICATE                        *)
(************************************************************************)

PROCEDURE AcceptCertificate (logID: TransactionLogID;
                        pdata: ByteStringPtr;  offset, length: CARDINAL;
                        VAR (*OUT*) modulus, exponent: BN);

    (* Accepts a certificate sent by the peer.  The returned modulus    *)
    (* and exponent are the public key.                                 *)

    CONST BNbufsize = 600;

    VAR message: ARRAY [0..800] OF CHAR;
        BNbuf: ARRAY [0..BNbufsize-1] OF CHAR;
        keybuff: ARRAY [0..4095] OF CARD8;
        part: ARRAY [0..31] OF ASN1part;
        pcert, pk: ByteStringPtr;
        j, N: CARDINAL;
        V: BN;
        values: ARRAY [0..15] OF CARDINAL;

    BEGIN
        length := Get3Bytes (pdata^, offset);
        message := "Length of certificate is ";
        AppendCard (length, message);
        LogTransaction (logID, message);

        (*
        message := "First few bytes of the certificate are ";
        AppendHexString (pdata^, offset, 16, TRUE, message);
        LogTransaction (logID, message);
        *)
        pcert := AddOffset (pdata, offset);
        N := ExtractParts (pcert^, part);

        (* There should be three parts. For testing, dump those 3 parts.*)

        IF N = 3 THEN
            FOR j := 0 TO N-1 DO
                DumpASN (j+1, part[j].length, part[j].val);
            END (*FOR*);
        ELSE
            message := "certificate has ";
            AppendCard (N, message);
            Strings.Append (" parts.", message);
            LogTransaction (logID, message);
        END (*IF*);

        (* Part 2 is signature value. *)

        message := "Signature is ";
        V := BitStringToBN (part[2].val^, 0);
        BigNum.ToHex (V, BNbuf, BNbufsize);
        Strings.Append (BNbuf, message);
        LogTransaction (logID, message);

        (* Part 0 is the certificate itself.  Break that into parts.  *)

        N := ExtractParts (part[0].val^, part);
        message := "Body of certificate has ";
        AppendCard (N, message);
        Strings.Append (" parts.", message);
        LogTransaction (logID, message);

        (* For now I'm only interested in part 6, which is the public   *)
        (* key.  It has further substructure, as shown below.           *)

        EVAL (ExtractParts (part[6].val^, part));
        pk := part[1].val;         (* Save part 1 for later. *)

        (* part[0] contains the object identifier for the encryption    *)
        (* method.  For now I'm assuming RSA encryption, whose object   *)
        (* ID is {1 2 840 113549 1 1 1}.  In a future version I'll have *)
        (* to check this.                                               *)

        EVAL (ExtractParts (part[0].val^, part));
        N := GetObjID (part[0].val^, 0, values);
        message := "Encryption = ";
        IF N = 0 THEN
            Strings.Append ("<missing>", message);
        ELSE
            Strings.Append ("{", message);
            FOR j := 0 TO N-1 DO
                IF j > 0 THEN
                    Strings.Append (" ", message);
                END (*IF*);
                AppendCard (values[j], message);
            END (*FOR*);
            Strings.Append ("}", message);
        END (*IF*);
        LogTransaction (logID, message);

        (* pk^ is the public key, which is itself an ASN.1 sequence of  *)
        (* modulus and exponent.  So we have one extra step of ASN.1    *)
        (* decoding.                                                    *)

        V := BitStringToBN (pk^, 0);
        (*
        message := "Public key = ";
        BigNum.ToHex (V, BNbuf, BNbufsize);
        Strings.Append (BNbuf, message);
        LogTransaction (logID, message);
        *)

        EVAL (BigNum.BNtoBytes (V, keybuff));
        EVAL (ExtractParts (keybuff, part));

        message := "Public key modulus = ";
        modulus := INTEGERtoBN (part[0].val^, 0);
        BigNum.ToHex (modulus, BNbuf, BNbufsize);
        Strings.Append (BNbuf, message);
        LogTransaction (logID, message);

        message := "Public key exponent = ";
        exponent := INTEGERtoBN (part[1].val^, 0);
        BigNum.ToHex (exponent, BNbuf, BNbufsize);
        Strings.Append (BNbuf, message);
        LogTransaction (logID, message);

    END AcceptCertificate;

(************************************************************************)

PROCEDURE AcceptCertificates (logID: TransactionLogID;
                        pdata: ByteStringPtr;  offset, length: CARDINAL;
                        VAR (*OUT*) modulus, exponent: BN): BOOLEAN;

    (* Accepts a chain of certificates sent by the peer.  (But, in this *)
    (* version, we only look at the first one.)  The returned modulus   *)
    (* and exponent are the public key.                                 *)

    VAR message: ARRAY [0..127] OF CHAR;

    BEGIN
        (*
        message := "In AcceptCertificates, first few bytes are ";
        AppendHexString (pdata^, offset, 16, TRUE, message);
        LogTransaction (logID, message);
        *)
        length := Get3Bytes (pdata^, offset);
        (*
        message := "Length of certificates is ";
        AppendCard (length, message);
        LogTransaction (logID, message);
        *)
        AcceptCertificate (logID, pdata, offset, length, modulus, exponent);
        RETURN TRUE;
    END AcceptCertificates;

(************************************************************************)
(*              PREPARING A CERTIFICATE FOR TRANSMISSION                *)
(************************************************************************)

PROCEDURE AssembleOurCertificates (VAR (*OUT*) length: CARDINAL;
                                    VAR (*OUT*) pdata: ByteStringPtr);

    (* Prepares the certificate data for transmission.  In this version *)
    (* I use a test certificate with a fixed filename, and there is     *)
    (* only one certificate in the chain.                               *)

    (* The required format is a 3-byte total length; then, for each     *)
    (* certificate, a 3-byte length followed by the certificate itself. *)
    (* That suggests that I need to define a new data type that allows  *)
    (* linking of nested structures, but for now I can't think of an    *)
    (* elegant way to do this.                                          *)

    CONST
        filename = "NPC.BIN";
        binary = TRUE;
        certificate_code = 11;
        overhead = 10;
        InitialAllocation = 256;

    VAR line: ARRAY [0..511] OF CHAR;
        q: ByteStringPtr;
        nread, certlength, allocated: CARDINAL;
        cid: ChanId;

    BEGIN
        allocated := InitialAllocation;
        ALLOCATE (pdata, allocated);
        length := overhead; (* First 4 bytes will be filled in by the   *)
                            (* caller.  The following 6 bytes will be   *)
                            (* filled in here once we know the length.  *)

        cid := OpenOldFile (filename, FALSE, binary);
        LOOP
            IF binary THEN
                ReadRaw (cid, line, 512, nread);
            ELSE
                REPEAT
                    ReadLine (cid, line);
                UNTIL (line[0] <> '-') OR (line[1] <> '-') OR (line[2] <> '-');
                IF line[0] = CtrlZ THEN
                    nread := 0;
                ELSE
                    nread := Strings.Length (line);
                END (*IF*);
            END (*IF*);
            IF nread = 0 THEN
                EXIT (*LOOP*);
            ELSE
                Checksize (pdata, allocated, length, nread);
                Copy (ADR(line), ADR(pdata^[length]), nread);
                INC (length, nread);
            END (*IF*);
        END (*LOOP*);
        CloseFile (cid);
        Resize (pdata, allocated, length);
        certlength := length - overhead;
        q := AddOffset (pdata, 4);
        Put3Bytes (q^, certlength+3);
        q := AddOffset (q, 3);
        Put3Bytes (q^, certlength);
        (*DEC (length, 4);*)

        WriteString ("Sending certificate ");  WriteString (filename);  WriteLn;

    END AssembleOurCertificates;

(************************************************************************)

END Certificates.


(**************************************************************************)
(*                                                                        *)
(*  TLS module                                                            *)
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

IMPLEMENTATION MODULE CipherSuites;

        (********************************************************)
        (*                                                      *)
        (*  Interface between TLS and the encryption algorithms *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            27 August 2018                  *)
        (*  Last edited:        27 August 2018                  *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT CARD8;

IMPORT Strings;

(************************************************************************)

TYPE
    BulkCipherAlgorithm = (nocipher, rc4_128, tripledes, aes128, aes256);
    PRFAlgorithm = (tls_prf_sha256);
    KeyExchangeAlgorithm = (nullkeyx, RSA);
    (*CipherType = (stream, block, aead);*)
    MACAlgorithm = (null, hmac_md5, hmac_sha1, hmac_sha256, hmac_sha384,
                                 hmac_sha512);

    (* See RFC 5246, Appendix C, for how these decompose into three     *)
    (* critical properties: key exchange, bulk cipher, and MAC.  There  *)
    (* is obviously extensive use of suites that are not mentioned in   *)
    (* RFC 5246, and for those I am relying on web sources.             *)

    SuitePropertyRecord =
            RECORD
                supported       : BOOLEAN;
                code1, code2    : CARD8;
                keyx            : KeyExchangeAlgorithm;
                cipher          : BulkCipherAlgorithm;
                mac             : MACAlgorithm;
            END (*RECORD*);

    PropertyList = ARRAY CipherSuite OF SuitePropertyRecord;

CONST
    SuiteProperty = PropertyList
            {
            {TRUE,  0, 0, nullkeyx, nocipher, null},
            {FALSE, 0, 1, RSA, nocipher, hmac_md5},
            {FALSE, 0, 2, RSA, nocipher, hmac_sha1},
            {FALSE, 0, 3BH, RSA, nocipher, hmac_sha256},
            {FALSE, 0, 4, RSA, rc4_128, hmac_md5},
            {FALSE, 0, 5, RSA, rc4_128, hmac_sha1},
            {FALSE, 0, 0AH, RSA, tripledes, hmac_sha1},
            {TRUE,  0, 2FH, RSA, aes128, hmac_sha1},
            {FALSE, 0, 35H, RSA, aes256, hmac_sha1},
            {FALSE, 0, 3CH, RSA, aes128, hmac_sha256},
            {FALSE, 0, 3DH, RSA, aes256, hmac_sha256}
            };

    (* The list of suites I get from OS/2 Firefox, in preference order, is
        00 1E       TLS_KRB5_WITH_DES_CBC_SHA (not implemented)
        C0 2B       TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256 (not implemented)
        C0 2F       TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256 (not implemented)
        C0 0A       TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA (not implemented)
        C0 09       TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA (not implemented)
        C0 13       TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA (not implemented)
        C0 14       TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA (not implemented)
        C0 07       TLS_ECDHE_ECDSA_WITH_RC4_128_SHA (not implemented)
        C0 11       TLS_ECDHE_RSA_WITH_RC4_128_SHA (not implemented)
        00 33       TLS_DHE_RSA_WITH_AES_128_CBC_SHA (not implemented)
        00 39       TLS_DHE_RSA_WITH_AES_256_CBC_SHA (not implemented)
        00 2F       TLS_RSA_WITH_AES_128_CBC_SHA (done)
        00 35       TLS_RSA_WITH_AES_256_CBC_SHA (not implemented, but I should consider this one)
        00 0A       TLS_RSA_WITH_3DES_EDE_CBC_SHA

    Clearly there is a disconnect between what I have implemented and
    what Firefox prefers.  To be compatible, I need to do one or more
    of the following:
      - implement the CBC version of AES; DONE;
      - implement RC4 - DONE;
      - look into ECDHE - probably hard, but worth a look.
      - implement 3DES - DONE.
    *)

(************************************************************************)
(*    TRANSLATING BETWEEN DIFFERENT REPRESENTATIONS OF A CipherSuite    *)
(************************************************************************)

PROCEDURE NameOfSuite (s: CipherSuite;  VAR (*OUT*) name: ARRAY OF CHAR);

    (* Returns name as the name of ciphersuite s. *)

    TYPE SNtype = ARRAY CipherSuite OF ARRAY [0..32] OF CHAR;

    CONST
        SuiteName = SNtype {"TLS_NULL_WITH_NULL_NULL",
                            "TLS_RSA_WITH_NULL_MD5",
                            "TLS_RSA_WITH_NULL_SHA",
                            "TLS_RSA_WITH_NULL_SHA256",
                            "TLS_RSA_WITH_RC4_128_MD5",
                            "TLS_RSA_WITH_RC4_128_SHA",
                            "TLS_RSA_WITH_3DES_EDE_CBC_SHA",
                            "TLS_RSA_WITH_AES_128_CBC_SHA",
                            "TLS_RSA_WITH_AES_256_CBC_SHA",
                            "TLS_RSA_WITH_AES_128_CBC_SHA256",
                            "TLS_RSA_WITH_AES_256_CBC_SHA256"
                            };

    BEGIN
        Strings.Assign (SuiteName[s], name);
    END NameOfSuite;

(************************************************************************)

PROCEDURE CodeToCipherSuite (code1, code2: CARD8): CipherSuite;

    (* Translates two-byte code to a CipherSuite. *)

    VAR j: CipherSuite;

    BEGIN
        j := NullSuite;
        WHILE j < MAX(CipherSuite) DO
            INC (j);
            IF (SuiteProperty[j].code1 = code1)
                    AND (SuiteProperty[j].code2 = code2) THEN
                RETURN j;
            END (*IF*);
        END (*LOOP*);
        RETURN NullSuite;
    END CodeToCipherSuite;

(************************************************************************)

PROCEDURE CipherSuiteToCode (s: CipherSuite;  VAR (*OUT*) code1, code2: CARD8);

    (* Returns the two-byte code for s. *)

    BEGIN
        code1 := SuiteProperty[s].code1;
        code2 := SuiteProperty[s].code2;
    END CipherSuiteToCode;

(************************************************************************)
(*                       ACCEPTING A CIPHER SUITE                       *)
(************************************************************************)

PROCEDURE AcceptCipherSuite (code1, code2: CARD8): CipherSuite;

    (* Returns result <> NullSuite iff we support this cipher suite. *)

    VAR suite: CipherSuite;

    BEGIN
        suite := CodeToCipherSuite (code1, code2);
        IF SuiteProperty[suite].supported THEN
            RETURN suite;
        ELSE
            RETURN NullSuite;
        END (*IF*);
    END AcceptCipherSuite;

(************************************************************************)

END CipherSuites.


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

IMPLEMENTATION MODULE TLSCrypt;

        (********************************************************)
        (*                                                      *)
        (*  Interface between TLS and the encryption algorithms *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            5 April 2018                    *)
        (*  Last edited:        3 October 2018                  *)
        (*  Status:             Just starting                   *)
        (*                                                      *)
        (********************************************************)


FROM VarStrings IMPORT
    (* type *)  ByteStringPtr;

FROM TLSBase IMPORT
    (* proc *)  NYI;

FROM TransLog IMPORT
    (* type *)  TransactionLogID;

FROM Storage IMPORT
    (* type *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

TYPE
    BulkCipherAlgorithm = (nocipher, rc4_128, tripledes, aes128, aes256);

    EncrRule = POINTER TO
                    RECORD
                        bulkcipher: BulkCipherAlgorithm;
                    END (*RECORD*);

    (* See RFC 5246, Appendix C, for how these decompose into three     *)
    (* critical properties: key exchange, bulk cipher, and MAC.  There  *)
    (* is obviously extensive use of suites that are not mentioned in   *)
    (* RFC 5246, and for those I am relying on web sources.             *)

(************************************************************************)
(*                             INITIALISATION                           *)
(************************************************************************)

PROCEDURE InitEncryptionRule (): EncrRule;

    (* Creates an initial "no encryption" state. *)

    VAR result: EncrRule;

    BEGIN
        NEW (result);
        result^.bulkcipher := nocipher;
        RETURN result;
    END InitEncryptionRule;

(************************************************************************)

PROCEDURE CloseEncryptionRule (VAR (*INOUT*) ER: EncrRule);

    (* Discards the EncrRule data. *)

    BEGIN
        DISPOSE (ER);
    END CloseEncryptionRule;

(************************************************************************)
(*                               ENCRYPTION                             *)
(************************************************************************)

PROCEDURE Encrypt (rule: EncrRule;  logID: TransactionLogID;
                   VAR (*INOUT*) message: ByteStringPtr;  length: CARDINAL)
                                                               : CARDINAL;

    (* The message is replaced by its encrypted version.  The returned  *)
    (* value is the new length.                                         *)

    BEGIN
        IF (rule <> NIL) AND (rule^.bulkcipher <> nocipher) THEN
            NYI (logID, "Encrypt");
        END (*IF*);
        RETURN length;
    END Encrypt;

(************************************************************************)

END TLSCrypt.


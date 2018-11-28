(**************************************************************************)
(*                                                                        *)
(*  PMOS/2 software library                                               *)
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

IMPLEMENTATION MODULE BigNum;

        (********************************************************)
        (*                                                      *)
        (*           Support for multibit integers              *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            12 November 2017                *)
        (*  Last edited:        16 October 2018                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT CARD8, CARD32;

IMPORT Arith64;

FROM RandCard IMPORT
    (* proc *)  Randomize, RandCardinal;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  Copy, IAND, LS, RS;

FROM OS2 IMPORT
    (* const*)  SEM_INDEFINITE_WAIT,
    (* type *)  HMTX,
    (* proc *)  DosCreateMutexSem, DosRequestMutexSem, DosReleaseMutexSem;

FROM STextIO IMPORT
    (* proc *)  WriteChar, WriteString, WriteLn;

(************************************************************************)

TYPE
    DataSubscript = [0..MAX(CARDINAL) DIV 16];
    DataPtr = POINTER TO ARRAY DataSubscript OF CARD32;

    BNptr = POINTER TO BNrecord;
    BNrecord  = RECORD
                    negative: BOOLEAN;
                    next: BNptr;
                    nwords: CARDINAL;
                    val: DataPtr;
                END (*RECORD*);
    BN = BNptr;

    (* A BN is stored as an array of nwords CARD32 words, in        *)
    (* little-endian order.  The array size is allowed to expand and    *)
    (* contract as the calculations proceed.  We use a sign-magnitude   *)
    (* representation, because twos complement arithmetic is harder to  *)
    (* do when the number of bits is not fixed.  When one of the        *)
    (* operations in this module is labelled "unsigned", that means     *)
    (* that that operation either works only for nonnegative arguments, *)
    (* or that it ignores the "negative" field and works on the         *)
    (* magnitude alone.  The next field is for memory leak checks.      *)

CONST
    bitsperword = 32;
    wordsize = SIZE(CARD32);
    topbit = 80000000H;

VAR
    (* Information about memory usage.  We keep a list of all Bignums,  *)
    (* the amount of memory they are using, so we can check for leaks.  *)

    Usage:  RECORD
                access: HMTX;
                listhead: BNptr;
                BNcount, bytecount: CARDINAL;
            END (*RECORD*);

(************************************************************************)
(*                         TRACKING MEMORY USAGE                        *)
(************************************************************************)

PROCEDURE LockList;

    (* Get exclusive access to Usage. *)

    BEGIN
        DosRequestMutexSem (Usage.access, SEM_INDEFINITE_WAIT);
    END LockList;

(************************************************************************)

PROCEDURE UnlockList;

    (* Relinquish exclusive access to Usage. *)

    BEGIN
        DosReleaseMutexSem (Usage.access);
    END UnlockList;

(************************************************************************)

PROCEDURE WriteCard (N: CARDINAL);

    (* Writes N in decimal to standard output. *)

    BEGIN
        IF N > 9 THEN
            WriteCard (N DIV 10);
            N := N MOD 10;
        END (*IF*);
        WriteChar (CHR(N + ORD('0')));
    END WriteCard;

(************************************************************************)

PROCEDURE ShowBNUsage;

    (* Reports how much memory is being used by Bignums.  *)

    BEGIN
        LockList;
        WriteString ("BN usage: ");
        WriteCard (Usage.BNcount);
        WriteString (" Bignums, ");
        WriteCard (Usage.bytecount);
        WriteString (" bytes.");
        WriteLn;
        UnlockList;
    END ShowBNUsage;

(************************************************************************)

PROCEDURE AddToList (A: BN);

    (* Records that A is using memory. *)

    BEGIN
        LockList;
        A^.next := Usage.listhead;
        Usage.listhead := A;
        INC (Usage.BNcount);
        INC (Usage.bytecount, SIZE(BNrecord) + wordsize * A^.nwords);
        UnlockList;
    END AddToList;

(************************************************************************)

PROCEDURE RemoveFromList (A: BN);

    (* Records that A is no longer using memory. *)

    VAR previous, p: BNptr;

    BEGIN
        LockList;

        (* Find A in the list. *)

        previous := NIL;  p := Usage.listhead;
        WHILE (p <> NIL) AND (p <> A) DO
            previous := p;  p := p^.next;
        END (*WHILE*);
        IF p = NIL THEN
            WriteString ("BIGNUM ERROR: Cannot remove element from list.");
            WriteLn;
        ELSE
            IF previous = NIL THEN
                Usage.listhead := p^.next;
            ELSE
                previous^.next := p^.next;
            END (*IF*);
            DEC (Usage.BNcount);
            DEC (Usage.bytecount, SIZE(BNrecord) + wordsize * A^.nwords);
        END (*IF*);
        UnlockList;
    END RemoveFromList;

(************************************************************************)
(*                           OUTPUT FUNCTIONS                           *)
(************************************************************************)

PROCEDURE WriteBignum (A: BN);

    (* Writes N in decimal to standard output. *)

    VAR buffer: ARRAY [0..2047] OF CHAR;
        pos: CARDINAL;

    BEGIN
        pos := 0;
        ToDecimal (A, buffer, pos);
        WriteString (buffer);
    END WriteBignum;

(************************************************************************)
(*                   CREATING AND DESTROYING BIGNUMS                    *)
(************************************************************************)

PROCEDURE AddShortU (A: BN;  B: CARD32);  FORWARD;          (* UNSIGNED *)
PROCEDURE LSBN (VAR (*INOUT*) val: BN;  N: CARDINAL);  FORWARD;

(************************************************************************)

PROCEDURE BinToBN (VAR (*IN*) bindata: ARRAY OF CARD8;  pos, N: CARDINAL): BN;

    (* Converts a string of N CARD8 values, starting at bindata[pos],   *)
    (* to a Bignum.                                                     *)

    VAR V: BN;
        j: CARDINAL;

    BEGIN
        V := Zero();
        FOR j := 1 TO N DO
            LSBN (V, 8);
            AddShortU (V, bindata[pos]);  INC (pos);
        END (*FOR*);
        RETURN V;
    END BinToBN;

(************************************************************************)

PROCEDURE BNtoBytes (V: BN;  VAR (*OUT*) result: ARRAY OF CARD8): CARDINAL;

    (* Converts an unsigned Bignum to an array of CARD8 values, in      *)
    (* BigEndian order with no leading zero bytes.  The function result *)
    (* is the number of bytes in the result.                            *)
    (* Warning: for a negative number we convert only the magnitude.    *)

    VAR pos: CARDINAL;

    (********************************************************************)

    PROCEDURE Card32toBytes (A, N: CARDINAL;  suppresszero: BOOLEAN);

        (* Converts A to an N-byte value at result[pos], updates pos.   *)
        (* Special case: if suppresszero = TRUE then we suppress        *)
        (* leading zeroes, so might produce fewer than N bytes.         *)

        BEGIN
            IF (A > 0) OR NOT suppresszero THEN
                IF N > 1 THEN
                    Card32toBytes (A DIV 256, N-1, suppresszero);
                END (*IF*);
                result[pos] := A MOD 256;  INC(pos);
            END (*IF*);
        END Card32toBytes;

    (********************************************************************)

    VAR j, NW: CARDINAL;

    BEGIN
        pos := 0;
        NW := V^.nwords;
        IF (NW = 1) AND (V^.val^[0] = 0) THEN
            (* Special case: V is zero. *)
            result[0] := 0;  pos := 1;
        ELSE
            Card32toBytes (V^.val^[NW-1], 4, TRUE);
            IF NW > 1 THEN
                FOR j := NW-2 TO 0 BY -1 DO
                    Card32toBytes (V^.val^[j], 4, FALSE);
                END (*FOR*);
            END (*IF*);
        END (*IF*);
        RETURN pos;
    END BNtoBytes;

(************************************************************************)

PROCEDURE MakeBignum (N: INTEGER): BN;

    (* Converts integer to BN. *)

    VAR result: BN;

    BEGIN
        NEW (result);
        IF N < 0 THEN
            result^.negative := TRUE;
            N := -N;
        ELSE
            result^.negative := FALSE;
        END (*IF*);
        result^.nwords := 1;
        ALLOCATE (result^.val, wordsize);
        result^.val^[0] := N;
        AddToList (result);
        RETURN result;
    END MakeBignum;

(************************************************************************)

PROCEDURE Zero(): BN;

    (* Returns a BigNum with value zero.  *)

    VAR result: BN;

    BEGIN
        NEW (result);
        result^.negative := FALSE;
        result^.nwords := 1;
        ALLOCATE (result^.val, wordsize);
        result^.val^[0] := 0;
        AddToList (result);
        RETURN result;
    END Zero;

(************************************************************************)

PROCEDURE Discard (VAR (*INOUT*) num: BN);

    (* Deallocates the storage used by num. *)

    BEGIN
        IF num <> NIL THEN
            RemoveFromList (num);
            DEALLOCATE (num^.val, num^.nwords*wordsize);
            DISPOSE (num);
        END (*IF*);
    END Discard;

(************************************************************************)
(*                          SIMPLE OPERATIONS                           *)
(************************************************************************)

PROCEDURE Card (num: BN): CARDINAL;

    (* Converts BN to cardinal, saturating result if out of range. *)

    BEGIN
        IF num^.negative THEN RETURN 0;
        ELSIF num^.nwords > 1 THEN RETURN MAX(CARDINAL);
        ELSE RETURN num^.val^[0];
        END (*IF*);
    END Card;

(************************************************************************)

PROCEDURE CopyBN (A: BN): BN;

    (* Returns a copy of A. *)

    VAR amount: CARDINAL;
        result: BN;

    BEGIN
        NEW (result);
        IF result = NIL THEN
            WriteLn;
            WriteString ("OUT OF MEMORY");
            WriteLn;
        END (*IF*);
        result^.negative := A^.negative;
        amount := A^.nwords;
        result^.nwords := amount;
        amount := wordsize*amount;
        ALLOCATE (result^.val, amount);
        Copy (A^.val, result^.val, amount);
        AddToList (result);
        RETURN result;
    END CopyBN;

(************************************************************************)

PROCEDURE ChangeSize (A: BN;  newsize: CARDINAL);

    (* Alters the data size of A to newsize words.  It is the caller's  *)
    (* responsibility to ensure that no data are lost.                  *)

    VAR oldsize, j, tocopy: CARDINAL;
        p: DataPtr;  bigger: BOOLEAN;

    BEGIN
        oldsize := A^.nwords;
        bigger := newsize > oldsize;
        IF newsize <> oldsize THEN
            IF bigger THEN
                tocopy := oldsize;
            ELSE
                tocopy := newsize;
            END (*IF*);
            tocopy := wordsize * tocopy;
            IF newsize = 0 THEN
                p := NIL;
            ELSE
                ALLOCATE (p, newsize*wordsize);
            END (*IF*);
            IF tocopy > 0 THEN
                Copy (A^.val, p, tocopy);
            END (*IF*);
            IF oldsize > 0 THEN
                DEALLOCATE (A^.val,oldsize*wordsize);
            END (*IF*);
            A^.val := p;
            A^.nwords := newsize;
            IF bigger THEN
                FOR j := oldsize TO newsize-1 DO
                    A^.val^[j] := 0;
                END (*FOR*);
            END (*IF*);

            (* Update the usage data. *)

            LockList;
            IF bigger THEN INC(Usage.bytecount, wordsize*(newsize-oldsize));
            ELSE DEC (Usage.bytecount, wordsize*(oldsize-newsize));
            END (*IF*);
            UnlockList;

        END (*IF*);
    END ChangeSize;

(************************************************************************)

PROCEDURE Extend (VAR (*INOUT*) A: BN;  newsize: CARDINAL);

    (* Increases the data size of A to newsize words.  Does nothing   *)
    (* if A is already this big or bigger.                            *)

    BEGIN
        IF newsize > A^.nwords THEN
            ChangeSize (A, newsize);
        END (*IF*);
    END Extend;

(************************************************************************)

PROCEDURE Prune (VAR (*INOUT*) A: BN);

    (* Removes redundant higher-order words.  *)

    VAR newsize: CARDINAL;

    BEGIN
        newsize := A^.nwords;

        (* Note: must leave at least one word in the result.  *)

        WHILE (newsize > 1) AND (A^.val^[newsize-1] = 0) DO
            DEC (newsize);
        END (*WHILE*);
        ChangeSize (A, newsize);
    END Prune;

(************************************************************************)

PROCEDURE LowWord (A: BN): CARDINAL;

    (* Returns the least significant word of the value of A. *)

    BEGIN
        RETURN A^.val^[0];
    END LowWord;

(************************************************************************)
(*                                SHIFTS                                *)
(************************************************************************)

PROCEDURE LSW (VAR (*INOUT*) V: BN;  N: CARDINAL);

    (* Shifts V left by N CARD32 words. *)

    VAR j: CARDINAL;

    BEGIN
        ChangeSize (V, V^.nwords+N);
        FOR j := V^.nwords - 1 TO N BY -1 DO
            V^.val^[j] := V^.val^[j-N];
        END (*FOR*);
        FOR j := 0 TO N-1 DO
            V^.val^[j] := 0;
        END (*FOR*);
    END LSW;

(************************************************************************)

PROCEDURE LSBN (VAR (*INOUT*) V: BN;  N: CARDINAL);

    (* Shifts V left by N binary digits. *)

    CONST Allones = 0FFFFFFFFH;

    VAR maskL, maskR, j, carry, newcarry: CARD32;

    BEGIN
        IF N >= 32 THEN
            LSW (V, N DIV 32);
            N := N MOD 32;
        END (*IF*);

        (* Now we may assume that N < 32.  *)

        IF N > 0 THEN
            ChangeSize (V, V^.nwords+1);
            maskR := RS (Allones, N);
            maskL := Allones - maskR;
            carry := 0;
            FOR j := 0 TO V^.nwords -1 DO
                newcarry := RS (IAND (V^.val^[j], maskL), 32-N);
                V^.val^[j] := LS (V^.val^[j], N) + carry;
                carry := newcarry;
            END (*FOR*);
            Prune (V);
        END (*IF*);
    END LSBN;

(************************************************************************)

PROCEDURE RSW (VAR (*INOUT*) V: BN;  N: CARDINAL);

    (* Shifts V right by N CARD32 words. *)

    VAR j: CARDINAL;

    BEGIN
        IF N > 0 THEN
            FOR j := 0 TO V^.nwords - N - 1 DO
                V^.val^[j] := V^.val^[j+N];
            END (*FOR*);
            FOR j := V^.nwords-N TO V^.nwords-1 DO
                V^.val^[j] := 0;
            END (*FOR*);
            Prune (V);
        END (*IF*);
    END RSW;

(************************************************************************)

PROCEDURE RSBN (VAR (*INOUT*) V: BN;  N: CARDINAL);

    (* Shifts V right by N binary digits. *)

    CONST Allones = 0FFFFFFFFH;

    VAR maskL, maskR, j, carry, newcarry: CARD32;

    BEGIN
        IF N >= 32 THEN
            RSW (V, N DIV 32);
            N := N MOD 32;
        END (*IF*);

        (* Now we may assume that N < 32.  *)

        IF N > 0 THEN
            maskL := LS (Allones, N);
            maskR := Allones - maskL;
            carry := 0;
            FOR j := 0 TO V^.nwords-1 BY -1 DO
                newcarry := LS (IAND (V^.val^[j], maskR), 32-N);
                V^.val^[j] := carry + RS (V^.val^[j], N);
                carry := newcarry;
            END (*FOR*);
            Prune (V);
        END (*IF*);
    END RSBN;

(************************************************************************)
(*                              COMPARISONS                             *)
(************************************************************************)

PROCEDURE CmpU (A, B: BN): INTEGER;              (* UNSIGNED *)

    (* Unsigned comparison.  Looking at magnitudes only, returns -1 if  *)
    (* A < B, 0 if A = B, and +1 if A > B.                              *)

    VAR nA, nB, j: CARDINAL;

    BEGIN
        nA := A^.nwords;  nB := B^.nwords;
        IF nA > nB THEN
            RETURN +1;
        ELSIF nA < nB THEN
            RETURN -1;
        END (*IF*);

        (* That takes care of the obvious cases.  Now we have to deal   *)
        (* with the case where A and B have the same number of bits.    *)

        j := nA - 1;
        LOOP
            IF A^.val^[j] > B^.val^[j] THEN
                RETURN +1;
            ELSIF A^.val^[j] < B^.val^[j] THEN
                RETURN -1;
            ELSIF j = 0 THEN
                RETURN 0;
            ELSE
                DEC (j);
            END (*IF*);
        END (*LOOP*);

    END CmpU;

(************************************************************************)

PROCEDURE Cmp (A, B: BN): INTEGER;

    (* Comparison.  Returns -1 if A < B, 0 if A = B, and +1 if A > B.  *)

    BEGIN
        IF A^.negative THEN
            IF B^.negative THEN RETURN CmpU (B, A);
            ELSE RETURN -1;
            END (*IF*);
        ELSIF B^.negative THEN RETURN +1;
        ELSE RETURN CmpU (A, B);
        END (*IF*);
    END Cmp;

(************************************************************************)

PROCEDURE IsZero (val: BN): BOOLEAN;

    (* Returns TRUE iff val = 0 *)

    VAR j: CARDINAL;

    BEGIN
        IF (val = NIL) OR (val^.nwords = 0) THEN
            (* Should never happen. *)
            RETURN TRUE;
        END (*IF*);
        FOR j := 0 TO val^.nwords-1 DO
            IF val^.val^[j] <> 0 THEN
                RETURN FALSE;
            END (*IF*);
        END (*FOR*);
        RETURN TRUE;
    END IsZero;

(************************************************************************)

PROCEDURE Sign (val: BN): INTEGER;

    (* Returns -1 if val < 0, 0 if val = 0, +1 if val > 0.  *)

    BEGIN
        IF IsZero(val) THEN RETURN 0
        ELSIF val^.negative THEN RETURN -1
        ELSE RETURN +1
        END (*IF*);
    END Sign;

(************************************************************************)

PROCEDURE IsOdd (A: BN): BOOLEAN;

    (* Returns TRUE iff A is an odd number.  *)

    BEGIN
        RETURN ODD(A^.val^[0]);
    END IsOdd;

(************************************************************************)

PROCEDURE Eq (A: BN;  N: CARD32): BOOLEAN;

    (* Returns TRUE iff A = N *)

    BEGIN
        IF A^.nwords <> 1 THEN RETURN FALSE;
        ELSE RETURN A^.val^[0] = N;
        END (*IF*);
    END Eq;

(************************************************************************)
(*                      ADDITION AND SUBTRACTION                        *)
(************************************************************************)

PROCEDURE Negate (A: BN);

    (* Changes the sign of A.  *)

    BEGIN
        A^.negative := NOT A^.negative;
    END Negate;

(************************************************************************)

PROCEDURE AddU (A: BN;  B: BN);              (* UNSIGNED *)

    (* Computes  A := A + B.  This differs from Sum (below) by being    *)
    (* an in-place calculation.  The old A is overwritten.  This is an  *)
    (* unsigned calculation; the signs of A and B are ignored.          *)

    VAR nA, nB, carry, j, toadd, newval: CARDINAL;

    BEGIN
        nA := A^.nwords;  nB := B^.nwords;

        (* Make sure that A is big enough to hold the answer. *)

        IF nB > nA THEN
            nA := nB + 1;
        ELSE
            INC (nA);
        END (*IF*);
        Extend (A, nA);

        carry := 0;
        FOR j := 0 TO nA-1 DO
            IF j >= nB THEN
                toadd := 0;
            ELSE
                toadd := B^.val^[j];
            END (*IF*);

            (* Add the three components, taking care of carry. *)

            IF (toadd = MAX(CARDINAL)) AND (carry = 1) THEN
                newval := A^.val^[j];
                carry := 1;
            ELSIF A^.val^[j] > MAX(CARDINAL) - toadd - carry THEN
                newval := A^.val^[j] - (MAX(CARDINAL) - toadd - carry) - 1;
                carry := 1;
            ELSE
                newval := A^.val^[j] + toadd + carry;
                carry := 0;
            END (*IF*);
            A^.val^[j] := newval;
        END (*FOR*);

        (* We are guaranteed that the final carry is zero, because we   *)
        (* made A big enough.  Now see whether it's too big.            *)

        Prune (A);

    END AddU;

(************************************************************************)

PROCEDURE AddShortU (A: BN;  B: CARD32);              (* UNSIGNED *)

    (* Computes  A := A + B.  This is a version of AddU (see above)     *)
    (* where the second argument is a CARD32 value.                     *)

    VAR nA, carry, j, newval: CARDINAL;

    BEGIN
        nA := A^.nwords + 1;
        Extend (A, nA);


        (* Calculate the least significant word of the result. *)

        IF A^.val^[0] > MAX(CARD32) - B THEN
            newval := A^.val^[0] - (MAX(CARD32) - B) - 1;
            carry := 1;
        ELSE
            newval := A^.val^[0] + B;
            carry := 0;
        END (*IF*);
        A^.val^[0] := newval;

        (* Propagate carry upwards. *)

        j := 1;
        WHILE carry > 0 DO

            (* Add the three components, taking care of carry. *)

            IF A^.val^[j] = MAX(CARD32) THEN
                A^.val^[j] := 0;
            ELSE
                INC (A^.val^[j]);
                carry := 0;
            END (*IF*);
            INC (j);
        END (*WHILE*);

        (* We are guaranteed that the final carry is zero, because we   *)
        (* made A big enough.  Now see whether it's too big.            *)

        Prune (A);

    END AddShortU;

(************************************************************************)

PROCEDURE SubU (A: BN;  B: BN);              (* UNSIGNED *)

    (* Computes  A := A - B.  This differs from Diff (below) by being   *)
    (* an in-place calculation.  The old A is overwritten.  This is an  *)
    (* unsigned calculation; the signs of A and B are ignored.  Also,   *)
    (* we assume that the caller has ensured that A is greater than     *)
    (* or equal to B in magnitude.                                      *)

    VAR nA, nB, borrow, j, tosub, newval: CARDINAL;

    BEGIN
        nA := A^.nwords;  nB := B^.nwords;

        borrow := 0;
        FOR j := 0 TO nA-1 DO
            IF j >= nB THEN
                tosub := 0;
            ELSE
                tosub := B^.val^[j];
            END (*IF*);

            (* Add the three components, taking care of borrow. *)

            IF (tosub = MAX(CARDINAL)) AND (borrow = 1) THEN
                newval := A^.val^[j];
                borrow := 1;
            ELSIF A^.val^[j] < tosub + borrow THEN
                newval := A^.val^[j] + (MAX(CARDINAL) - tosub) + (1 - borrow);
                borrow := 1;
            ELSE
                newval := A^.val^[j] - tosub - borrow;
                borrow := 0;
            END (*IF*);
            A^.val^[j] := newval;
        END (*FOR*);

        Prune (A);

    END SubU;

(************************************************************************)

PROCEDURE IncrU (A: BN);              (* UNSIGNED *)

    (* Increases the magnitude of A by 1.  The caller must guarantee    *)
    (* that the magnitude is nonzero.                                   *)

    VAR j: CARDINAL;  carry: BOOLEAN;

    BEGIN
        carry := TRUE;  j := 0;
        WHILE carry DO
            IF j = A^.nwords THEN
                ChangeSize (A, j+1);
                A^.val^[j] := 1;
                carry := FALSE;
            ELSIF A^.val^[j] = MAX(CARD32) THEN
                A^.val^[j] := j;
                INC (j);
            ELSE
                INC (A^.val^[j]);
                carry := FALSE;
            END (*IF*);
        END (*WHILE*);
    END IncrU;

(************************************************************************)

PROCEDURE DecrU (A: BN);              (* UNSIGNED *)

    (* Reduces the magnitude of A by 1.  The caller must guarantee that *)
    (* the magnitude is nonzero.                                        *)

    VAR j: CARDINAL;  borrow: BOOLEAN;

    BEGIN
        borrow := TRUE;  j := 0;
        WHILE borrow DO
            IF A^.val^[j] = 0 THEN
                A^.val^[j] := MAX(CARD32);
                INC (j);
            ELSE
                DEC (A^.val^[j]);
                borrow := FALSE;
            END (*IF*);
        END (*WHILE*);

        (* The above loop will terminate on the first nonzero word.     *)
        (* It is possible that the top word was reduced to zero, so     *)
        (* we need to check for that case.                              *)

        IF (A^.val^[A^.nwords-1] = 0) AND (A^.nwords > 1) THEN
            Prune (A);
        END (*IF*);

    END DecrU;

(************************************************************************)

PROCEDURE Incr (A: BN);

    (* Increases the value of A by 1. *)

    BEGIN
        IF A^.negative THEN
            DecrU (A);
            IF IsZero(A) THEN
                A^.negative := FALSE;
            END (*IF*);
        ELSE
            IncrU (A);
        END (*IF*);
    END Incr;

(************************************************************************)

PROCEDURE Decr (A: BN);

    (* Reduces the value of A by 1. *)

    BEGIN
        IF IsZero(A) THEN
            A^.negative := TRUE;
        END (*IF*);
        IF A^.negative THEN
            IncrU (A);
        ELSE
            DecrU (A);
        END (*IF*);
    END Decr;

(************************************************************************)

PROCEDURE Sum (A, B: BN): BN;

    (* Returns A+B. *)

    VAR result, other: BN;  AgtB: INTEGER;  negate: BOOLEAN;

    BEGIN
        AgtB := CmpU (A, B);
        IF AgtB >= 0 THEN
            result := CopyBN(A);
            other := B;
        ELSE
            result := CopyBN(B);
            other := A;
        END (*IF*);

        (* The following logic could possibly be simplified, but I've   *)
        (* lost the patience to do it.  It is basically just separating *)
        (* the possibilities into four cases.                           *)

        IF A^.negative = B^.negative THEN
            AddU (result, other);
        ELSE
            SubU (result, other);
        END (*IF*);

        IF A^.negative THEN
            IF B^.negative THEN
                negate := TRUE;
            ELSE
                (* A < 0, B >= 0. *)
                negate := AgtB >= 0;
            END (*IF*);
        ELSE
            IF B^.negative THEN
                (* A >= 0, B < 0. *)
                negate := AgtB < 0;
            ELSE
                negate := FALSE;
            END (*IF*);
        END (*IF*);
        result^.negative := negate;
        RETURN result;

    END Sum;

(************************************************************************)

PROCEDURE Diff (A, B: BN): BN;

    (* Returns A-B. *)

    VAR BCopy, result: BN;

    BEGIN
        BCopy := CopyBN (B);
        BCopy^.negative := NOT B^.negative;
        result := Sum (A, BCopy);
        Discard (BCopy);
        RETURN result;
    END Diff;

(************************************************************************)
(*                              MULTIPLICATION                          *)
(************************************************************************)

PROCEDURE Prod (A,B: BN): BN;

    (* Returns A*B.  *)

    VAR result: BN;
        product: Arith64.CARD64;

    (********************************************************************)

    PROCEDURE Addpart (k: CARDINAL);

        (* Adds the partial product into result starting at word k. *)

        VAR carry, j: CARDINAL;
            part: ARRAY [0..1] OF CARD32;

        BEGIN

            part[0] := product.low;  part[1] := product.high;
            carry := 0;
            FOR j := 0 TO 1 DO
                IF (part[j] = MAX(CARD32)) AND (carry = 1) THEN
                    (* Let carry continue to propagate. *)
                ELSIF result^.val^[k] <= MAX(CARD32) - part[j] - carry THEN
                    INC (result^.val^[k], part[j] + carry);
                    carry := 0;
                ELSE
                    DEC (result^.val^[k], (MAX(CARD32) - part[j]) + (1 - carry));
                    carry := 1;
                END (*IF*);
                INC (k);
            END (*FOR*);

            (* We have allocated enough result space to ensure that the *)
            (* following loop won't run off the end of the array.       *)

            WHILE carry > 0 DO
                IF result^.val^[k] = MAX(CARD32) THEN
                    result^.val^[k] := 0;
                ELSE
                    INC (result^.val^[k]);
                    carry := 0;
                END (*IF*);
                INC (k);
            END (*WHILE*);

        END Addpart;

    (********************************************************************)

    VAR i, j: CARDINAL;

    BEGIN
        (* Create an initially zero result. *)

        NEW (result);
        result^.negative := (A^.negative <> B^.negative);
        result^.nwords := 0;
        result^.val := NIL;
        AddToList (result);
        ChangeSize (result, A^.nwords + B^.nwords);
        FOR i := 0 TO result^.nwords-1 DO
            result^.val^[i] := 0;
        END (*FOR*);

        (* Collect and add the partial products.  *)

        FOR i := 0 TO A^.nwords-1 DO
            FOR j := 0 TO B^.nwords-1 DO

                product := Arith64.Mul32 (A^.val^[i], B^.val^[j]);
                Addpart (i+j);

            END (*FOR*);

        END (*FOR*);

        Prune (result);
        RETURN result;

    END Prod;

(************************************************************************)

PROCEDURE Double (A: BN);

    (* In-place multiplication by two.  *)

    VAR size, j, carry, nextcarry, x: CARDINAL;

    BEGIN
        size := A^.nwords;
        carry := 0;
        FOR j := 0 TO size-1 DO
            nextcarry := 0;
            x := A^.val^[j];
            IF x >= topbit THEN
                nextcarry := 1;
                DEC (x, topbit);
            END (*IF*);
            x := 2 * x + carry;
            A^.val^[j] := x;
            carry := nextcarry;
        END (*FOR*);

        (* If there was a final carry, we have to extend the size. *)

        IF carry > 0 THEN
            Extend (A, size+1);
            A^.val^[size] := 1;
        END (*IF*);

    END Double;

(************************************************************************)
(*                               DIVISION                               *)
(************************************************************************)

PROCEDURE Halve (A: BN);

    (* In-place division by two.  We round towards zero.  *)

    VAR size, j, x: CARDINAL;
        carry, nextcarry: BOOLEAN;

    BEGIN
        size := A^.nwords;
        carry := FALSE;
        FOR j := size-1 TO 0 BY -1 DO
            x := A^.val^[j];
            nextcarry := ODD(x);
            x := x DIV 2;
            IF carry THEN
                INC (x, topbit);
            END (*IF*);
            A^.val^[j] := x;
            carry := nextcarry;
        END (*FOR*);

        IF A^.val^[size-1] = 0 THEN
            Prune (A);
        END (*IF*);

    END Halve;

(************************************************************************)

PROCEDURE DivideU (A, B: BN;  VAR (*OUT*) Q, R: BN);

    (* Divides A by B, returns quotient and remainder.  This is the     *)
    (* unsigned case, where we assume that A and B are nonzero and      *)
    (* positive.                                                        *)

    VAR K, BK: BN;
        mask, test: CARD32;
        j: CARDINAL;

    BEGIN
        Q := Zero();  R := CopyBN(A);

        (* Invariant: A = B*Q + R.  *)

        (* We are going to build up the quotient as the sum of numbers  *)
        (* each of which has a single 1 bit.  In the worst case (B = 1) *)
        (* the quotient can have as many bits as A does.  Let K be a    *)
        (* sufficiently large number whose bits are all zero except for *)
        (* the most significant bit.  My estimate below is a bit        *)
        (* conservative, but we should still get reasonable speed.      *)

        test := A^.val^[A^.nwords-1];

        (* First approximation to mask.  *)

        IF test < 100H THEN
            mask := 80H;
        ELSIF test < 10000H THEN
            mask := 8000H;
        ELSIF test < 1000000H THEN
            mask := 800000H;
        ELSE
            mask := 80000000H;
        END (*IF*);

        (* Refinement of mask. *)

        WHILE IAND (mask, test) = 0 DO
            mask := mask DIV 2;
        END (*WHILE*);

        NEW (K);
        K^.negative := FALSE;
        K^.nwords := A^.nwords;
        ALLOCATE (K^.val, K^.nwords * wordsize);
        IF K^.nwords > 1 THEN
            FOR j := 0 TO K^.nwords - 2 DO
                K^.val^[j] := 0;
            END (*FOR*);
        END (*IF*);
        K^.val^[K^.nwords-1] := mask;
        AddToList (K);

        BK := Prod (B, K);

        WHILE CmpU (R, B) >= 0 DO

            (* For any K, if we set                                 *)
            (*          Qnew = Q + K       Rnew = R - B*K           *)
            (* then the above invariant condition will still be     *)
            (* satisfied.                                           *)

            IF CmpU (R, BK) >= 0 THEN
                AddU (Q, K);  SubU (R, BK);
            END (*IF*);
            Halve (K);  Halve (BK);

        END (*WHILE*);

        Prune (Q);  Prune (R);
        Discard (K);  Discard (BK);

    END DivideU;

(************************************************************************)

PROCEDURE Divide (A, B: BN;  VAR (*OUT*) Q, R: BN);

    (* Divides A by B, returns quotient and remainder.  This might      *)
    (* need more work for negative A or B.                              *)

    VAR Aneg, Bneg: BOOLEAN;

    BEGIN
        IF IsZero(B) THEN
            (* Result undefined, pick an arbitrary result. *)
            Q := Zero();  R := CopyBN(A);
        ELSIF IsZero(A) THEN
            Q := Zero();  R := Zero();
        ELSE
            Aneg := A^.negative;  Bneg := B^.negative;
            A^.negative := FALSE;  B^.negative := FALSE;
            DivideU (A, B, Q, R);
            A^.negative := Aneg;  B^.negative := Bneg;

            (* The following rules are the simplest way I know of to    *)
            (* ensure that A=QB+R in the signed case.                   *)

            IF Aneg <> Bneg THEN
                IF NOT IsZero(Q) THEN
                    Q^.negative := TRUE;
                END (*IF*);
            END (*IF*);
            IF Aneg THEN
                IF NOT IsZero(R) THEN
                    R^.negative := TRUE;
                END (*IF*);
            END (*IF*);
        END (*IF*);
    END Divide;

(************************************************************************)
(*                           MODULAR ARITHMETIC                         *)
(************************************************************************)

PROCEDURE ModularPower (base, exponent, modulus: BN): BN;

    (* Calculates base^exponent MOD modulus.  We use the traditional    *)
    (* fast squaring method, but with modular reduction to stop the     *)
    (* intermediate results from getting too large.                     *)

    (* This is the version where all three parameters are Bignums.    *)

    VAR result, product, Q, R, cbase, cexp: BN;

    BEGIN
        (*WriteString ("Entering ModularPower, ");  ShowBNUsage;*)

        IF Eq(modulus, 1) THEN RETURN MakeBignum(0) END(*IF*);

        (* Make local copies of the arguments we will change, so as not *)
        (* to affect the caller's copies of these variables.            *)

        cbase := CopyBN (base);
        cexp := CopyBN (exponent);

        result := MakeBignum(1);
        Divide (cbase, modulus, Q, R);
        Discard (Q);
        Discard (cbase);
        cbase := R;
        WHILE NOT IsZero(cexp) DO
            IF IsOdd (cexp) THEN
                product := Prod (result, cbase);
                Discard (result);
                Divide (product, modulus, Q, result);
                Discard (product);
                Discard (Q);
            END (*IF*);
            Halve (cexp);
            product := Prod (cbase, cbase);
            Discard (cbase);
            Divide (product, modulus, Q, cbase);
            Discard (product);
            Discard (Q);
        END (*WHILE*);
        Discard (cexp);
        Discard (cbase);
        (*WriteString ("Leaving ModularPower, ");  ShowBNUsage;*)
        RETURN result;
    END ModularPower;

(************************************************************************)
(*                       RANDOM NUMBER GENERATOR                        *)
(************************************************************************)

PROCEDURE Rand (N: CARDINAL): BN;

    (* Produces a random N-word nonnegative number.  *)

    (* The present implementation probably doesn't have the randomness  *)
    (* properties that we would prefer, but as a proof-of-concept       *)
    (* implementation it should be good enough for now.                 *)

    VAR result: BN;
        j: CARDINAL;

    BEGIN
        NEW (result);
        result^.negative := FALSE;
        result^.nwords := N;
        ALLOCATE (result^.val, wordsize*N);
        FOR j := 0 TO N-1 DO
            result^.val^[j] := RandCardinal();
        END (*FOR*);
        AddToList (result);
        RETURN result;
    END Rand;

(************************************************************************)
(*                    MISCELLANEOUS OTHER FUNCTIONS                     *)
(************************************************************************)

PROCEDURE Nbits (A: BN): CARDINAL;

    (* Returns the number of significant bits in A.  Equivalently,      *)
    (* returns an N such that A >= 2^(N-1) but A < 2^N.                 *)
    (* Special case: if A is zero we return 0.                          *)

    VAR N, topword, mask: CARDINAL;

    BEGIN
        topword := A^.val^[A^.nwords-1];
        IF topword = 0 THEN
            Prune (A);
            topword := A^.val^[A^.nwords-1];
            IF topword = 0 THEN
                RETURN 0;
            END (*IF*);
        END (*IF*);
        N := bitsperword * A^.nwords;
        mask := topbit;
        WHILE mask > topword DO
            mask := mask DIV 2;
            DEC (N);
        END (*WHILE*);
        RETURN N;
    END Nbits;

(************************************************************************)

PROCEDURE Digits (A: BN): CARDINAL;

    (* An estimate of the number of decimal digits in A.  Since we      *)
    (* don't calculate this precisely, we err if necessary on the high  *)
    (* side.  For negative A, we count the '-' as an extra "digit".     *)

    CONST log2 = 0.30103;

    VAR c: REAL;  d: INTEGER;

    BEGIN
        c := log2 * FLOAT (Nbits(A));
        d := TRUNC (c);
        IF c > FLOAT(d) THEN
            INC (d);
        END (*IF*);
        IF A^.negative THEN
            INC (d);
        END (*IF*);
        RETURN d;
    END Digits;

(************************************************************************)
(*                          STRING CONVERSIONS                          *)
(************************************************************************)

PROCEDURE ToHexN (x: CARD32;  N: CARDINAL;  dropLZ: BOOLEAN;
        VAR (*OUT*) buffer: ARRAY OF CHAR;  VAR (*INOUT*) pos: CARDINAL);
                                                            (* UNSIGNED *)

    (* Puts N-digit hexadecimal at buffer[pos], updates pos.  Drops     *)
    (* leading zeros if dropLZ is TRUE.                                 *)

    VAR firstpart: CARD32;

    BEGIN
        IF N > 1 THEN
            firstpart := x DIV 16;
            IF (firstpart > 0) OR NOT dropLZ THEN
                ToHexN (firstpart, N-1, dropLZ, buffer, pos);
            END (*IF*);
            x := x MOD 16;
        END (*IF*);
        IF x > 9 THEN
            buffer[pos] := CHR(ORD('A') - 10 + x);
        ELSE
            buffer[pos] := CHR(ORD('0') + x);
        END (*IF*);
        INC (pos);
    END ToHexN;

(************************************************************************)

PROCEDURE ToHex8 (A: CARD32;  dropLZ: BOOLEAN;
                        VAR (*OUT*) buffer: ARRAY OF CHAR;
                        VAR (*INOUT*) pos: CARDINAL);       (* UNSIGNED *)

    (* Puts 8-digit hexadecimal at buffer[pos], updates pos.  Drops     *)
    (* leading zeros if dropLZ is TRUE.                                 *)

    BEGIN
        ToHexN (A, 8, dropLZ, buffer, pos);
    END ToHex8;

(************************************************************************)

PROCEDURE ToHex (A: BN;  VAR (*OUT*) buffer: ARRAY OF CHAR;  bufsize: CARDINAL);

    (* Puts A in buffer in hexadecimal. *)

    VAR j, pos: CARDINAL;  dropLZ: BOOLEAN;

    BEGIN
        pos := 0;
        IF A^.negative THEN
            buffer[0] := '-';
            pos := 1;
        END (*IF*);

        (* Check for buffer big enough. *)

        IF 8*A^.nwords + pos > bufsize THEN
            FOR pos := 0 TO bufsize-1 DO
                buffer[pos] := '*';
            END (*FOR*);
            RETURN;
        END (*IF*);

        dropLZ := TRUE;
        FOR j := A^.nwords-1 TO 0 BY -1 DO
            ToHex8 (A^.val^[j], dropLZ, buffer, pos);
            dropLZ := FALSE;
        END (*FOR*);
        IF pos < bufsize THEN
            buffer[pos] := CHR(0);
        END (*IF*);

    END ToHex;

(************************************************************************)

PROCEDURE FromHexW (VAR (*IN*) hexdata: ARRAY OF CHAR;
                  VAR (*INOUT*) pos: CARDINAL;  nchars: CARDINAL): CARD32;
                                                            (* UNSIGNED *)

    (* Converts a string of nchars hexadecimal characters, starting at  *)
    (* hexdata[pos], to numeric.  Updates pos.                          *)

    VAR j, result: CARDINAL;  ch: CHAR;

    BEGIN
        result := 0;
        FOR j := 0 TO nchars-1 DO
            ch := hexdata[pos];  INC (pos);
            IF ch > '9' THEN
                ch := CAP(ch);
                result := 16*result + (ORD(ch) - ORD('A') + 10);
            ELSE
                result := 16*result + ORD(ch) - ORD('0');
            END (*IF*);
        END (*FOR*);
        RETURN result;
    END FromHexW;

(************************************************************************)

PROCEDURE FromHex (hexdata: ARRAY OF CHAR): BN;

    (* Converts hexadecimal string to BN. *)

    VAR N, nchars, j, pos: CARDINAL;
        negative: BOOLEAN;
        result: BN;

    BEGIN
        pos := 0;  negative := FALSE;
        IF hexdata[0] = '+' THEN
            pos := 1;
        ELSIF hexdata[0] = '-' THEN
            negative := TRUE;
            pos := 1;
        END (*IF*);

        (* There are 8 characters per CARD32 value. *)

        N := HIGH(hexdata) - pos;
        nchars := N MOD 8;
        IF nchars = 0 THEN
            nchars := 8;
        ELSE
            INC (N, 8-nchars);
        END (*IF*);
        N := N DIV 8;
        NEW (result);
        result^.negative := negative;
        result^.nwords := N;
        ALLOCATE (result^.val, N*wordsize);
        FOR j := N-1 TO 0 BY -1 DO
            result^.val^[j] := FromHexW (hexdata, pos, nchars);
            nchars := 8;
        END (*FOR*);

        AddToList (result);
        RETURN result;

    END FromHex;

(************************************************************************)

PROCEDURE ToDecimalU (A, ten: BN;  VAR (*INOUT*) buffer: ARRAY OF CHAR;
                                            VAR (*INOUT*) pos: CARDINAL);

    (* Puts A at buffer[pos] in decimal, updates pos.  The second       *)
    (* parameter is a precomputed 10 decimal.  A must be >= 0.          *)

    VAR digit: CARDINAL;
        Q, R: BN;

    BEGIN
        IF CmpU (A, ten) >= 0 THEN
            Divide (A, ten, Q, R);
            digit := R^.val^[0];
            ToDecimalU (Q, ten, buffer, pos);
            Discard (Q);  Discard (R);
        ELSE
            digit := A^.val^[0];
        END (*IF*);
        IF pos <= HIGH(buffer) THEN
            buffer[pos] := CHR(ORD('0') + digit);
            INC (pos);
        END (*IF*);
    END ToDecimalU;

(************************************************************************)

PROCEDURE ToDecimal (A: BN;  VAR (*OUT*) buffer: ARRAY OF CHAR;
                                            VAR (*INOUT*) pos: CARDINAL);

    (* Puts A at buffer[pos] in decimal, updates pos. *)

    VAR ten: BN;
        Aneg: BOOLEAN;

    BEGIN
        ten := Zero();  ten^.val^[0] := 10;
        Aneg := A^.negative;
        IF Aneg THEN
            buffer[0] := '-';  pos := 1;
            A^.negative := FALSE;
        END (*IF*);
        ToDecimalU (A, ten, buffer, pos);
        IF pos <= HIGH(buffer) THEN
            buffer[pos] := CHR(0);
        END (*IF*);
        A^.negative := Aneg;
        Discard (ten);
    END ToDecimal;

(************************************************************************)

PROCEDURE FromDecimal (decdata: ARRAY OF CHAR): BN;

    (* Converts decimal string to BN. *)

    VAR ten, A, result: BN;
        pos: CARDINAL;   negate: BOOLEAN;

    BEGIN
        ten := Zero();  ten^.val^[0] := 10;
        result := Zero();
        negate := FALSE;
        pos := 0;
        IF decdata[0] = '+' THEN
            pos := 1;
        ELSIF decdata[0] = '-' THEN
            negate := TRUE;
            pos := 1;
        END (*IF*);
        WHILE (pos <= HIGH(decdata)) AND (decdata[pos] <> CHR(0)) DO
            A := Prod (result, ten);
            AddShortU (A, ORD(decdata[pos]) - ORD('0'));
            INC (pos);
            Discard (result);
            result := A;
        END (*WHILE*);
        Discard (ten);
        IF negate THEN
            result^.negative := TRUE;
        END (*IF*);
        RETURN result;
    END FromDecimal;

(************************************************************************)

BEGIN
    WITH Usage DO
        DosCreateMutexSem (NIL, access, 0, FALSE);
        listhead := NIL;
        BNcount := 0;
        bytecount := 0;
    END (*WITH*);
    Randomize;
END BigNum.


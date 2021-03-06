(**************************************************************************)
(*                                                                        *)
(*  Web server input stream handler                                       *)
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

IMPLEMENTATION MODULE TLScompression;

        (********************************************************)
        (*                                                      *)
        (* Interface between TLS and the compression algorithms *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            5 April 2018                    *)
        (*  Last edited:        5 April 2018                    *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


(************************************************************************)
(*                            METHOD SUPPORTED                          *)
(************************************************************************)

PROCEDURE CompressionSupported (N: CARDINAL): CompressionMethod;

    (* Returns the compression method if method N supported,    *)
    (* otherwise returns nocompression.                         *)

    BEGIN
        RETURN nocompression;
    END CompressionSupported;

(************************************************************************)

END TLScompression.


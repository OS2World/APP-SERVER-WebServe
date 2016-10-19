(**************************************************************************)
(*                                                                        *)
(*  Setup for web server                                                  *)
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

IMPLEMENTATION MODULE SUMIME;

        (****************************************************************)
        (*                                                              *)
        (*                     PM Setup for web server                  *)
        (*             The 'MIME types' page of the notebook            *)
        (*                                                              *)
        (*        Started:        8 April 2015                          *)
        (*        Last edited:    14 August 2016                        *)
        (*        Status:         This module OK                        *)
        (*                                                              *)
        (****************************************************************)


IMPORT CommonSettings;

FROM RINIData IMPORT
    (* proc *)  OpenINIFile, CloseINIFile, KeyExists, INIDeleteKey;

(************************************************************************)
(*                 DELETING OBSOLETE INI FILE ENTRIES                   *)
(************************************************************************)

PROCEDURE DeleteObsoleteEntries;

    (* Removes the INI entry $SYS/MIME. *)

    VAR app, key: ARRAY [0..4] OF CHAR;
        INIname: ARRAY [0..31] OF CHAR;
        UseTNI: BOOLEAN;

    BEGIN
        UseTNI := CommonSettings.GetINIFilename (INIname);
        app := "$SYS";  key := "MIME";
        IF OpenINIFile (INIname, UseTNI) THEN
            IF KeyExists (app, key) THEN
                (* Obsolete option *)
                INIDeleteKey (app, key);
            END (*IF*);
            CloseINIFile;
        END (*IF*);
    END DeleteObsoleteEntries;

(************************************************************************)

END SUMIME.


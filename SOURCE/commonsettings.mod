(**************************************************************************)
(*                                                                        *)
(*  Setup for lightweight web server                                      *)
(*  Copyright (C) 2017   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE CommonSettings;

        (****************************************************************)
        (*                                                              *)
        (*                  Setup program for WebServe                  *)
        (*            Data common to the main Setup notebook            *)
        (*                                                              *)
        (*    Started:        4 April 2015                              *)
        (*    Last edited:    22 May 2017                               *)
        (*    Status:         OK                                        *)
        (*                                                              *)
        (****************************************************************)


IMPORT OS2, Strings, FileSys, INIData, RINIData, Languages;

FROM Names IMPORT
    (* type *)  FilenameString;

FROM MiscFuncs IMPORT
    (* proc *)  StringMatch;

(************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    FontNameArray = ARRAY FontGroup OF FontName;

<* PUSH *>
<* VOLATILE+ *>

VAR
    SetupINIname, INIFileName: ARRAY [0..15] OF CHAR;
    TNI: BOOLEAN;
    FontChangeEnabled: BOOLEAN;
    OurFontName: FontNameArray;
    OurLanguage: Languages.LangHandle;
    LanguageCode: ARRAY [0..31] OF CHAR;

<* POP *>

CONST
    FontGroupLabel = FontNameArray {"", "MainNotebook", "DomainEditor"};
    DefaultFont = FontNameArray {"", "8.Helv", "8.Helv"};

(************************************************************************)

PROCEDURE EnableFontChanges (enable: BOOLEAN);

    (* Enables or disables font updates.  We want to be able to ignore  *)
    (* "new font" messages while notebooks are opening or closing.      *)

    BEGIN
        FontChangeEnabled := enable;
    END EnableFontChanges;

(************************************************************************)

PROCEDURE CurrentFont (group: FontGroup;  VAR (*OUT*) fontname: FontName);

    (* Returns the currently set font for this group. *)

    BEGIN
        fontname := OurFontName[group];
    END CurrentFont;

(************************************************************************)

PROCEDURE UpdateFontFrom (hwnd: OS2.HWND;  group: FontGroup);

    (* Takes the font setting from window hwnd and propagates it to the *)
    (* entire group.  Note that this will often be a "no operation"     *)
    (* because the font is no different from the presently active one.  *)
    (* The new font is also saved to Setup.INI or Setup.TNI.            *)

    VAR NewFontName: FontName;
        AttrFound, length: CARDINAL;
        hini: INIData.HINI;  target: OS2.HWND;
        app: ARRAY [0..4] OF CHAR;
        key: FontName;

    BEGIN
        IF FontChangeEnabled AND (group <> NilFontGroup) THEN
            length := OS2.WinQueryPresParam (hwnd, OS2.PP_FONTNAMESIZE, 0,
                                         AttrFound, FontNameSize, NewFontName,
                                          0(*OS2.QPF_NOINHERIT*));
            IF length < FontNameSize THEN
                NewFontName[length] := Nul;
            END (*IF*);

            IF NOT Strings.Equal (NewFontName, OurFontName[group]) THEN

                OurFontName[group] := NewFontName;
                hini := INIData.OpenINIFile (SetupINIname, TNI);
                app := "Font";
                key := FontGroupLabel[group];
                INIData.INIPutString (hini, app, key, NewFontName);
                INIData.CloseINIFile (hini);

                (* For reasons that are still a mystery to me, we have to go    *)
                (* up three levels in the hierarchy to get from here to the     *)
                (* frame.  (I calculated two, but there's apparently an extra   *)
                (* container window that we're not told about.)                 *)

                target := OS2.WinQueryWindow(hwnd, OS2.QW_PARENT);
                target := OS2.WinQueryWindow(target, OS2.QW_PARENT);
                target := OS2.WinQueryWindow(target, OS2.QW_PARENT);
                OS2.WinSendMsg (target, FONTCHANGED, NIL, NIL);

            END (*IF*);

        END (*IF*);

    END UpdateFontFrom;

(************************************************************************)

PROCEDURE StoreLanguage (name: ARRAY OF CHAR);

    (* Puts the language code into the Setup INI file. *)

    VAR hini: INIData.HINI;
        app: ARRAY [0..5] OF CHAR;

    BEGIN
        hini := INIData.OpenINIFile(SetupINIname, TNI);
        app := "Setup";
        IF INIData.INIValid (hini) THEN
            INIData.INIPutString (hini, app, 'Language', name);
        END (*IF*);
        INIData.CloseINIFile (hini);
    END StoreLanguage;

(************************************************************************)

PROCEDURE ChangeLanguageTo (name: ARRAY OF CHAR): BOOLEAN;

    (* Changes the current language setting, but only if a suitable     *)
    (* setup.name.lng file exists.  Returns FALSE if no change.         *)

    CONST prefix = "Setup";

    VAR result: BOOLEAN;  file: FilenameString;

    BEGIN
        result := FALSE;
        IF NOT StringMatch (name, LanguageCode) THEN
            Strings.Assign (prefix, file);
            Strings.Append (".", file);
            Strings.Append (name, file);
            Strings.Append (".lng", file);
            IF FileSys.Exists (file) THEN
                Languages.DropLanguage (OurLanguage);
                Strings.Assign (name, LanguageCode);
                OurLanguage := Languages.UseLanguage (prefix, name);
                StoreLanguage (name);
                result := TRUE;
            END (*IF*);
        END (*IF*);
        RETURN result;
    END ChangeLanguageTo;

(************************************************************************)

PROCEDURE CurrentLanguage (VAR (*OUT*) lang: Languages.LangHandle;
                                VAR (*OUT*) name: ARRAY OF CHAR);

    (* Returns the current language setting. *)

    BEGIN
        lang := OurLanguage;
        Strings.Assign (LanguageCode, name);
    END CurrentLanguage;

(************************************************************************)

PROCEDURE SetInitialLanguage;

    (* Sets the language from the Setup INI or TNI file. *)

    VAR hini: INIData.HINI;
        app: ARRAY [0..5] OF CHAR;

    BEGIN
        hini := INIData.OpenINIFile(SetupINIname, TNI);
        app := "Setup";
        IF NOT INIData.INIValid (hini)
           OR NOT INIData.INIGetString (hini, app, 'Language', LanguageCode)
                                     OR (LanguageCode[0] = Nul) THEN
            LanguageCode := "en";
        END (*IF*);
        OurLanguage := Languages.UseLanguage ("Setup", LanguageCode);
        INIData.CloseINIFile (hini);
        StoreLanguage (LanguageCode);
    END SetInitialLanguage;

(************************************************************************)

PROCEDURE SetFonts;

    (* Sets initial fonts from Setup.INI or Setup.TNI. *)

    VAR hini: INIData.HINI;  group: FontGroup;
        app: ARRAY [0..4] OF CHAR;
        label: FontName;

    BEGIN
        hini := INIData.OpenINIFile(SetupINIname, TNI);
        IF NOT INIData.INIValid(hini) THEN
            hini := INIData.CreateINIFile(INIFileName, TNI);
        END (*IF*);
        FOR group := MIN(FontGroup) TO MAX(FontGroup) DO
            IF group <> NilFontGroup THEN
                app := "Font";
                label := FontGroupLabel[group];
                IF NOT INIData.INIGetString (hini, app, label,
                                                    OurFontName[group])
                              OR (OurFontName[group][0] = Nul) THEN
                    OurFontName[group] := DefaultFont[group];
                    INIData.INIPutString (hini, app, label,
                                                        OurFontName[group]);
                END (*IF*);
            END (*IF*);
        END (*FOR*);
        INIData.CloseINIFile (hini);
    END SetFonts;

(************************************************************************)

PROCEDURE SetINIFilename (UseTNI: BOOLEAN);

    (* Makes a record of the application INI file (not the Setup INI    *)
    (* file) name, which can then be queried by other modules.          *)

    VAR hini: INIData.HINI;

    BEGIN
        TNI := UseTNI;
        INIFileName := "WebServe.";
        SetupINIname := "Setup.";
        IF TNI THEN
            Strings.Append ("TNI", INIFileName);
            Strings.Append ("TNI", SetupINIname);
        ELSE
            Strings.Append ("INI", INIFileName);
            Strings.Append ("INI", SetupINIname);
        END (*IF*);

        (* Make sure the two files exist. *)

        hini := INIData.OpenINIFile(SetupINIname, TNI);
        IF NOT INIData.INIValid(hini) THEN
            hini := INIData.CreateINIFile(INIFileName, TNI);
        END (*IF*);
        INIData.CloseINIFile (hini);

        IF RINIData.OpenINIFile(INIFileName, TNI) THEN
            RINIData.CloseINIFile;
        END (*IF*);

    END SetINIFilename;

(************************************************************************)

PROCEDURE GetINIFilename (VAR (*OUT*) name: ARRAY OF CHAR): BOOLEAN;

    (* Returns the name of the INI or TNI file for this application. *)
    (* Returns TRUE iff this is a TNI file.                          *)

    BEGIN
        Strings.Assign (INIFileName, name);
        RETURN TNI;
    END GetINIFilename;

(************************************************************************)

BEGIN
    FontChangeEnabled := FALSE;
    SetINIFilename (FALSE);
END CommonSettings.


(**************************************************************************)
(*                                                                        *)
(*  Setup for lightweight web server                                      *)
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

IMPLEMENTATION MODULE SUPage1;

        (****************************************************************)
        (*                                                              *)
        (*                    PM Setup for WebServe                     *)
        (*                    Page 1 of the notebook                    *)
        (*                                                              *)
        (*        Started:        5 April 2015                          *)
        (*        Last edited:    27 January 2018                       *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


FROM SYSTEM IMPORT ADDRESS, INT16, CAST, ADR;

IMPORT OS2, OS2RTL;

IMPORT DID, Strings, CommonSettings;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer, StrToBufferN;

FROM RINIData IMPORT
    (* proc *)  RemoteOperation, INIPut, INIPutString,
                OpenINIFile, CloseINIFile,
                INIFetch, INIGetCard, INIGetTwoShort, INIGetString,
                MakeDirectory;

FROM Remote IMPORT
    (* proc *)  OurDirectory;

FROM MiscFuncs IMPORT
    (* type *)  CharArrayPointer,
    (* proc *)  EVAL;

(**************************************************************************)

CONST
    Nul = CHR(0);
    LangStringSize = 32;

VAR
    INIname: ARRAY [0..31] OF CHAR;
    UseTNI: BOOLEAN;

    ChangeInProgress: BOOLEAN;
    OurPageID: CARDINAL;
    OurLang: LangHandle;
    OurLangStr: ARRAY [0..LangStringSize] OF CHAR;

    (* Handle to the window that belongs to this page.  We can save     *)
    (* this as a static value because there is never more than one      *)
    (* instance of this page.                                           *)

    pagehandle, hwndParent: OS2.HWND;
    notebookhandle: OS2.HWND;

    (* Original values of some ini variables. *)

    OldServerPort, OldTimeout, OldMaxClients: CARDINAL;
    OldEnable: CARDINAL;
    OldResolveIP: BOOLEAN;
    OldLanguage: ARRAY [0..LangStringSize] OF CHAR;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        OurLang := lang;
        StrToBuffer (lang, "Page1.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,OurPageID), ADR(stringval));
        StrToBuffer (lang, "Page1.ServerLabel", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ServerLabel, stringval);
        StrToBuffer (lang, "Page1.Port", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.WebPortLabel, stringval);
        StrToBuffer (lang, "Page1.Timeout", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.TimeoutLabel, stringval);
        StrToBuffer (lang, "Page1.MaxClients", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.WebMaxLabel, stringval);
        StrToBuffer (lang, "Page1.Language", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.LanguageLabel, stringval);
        StrToBuffer (lang, "Page1.ResolveIP", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ResolveIP, stringval);
    END SetLanguage;

(************************************************************************)
(*                     LOADING DATA FROM THE INI FILE                   *)
(************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the dialogue elements on page 1 with data from the INI file,       *)
    (* or loads default values if they're not in the INI file.                  *)

    VAR val0: INT16;
        val1: BOOLEAN;

    BEGIN
        EVAL (OpenINIFile (INIname, UseTNI));

        (* Server port. *)

        IF INIGetCard ('$SYS', 'ServerPort', OldServerPort) THEN
            val0 := OldServerPort;
        ELSE
            val0 := 80;
        END (*IF*);
        OS2.WinSetDlgItemShort (hwnd, DID.WebPortField, val0, FALSE);

        (* Timeout. *)

        IF INIGetCard ('$SYS', 'Timeout', OldTimeout) THEN
            val0 := OldTimeout;
        ELSE
            val0 := 120;
        END (*IF*);
        OS2.WinSetDlgItemShort (hwnd, DID.Timeout, val0, FALSE);

        (* Maximum number of users. *)

        IF INIGetCard ('$SYS', 'MaxClients', OldMaxClients) THEN
            val0 := OldMaxClients;
        ELSE
            val0 := 20;
        END (*IF*);
        OS2.WinSetDlgItemShort (hwnd, DID.MaxClients, val0, FALSE);

        (* Resolve IP addresses. *)

        IF INIFetch ('$SYS', 'ResolveIP', OldResolveIP) THEN
            val1 := OldResolveIP;
        ELSE
            val1 := FALSE;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.ResolveIP, OS2.BM_SETCHECK,
                                 OS2.MPFROMSHORT(ORD(val1)), NIL);

        CloseINIFile;

        (* Language has to be obtained from CommonSettings. *)

        CommonSettings.CurrentLanguage (OurLang, OurLangStr);
        OS2.WinSetDlgItemText(hwnd, DID.Language, OurLangStr);

    END LoadValues;

(************************************************************************)
(*                      STORING DATA TO THE INI FILE                    *)
(************************************************************************)

PROCEDURE QueryButton (hwnd: OS2.HWND;  B: CARDINAL): CARDINAL;

    BEGIN
        RETURN OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, B,
                                              OS2.BM_QUERYCHECK, NIL, NIL));
    END QueryButton;

(**************************************************************************)

PROCEDURE StoreData (hwnd1: OS2.HWND);

    (* Stores the values on page 1 back into the INI file.  *)

    VAR val: CARDINAL;  temp: INT16;  bool: BOOLEAN;

    BEGIN
        EVAL (OpenINIFile (INIname, UseTNI));

        (* Server port. *)

        OS2.WinQueryDlgItemShort (hwnd1, DID.WebPortField, temp, FALSE);
        val := temp;
        IF val <> OldServerPort THEN
            INIPut ('$SYS', 'ServerPort', val);
        END (*IF*);

        (* Timeout. *)

        OS2.WinQueryDlgItemShort (hwnd1, DID.Timeout, temp, FALSE);
        val := temp;
        IF val <> OldTimeout THEN
            INIPut ('$SYS', 'Timeout', val);
        END (*IF*);

        (* Maximum number of users. *)

        OS2.WinQueryDlgItemShort (hwnd1, DID.MaxClients, temp, FALSE);
        val := temp;
        IF val <> OldMaxClients THEN
            INIPut ('$SYS', 'MaxClients', val);
        END (*IF*);

        (* Resolve IP addresses. *)

        temp := QueryButton (hwnd1, DID.ResolveIP);
        bool := temp <> 0;
        IF bool <> OldResolveIP THEN
            INIPut ('$SYS', 'ResolveIP', bool);
        END (*IF*);

        (* Language has already been stored. *)

        CloseINIFile;

    END StoreData;

(**************************************************************************)

PROCEDURE ["SysCall"] DialogueProc (hwnd: OS2.HWND;  msg: OS2.ULONG;
                                      mp1, mp2: OS2.MPARAM): OS2.MRESULT;

    VAR id, code: OS2.USHORT;
        value: CARDINAL;
        NewLangStr: ARRAY [0..LangStringSize] OF CHAR;

    BEGIN
        IF msg = OS2.WM_INITDLG THEN

            OS2.WinSetWindowPos (hwnd, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
            LoadValues (hwnd);
            RETURN NIL;

        ELSIF msg = OS2.WM_PRESPARAMCHANGED THEN

            IF ChangeInProgress THEN
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            ELSE
                ChangeInProgress := TRUE;
                CommonSettings.UpdateFontFrom (hwnd, CommonSettings.MainNotebook);
                ChangeInProgress := FALSE;
                RETURN NIL;
            END (*IF*);

        ELSIF msg = OS2.WM_CONTROL THEN

            id := OS2.USHORT1FROMMP (mp1);
            code := OS2.USHORT2FROMMP (mp1);
            value := OS2.ULONGFROMMP(mp2);
            IF code = OS2.EN_CHANGE THEN
                IF id = DID.Language THEN
                    OS2.WinQueryDlgItemText (hwnd, DID.Language,
                                             LangStringSize+1, NewLangStr);
                    IF CommonSettings.ChangeLanguageTo(NewLangStr) THEN
                        CommonSettings.CurrentLanguage (OurLang, OurLangStr);
                        SetLanguage (OurLang);
                        OS2.WinPostMsg (hwndParent, CommonSettings.LANGCHANGED,
                               OS2.MPFROMLONG(1), OS2.MPFROMLONG(0));
                    END (*IF*);
                    RETURN NIL;
                END (*IF*);
            END (*IF*);

        END (*IF*);

        RETURN OS2.WinDefDlgProc (hwnd, msg, mp1, mp2);

    END DialogueProc;

(**************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  VAR (*OUT*) PageID: CARDINAL): OS2.HWND;

    (* Creates page 1 and adds it to the notebook. *)

    VAR Label: ARRAY [0..31] OF CHAR;

    BEGIN
        UseTNI := CommonSettings.GetINIFilename (INIname);
        notebookhandle := notebook;
        hwndParent := OS2.WinQueryWindow (notebook, OS2.QW_PARENT);
        pagehandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.page1,                (* dialogue ID *)
                       NIL);                 (* creation parameters *)
        PageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         NIL, OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_LAST)));
        OurPageID := PageID;
        Label := "Basic";
        OS2.WinSendMsg (notebook, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(Label));
        OS2.WinSendMsg (notebook, OS2.BKM_SETPAGEWINDOWHWND,
                        CAST(ADDRESS,PageID), CAST(ADDRESS,pagehandle));
        RETURN pagehandle;
    END CreatePage;

(**************************************************************************)

PROCEDURE SetFont (VAR (*IN*) name: CommonSettings.FontName);

    (* Sets the font of the text on this page. *)

    CONST bufsize = CommonSettings.FontNameSize;

    BEGIN
        OS2.WinSetPresParam (pagehandle, OS2.PP_FONTNAMESIZE, bufsize, name);
    END SetFont;

(**************************************************************************)

BEGIN
    INIname := "garbage.INI";
    UseTNI := FALSE;
    ChangeInProgress := FALSE;
    pagehandle := OS2.NULLHANDLE;
    hwndParent := OS2.NULLHANDLE;
    OldMaxClients := 99;
    OldServerPort := 0;
    OldTimeout := 0;
    OldEnable := 0;
    OldLanguage := "";
    OldResolveIP := FALSE;
END SUPage1.


(**************************************************************************)
(*                                                                        *)
(*  Setup for web server                                                  *)
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

IMPLEMENTATION MODULE DomainEditor;

        (************************************************************)
        (*                                                          *)
        (*                  PM Setup for web server                 *)
        (*    The settings notebook for a domain, and its frame     *)
        (*                                                          *)
        (*    Started:        16 April 2015                         *)
        (*    Last edited:    22 May 2017                           *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


(*
FROM FileOps IMPORT             (* for debugging *)
    (* type *)  ChanId,
    (* proc *)  Exists, OpenOldFile, OpenNewFile, CloseFile,
    (* proc *)  FWriteString, FWriteLn;
*)

IMPORT SYSTEM, OS2, OS2RTL, DID, PMInit, Strings, CommonSettings,
       INIData, RINIData, Remote, OneLine;

FROM Names IMPORT
    (* type *)  DomainName, HostNameIndex, HostName, FilenameIndex, FilenameString;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM MiscFuncs IMPORT
    (* type *)  CharArrayPointer;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

(************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    LanguageString = ARRAY [0..31] OF CHAR;

VAR
    OurLang: LangHandle;
    INIname, SetupINIname: ARRAY [0..15] OF CHAR;
    UseTNI: BOOLEAN;
    OurHWND: OS2.HWND;
    DomName: DomainName;
    ChangeInProgress, Changed: BOOLEAN;
    PageFont: CommonSettings.FontName;
    OldHTMLRoot, OldCGIDir: FilenameString;

(************************************************************************)
(*                          TRACE FILE FOR DEBUGGING                    *)
(************************************************************************)

(*
CONST TraceFilename = "TRACE.TXT";
VAR trace: ChanId;

(************************************************************************)

PROCEDURE OpenTraceFile;

    BEGIN
        IF Exists (TraceFilename) THEN
            trace := OpenOldFile (TraceFilename, TRUE, FALSE);
        ELSE
            trace := OpenNewFile (TraceFilename, FALSE);
        END (*IF*);
    END OpenTraceFile;

(************************************************************************)

PROCEDURE CloseTraceFile;

    BEGIN
        CloseFile (trace);
    END CloseTraceFile;

(************************************************************************)

PROCEDURE TraceMessage (message: ARRAY OF CHAR);

    BEGIN
        FWriteString (trace, message);
        FWriteLn (trace);
    END TraceMessage;
*)

(************************************************************************)
(*                       SETTING LABELS AND FONTS                       *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        OurLang := lang;
        StrToBuffer (lang, "DomainEd.Title", stringval);
        OS2.WinSetDlgItemText (OurHWND, DID.DomainDialogue, stringval);
        StrToBuffer (lang, "DomainEd.RootLabel", stringval);
        OS2.WinSetDlgItemText (OurHWND, DID.RootLabel, stringval);
        StrToBuffer (lang, "DomainEd.CGIDirLabel", stringval);
        OS2.WinSetDlgItemText (OurHWND, DID.CGIDirLabel, stringval);
        StrToBuffer (lang, "DomainEd.HostlistLabel", stringval);
        OS2.WinSetDlgItemText (OurHWND, DID.HostlistLabel, stringval);
        StrToBuffer (lang, "Buttons.Add", stringval);
        OS2.WinSetDlgItemText (OurHWND, DID.Addhost, stringval);
        StrToBuffer (lang, "Buttons.Promote", stringval);
        OS2.WinSetDlgItemText (OurHWND, DID.Promotehost, stringval);
        StrToBuffer (lang, "Buttons.Delete", stringval);
        OS2.WinSetDlgItemText (OurHWND, DID.Deletehost, stringval);
        StrToBuffer (lang, "Buttons.OK", stringval);
        OS2.WinSetDlgItemText (OurHWND, DID.DomainDialogueOK, stringval);
        StrToBuffer (lang, "Buttons.Cancel", stringval);
        OS2.WinSetDlgItemText (OurHWND, DID.DomainDialogueCancel, stringval);
    END SetLanguage;

(************************************************************************)

PROCEDURE SetPageFonts;

    (* Changes the font of the notebook pages to the font recorded in   *)
    (* the INI file as belonging to this dialogue.                      *)

    VAR NewFontName: CommonSettings.FontName;

    BEGIN
        CommonSettings.CurrentFont (CommonSettings.DomainDlg, NewFontName);
        IF NOT Strings.Equal (NewFontName, PageFont) THEN
            PageFont := NewFontName;
        END (*IF*);
    END SetPageFonts;

(************************************************************************)
(*                         LOADING DIALOGUE DATA                        *)
(************************************************************************)

PROCEDURE LoadDialogueData (hwnd: OS2.HWND;  domain: ARRAY OF CHAR);

    VAR str: FilenameString;
        state: RINIData.StringReadState;
        name: HostName;
        namesset: BOOLEAN;

    BEGIN
        EVAL (RINIData.OpenINIFile (INIname, UseTNI));

        (* HTML root. *)

        IF RINIData.INIGetString (domain, 'HTMLRoot', OldHTMLRoot) THEN
            str := OldHTMLRoot;
        ELSE
            OldHTMLRoot := "xxx";
            str := ".";
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.HTMLRoot, str);

        (* cgi-bin directory. *)

        IF RINIData.INIGetString (domain, 'CGIDir', OldCGIDir) THEN
            str := OldCGIDir;
        ELSE
            OldCGIDir := "xxx";
            str := "cgi-bin";
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.CGIDir, str);

        (* The list of alternative names for this domain. *)

        IF RINIData.INIFetch (DomName, "namesset", namesset) THEN
            RINIData.GetStringList (DomName, "names", state);
            REPEAT
                RINIData.NextString (state, name);
                IF name[0] <> Nul THEN
                    (* Add name to the listbox. *)

                    OS2.WinSendDlgItemMsg (hwnd, DID.Hostlist, OS2.LM_INSERTITEM,
                         OS2.MPFROMSHORT(OS2.LIT_END), SYSTEM.ADR(name));

                END (*IF*);
            UNTIL name[0] = Nul;
            RINIData.CloseStringList (state);
        ELSE
            (* First time, so add a wildcard entry. *)

            name := "*.";
            Strings.Append (DomName, name);
            OS2.WinSendDlgItemMsg (hwnd, DID.Hostlist, OS2.LM_INSERTITEM,
                         OS2.MPFROMSHORT(OS2.LIT_END), SYSTEM.ADR(name));
        END (*IF*);

        RINIData.CloseINIFile;

    END LoadDialogueData;

(************************************************************************)
(*                           STORING THE DATA                           *)
(************************************************************************)

PROCEDURE StoreList (hwnd: OS2.HWND);

    (* Stores a HostList into the INI file.  The hwnd parameter is the  *)
    (* handle of the listbox.  We assume that the INI file is already   *)
    (* open.                                                            *)

    CONST NameLength = MAX(HostNameIndex)+1;

    VAR bufptr: CharArrayPointer;
        BufferSize: CARDINAL;
        j, k, count, index: CARDINAL;
        name: HostName;
        namesset: BOOLEAN;

    BEGIN
        (* Work out how much buffer space we need. *)

        BufferSize := 0;
        count := OS2.ULONGFROMMR(OS2.WinSendMsg (hwnd, OS2.LM_QUERYITEMCOUNT, NIL, NIL));
        IF count > 0 THEN
            FOR index := 0 TO count-1 DO
                INC (BufferSize,
                     OS2.ULONGFROMMR(OS2.WinSendMsg (hwnd, OS2.LM_QUERYITEMTEXTLENGTH,
                                   OS2.MPFROMUSHORT(index), NIL)) + 1);
            END (*FOR*);
        END (*IF*);

        (* Create the string buffer. *)

        IF BufferSize = 0 THEN
            bufptr := NIL;
        ELSE
            INC (BufferSize);
            ALLOCATE (bufptr, BufferSize);
        END (*IF*);

        (* Store all the strings into the buffer. *)

        IF count > 0 THEN
            j := 0;
            FOR index := 0 TO count-1 DO
                OS2.WinSendMsg (hwnd, OS2.LM_QUERYITEMTEXT,
                                OS2.MPFROM2USHORT(index, NameLength), SYSTEM.ADR(name));
                k := 0;
                REPEAT
                    bufptr^[j] := name[k];
                    INC (k);  INC (j);
                UNTIL (name[k] = Nul) OR (k = NameLength);
                bufptr^[j] := Nul;
                INC (j);
            END (*FOR*);

            bufptr^[j] := Nul;

        END (*IF*);

        (* Write the buffer to the INI file. *)

        IF BufferSize = 0 THEN
            RINIData.INIPutBinary (DomName, "names", j, 0);
        ELSE
            RINIData.INIPutBinary (DomName, "names", bufptr^, BufferSize);
        END (*IF*);
        namesset := TRUE;
        RINIData.INIPut (DomName, "namesset", namesset);

        (* Deallocate the buffer space. *)

        IF BufferSize > 0 THEN
            DEALLOCATE (bufptr, BufferSize);
        END (*IF*);

    END StoreList;

(************************************************************************)

PROCEDURE StoreData (hwnd: OS2.HWND;  domain: ARRAY OF CHAR);

    VAR val: FilenameString;

    BEGIN
        EVAL (RINIData.OpenINIFile (INIname, UseTNI));

        (* HTML root. *)

        OS2.WinQueryDlgItemText (hwnd, DID.HTMLRoot, MAX(FilenameIndex)+1, val);
        IF val <> OldHTMLRoot THEN
            RINIData.INIPutString (domain, "HTMLRoot", val);
        END (*IF*);

        (* cgi-bin directory. *)

        OS2.WinQueryDlgItemText (hwnd, DID.CGIDir, MAX(FilenameIndex)+1, val);
        IF val <> OldCGIDir THEN
            RINIData.INIPutString (domain, "CGIDir", val);
        END (*IF*);

        (* Alternative names for the domain. *)

        StoreList (OS2.WinWindowFromID (hwnd, DID.Hostlist));

        RINIData.CloseINIFile;

    END StoreData;

(************************************************************************)
(*                WINDOW PROCEDURE FOR SUBCLASSED CASE                  *)
(************************************************************************)

PROCEDURE ["SysCall"] SubWindowProc (hwnd     : OS2.HWND;
                                     msg      : OS2.ULONG;
                                     mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    (* Window procedure to intercept some of the things that happen in  *)
    (* the notebook subwindow.  We want this here mainly so that we can *)
    (* detect a new font dropped on the notebook tabs.  If the message  *)
    (* is something we don't want to deal with here, we pass it         *)
    (* to the parent window procedure.                                  *)

    VAR OldWndProc: OS2.PFNWP;
        owner: OS2.HWND;
        length, AttrFound: CARDINAL;
        NewFontName: CommonSettings.FontName;

    BEGIN
        OldWndProc := SYSTEM.CAST (OS2.PFNWP, OS2.WinQueryWindowPtr (hwnd, OS2.QWL_USER));
        owner := OS2.WinQueryWindow(hwnd,OS2.QW_OWNER);

        (* Because of the interaction between subclassing and DragText, *)
        (* some messages will go lost if we use the obvious strategy of *)
        (* sending them through to OldWndProc.  To get around this, we  *)
        (* have to send those messages directly to the target window.   *)

        IF (msg = OS2.WM_BUTTON2DOWN) OR (msg = OS2.DM_DRAGOVER)
                   OR (msg = OS2.DM_DRAGLEAVE) OR (msg = OS2.DM_DROP) THEN

            RETURN OS2.WinSendMsg (owner, msg, mp1, mp2);

        (* Check for font or colour change. *)

        ELSIF msg = OS2.WM_PRESPARAMCHANGED THEN

            IF ChangeInProgress THEN
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            ELSE
                ChangeInProgress := TRUE;
                length := OS2.WinQueryPresParam (hwnd, OS2.PP_FONTNAMESIZE, 0,
                                             AttrFound, CommonSettings.FontNameSize, NewFontName,
                                              0(*OS2.QPF_NOINHERIT*));
                IF length < CommonSettings.FontNameSize THEN
                    NewFontName[length] := Nul;
                END (*IF*);
                ChangeInProgress := FALSE;
                RETURN NIL;
            END (*IF*);

        END (*IF*);

        RETURN OldWndProc (hwnd, msg, mp1, mp2);

    END SubWindowProc;

(************************************************************************)
(*                   WINDOW PROCEDURE FOR MAIN DIALOGUE                 *)
(************************************************************************)

PROCEDURE ["SysCall"] MainDialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR Title: ARRAY [0..255] OF CHAR;
        listwindow: OS2.HWND;
        ButtonID, NotificationCode: CARDINAL;
        index: INTEGER;
        name: ARRAY HostNameIndex OF CHAR;
        message: ARRAY [0..127] OF CHAR;

    BEGIN
        index := OS2.LONGFROMMR(
                   OS2.WinSendDlgItemMsg (hwnd, DID.Hostlist, OS2.LM_QUERYSELECTION, NIL, NIL));
        CASE msg OF
           |  OS2.WM_INITDLG:
                   Title := "DomainEditor";
                   INIData.SetInitialWindowPosition (hwnd, SetupINIname,
                                                     Title, UseTNI);
                   Title := "Domain = ";
                   Strings.Append (DomName, Title);
                   OS2.WinSetWindowText (hwnd, Title);
                   OS2.WinSetWindowPtr (OurHWND, OS2.QWL_USER,
                               SYSTEM.CAST(SYSTEM.ADDRESS,
                                    OS2.WinSubclassWindow (OurHWND,
                                                           SubWindowProc)));
                   OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.Promotehost), FALSE);
                   OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.Deletehost), FALSE);

           |  CommonSettings.FONTCHANGED:

                   IF ChangeInProgress THEN
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   ELSE
                       ChangeInProgress := TRUE;
                       SetPageFonts;
                       ChangeInProgress := FALSE;
                       RETURN NIL;
                   END (*IF*);

           |  OS2.WM_COMMAND:

                   listwindow := OS2.WinWindowFromID(hwnd,DID.Hostlist);
                   ButtonID := OS2.SHORT1FROMMP(mp1);

                   IF ButtonID = DID.DomainDialogueOK THEN
                       StoreData (hwnd, DomName);
                       OS2.WinPostMsg (hwnd, OS2.WM_CLOSE, mp1, mp2)
                   ELSIF ButtonID = DID.DomainDialogueCancel THEN
                       OS2.WinPostMsg (hwnd, OS2.WM_CLOSE, mp1, mp2)

                   ELSIF ButtonID = DID.Addhost THEN
                          name := "";
                          StrToBuffer (OurLang, "Local.EnterName", message);
                          OneLine.Edit (hwnd, message, name, UseTNI);
                          IF name[0] <> Nul THEN
                              IF index = OS2.LIT_NONE THEN
                                  index := 0;
                              ELSE
                                  INC(index);
                              END (*IF*);
                              OS2.WinSendDlgItemMsg (hwnd, DID.Hostlist, OS2.LM_INSERTITEM,
                                     OS2.MPFROMSHORT(index), SYSTEM.ADR(name));
                              OS2.WinSendDlgItemMsg (hwnd, DID.Hostlist, OS2.LM_SELECTITEM,
                                     OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                          END (*IF*);
                          Changed := TRUE;

                   ELSIF ButtonID = DID.Promotehost THEN

                          OS2.WinSendMsg (listwindow, OS2.LM_QUERYITEMTEXT,
                                          OS2.MPFROM2USHORT(index, MAX(HostNameIndex)+1), SYSTEM.ADR(name));
                          OS2.WinSendMsg (listwindow, OS2.LM_DELETEITEM,
                                                 OS2.MPFROMSHORT(index), NIL);
                          DEC (index);
                          OS2.WinSendMsg (listwindow, OS2.LM_INSERTITEM,
                                 OS2.MPFROMSHORT(index), SYSTEM.ADR(name));
                          OS2.WinSendMsg (listwindow, OS2.LM_SELECTITEM,
                                 OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                          Changed := TRUE;

                   ELSIF ButtonID = DID.Deletehost THEN

                          OS2.WinSendDlgItemMsg (hwnd, DID.Hostlist, OS2.LM_DELETEITEM,
                                                 OS2.MPFROMSHORT(index), NIL);
                          OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.Promotehost), FALSE);
                          OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.Deletehost), FALSE);
                          Changed := TRUE;
                   END (*IF*);
                   RETURN NIL;

           |  OS2.WM_CONTROL:

                   NotificationCode := OS2.ULONGFROMMP(mp1);
                   ButtonID := NotificationCode MOD 65536;
                   NotificationCode := NotificationCode DIV 65536;

                   IF ButtonID = DID.Hostlist THEN
                       IF NotificationCode = OS2.LN_SELECT THEN

                           (* For some reason the more obvious code doesn't work below, so     *)
                           (* we have to use an if/then/else construct.                        *)

                           IF index > 0 THEN
                               OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.Promotehost), TRUE);
                           ELSE
                               OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.Promotehost), FALSE);
                           END (*IF*);
                           OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.Deletehost), TRUE);
                           RETURN NIL;
                       ELSIF NotificationCode = OS2.LN_ENTER THEN
                           (* Treat this one as if the OK button had been clicked. *)
                           OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                                 OS2.MPFROMSHORT(DID.DomainDialogueOK), NIL);
                           RETURN NIL;
                       ELSE
                           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                       END (*IF*);
                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*IF*);

           |  OS2.WM_CLOSE:
                   CommonSettings.EnableFontChanges(FALSE);
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

        ELSE    (* default *)
           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);
        RETURN NIL;
    END MainDialogueProc;

(**********************************************************************)
(*                            MAIN PROGRAM                            *)
(**********************************************************************)

PROCEDURE Edit (owner: OS2.HWND;  VAR (*INOUT*) name: ARRAY OF CHAR);

    (* Opens and allows editing of the main dialogue box for this domain. *)

    VAR Lang: LangHandle;
        LangName: LanguageString;

    BEGIN
        OS2.DosError (OS2.FERR_DISABLEHARDERR);
        UseTNI := CommonSettings.GetINIFilename (INIname);
        Strings.Assign (name, DomName);
        Changed := FALSE;
        OurHWND := OS2.WinLoadDlg(OS2.HWND_DESKTOP, owner,
                       MainDialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.DomainDialogue,                (* dialogue ID *)
                       NIL);               (* creation parameters *)
        Remote.SetInitialWindowPosition (OurHWND, "DomainDlg");
        CommonSettings.CurrentLanguage (Lang, LangName);
        SetLanguage (Lang);
        LoadDialogueData (OurHWND, name);

        OS2.WinProcessDlg(OurHWND);

        Remote.StoreWindowPosition (OurHWND, "DomainDlg", TRUE);
        OS2.WinDestroyWindow (OurHWND);
    END Edit;

(************************************************************************)

BEGIN
    SetupINIname := "Setup.INI";
    ChangeInProgress := FALSE;
    UseTNI := FALSE;
END DomainEditor.


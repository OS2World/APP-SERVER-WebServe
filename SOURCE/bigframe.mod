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

IMPLEMENTATION MODULE BigFrame;

        (************************************************************)
        (*                                                          *)
        (*                   PM Setup for web server                *)
        (*             The settings notebook and its frame          *)
        (*                                                          *)
        (*    Started:        4 April 2015                          *)
        (*    Last edited:    22 May 2017                           *)
        (*    Status:         Working                               *)
        (*                                                          *)
        (************************************************************)


IMPORT SYSTEM;

IMPORT OS2, OS2RTL, Strings;

IMPORT DID, SUPage1, SUMIME, DomainPage, SULogging, About, CommonSettings,
            INIData, RINIData;

FROM Names IMPORT
    (* type *)  FilenameString, UserName, UserNameIndex, DomainName;

FROM Remote IMPORT
    (* proc *)  SelectRemoteFile, StoreWindowPosition;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer, StrToBufferA;

FROM MiscFuncs IMPORT
    (* type *)  CharArrayPointer,
    (* proc *)  EVAL;

FROM PMInit IMPORT
    (* proc *)  WarningBox;

FROM LowLevel IMPORT
    (* proc *)  IAND;

(**************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    LanguageString = ARRAY [0..31] OF CHAR;
    Page = (pbase, pdomains, plog, pabout);

VAR
    (* INI file name for the Setup INI *)

    SetupINIname: ARRAY [0..9] OF CHAR;

    pagehandle: ARRAY Page OF OS2.HWND;
    IDofPage: ARRAY Page OF CARDINAL;
    ChangeInProgress: BOOLEAN;
    NewStyle: BOOLEAN;

    (* Use TNI files rather than INI files. *)

    UseTNI: BOOLEAN;

    (* Page number for the initial page to display. *)

    StartingPage: Page;

    (* Fonts and language. *)

    PageFont, TabFontName: CommonSettings.FontName;
    OurLanguage: LanguageString;
    LangCode: LangHandle;

(**************************************************************************)

PROCEDURE SetLanguage;

    (* Changes the language of the notebook pages to the current language *)
    (* as recorded by module CommonSettings.                              *)

    VAR NewName: LanguageString;

    BEGIN
        CommonSettings.CurrentLanguage (LangCode, NewName);
        IF NOT Strings.Equal(NewName, OurLanguage) THEN
            SUPage1.SetLanguage (LangCode);
            SULogging.SetLanguage (LangCode);
            DomainPage.SetLanguage (LangCode);
            About.SetLanguage (LangCode);
        END (*IF*);
        OurLanguage := NewName;
    END SetLanguage;

(**************************************************************************)

PROCEDURE SetPageFonts (UpdateAll: BOOLEAN);

    (* Changes the font of the notebook pages to the font recorded in the *)
    (* INI file as belonging to this notebook.                            *)

    VAR NewFontName: CommonSettings.FontName;

    BEGIN
        CommonSettings.CurrentFont (CommonSettings.MainNotebook, NewFontName);
        IF NOT Strings.Equal (NewFontName, PageFont) THEN
            PageFont := NewFontName;

            IF UpdateAll THEN
                SUPage1.SetFont (PageFont);
                SULogging.SetFont (PageFont);
                DomainPage.SetFont (PageFont);
                About.SetFont (PageFont);
            END (*IF*);
        END (*IF*);

    END SetPageFonts;

(**************************************************************************)

PROCEDURE MakeNotebookNewStyle (hwnd: OS2.HWND;  NewStyle: BOOLEAN);

    (* Change to Warp 3 or Warp 4 notebook style. *)

    CONST
        OldStyleFlags = OS2.BKS_BACKPAGESBR + OS2.BKS_MAJORTABBOTTOM
                + OS2.BKS_ROUNDEDTABS + OS2.BKS_TABTEXTCENTER
                + OS2.BKS_STATUSTEXTCENTER + OS2.BKS_SPIRALBIND;
        NewStyleFlags = OS2.BKS_TABBEDDIALOG + OS2.BKS_MAJORTABTOP + OS2.BKS_BACKPAGESTR;

    VAR style: CARDINAL;

    BEGIN
        style := OS2.WinQueryWindowULong (hwnd, OS2.QWL_STYLE);
        style := IAND (style, 0FFFF0000H);
        IF NewStyle THEN
            INC (style, NewStyleFlags);
        ELSE
            INC (style, OldStyleFlags);
        END (*IF*);
        OS2.WinSetWindowULong (hwnd, OS2.QWL_STYLE, style);
    END MakeNotebookNewStyle;

(**************************************************************************)

PROCEDURE InitialiseNotebook (hwnd: OS2.HWND);

    (* hwnd is the handle of the notebook control. *)

    VAR swp: OS2.SWP;  scale: CARDINAL;
        owner: OS2.HWND;

    BEGIN
        (* Find OS version to decide what notebook style to use. *)

        scale := 30;
        OS2.DosQuerySysInfo(12, 12, SYSTEM.ADR(scale), SIZE(CARDINAL));
        NewStyle := scale >= 40;
        MakeNotebookNewStyle (hwnd, NewStyle);

        OS2.WinSetPresParam (hwnd, OS2.PP_FONTNAMESIZE,CommonSettings.FontNameSize, TabFontName);

        (* If the new style is enabled, the following code will have no effect *)
        (* because the messages to set tab size and colours will be ignored.   *)

        OS2.WinQueryWindowPos (hwnd, swp);
        scale := 2*swp.cx DIV 13;
        IF NewStyle THEN
            OS2.WinSendMsg (hwnd, OS2.BKM_SETDIMENSIONS,
                 OS2.MPFROM2SHORT(scale,5*scale DIV 12), OS2.MPFROMSHORT(OS2.BKA_MAJORTAB));
            OS2.WinSendMsg (hwnd, OS2.BKM_SETNOTEBOOKCOLORS,
                            OS2.MPFROMLONG(00FFFFAAH(*0055DBFFH*)), OS2.MPFROMLONG(OS2.BKA_BACKGROUNDPAGECOLOR));
            OS2.WinSendMsg (hwnd, OS2.BKM_SETNOTEBOOKCOLORS,
                            OS2.MPFROMLONG(0080DBAAH), OS2.MPFROMLONG(OS2.BKA_BACKGROUNDMAJORCOLOR));
        END (*IF*);

        (* Create the notebook pages. *)

        pagehandle[pbase] := SUPage1.CreatePage(hwnd, IDofPage[pbase]);
        pagehandle[pdomains] := DomainPage.CreatePage(hwnd, IDofPage[pdomains]);
        pagehandle[plog] := SULogging.CreatePage(hwnd, IDofPage[plog]);
        SUMIME.DeleteObsoleteEntries;
        About.Create(hwnd, IDofPage[pabout]);

        CommonSettings.EnableFontChanges(TRUE);
        SetPageFonts (TRUE);
        SetLanguage;
        OS2.WinSendMsg (hwnd, OS2.BKM_TURNTOPAGE,
                           OS2.MPFROMULONG(IDofPage[StartingPage]), NIL);
        OS2.WinShowWindow (hwnd, TRUE);
        CommonSettings.EnableFontChanges(TRUE);

        (* The parent of this window is the frame.  The owner of that   *)
        (* frame is the window we want to hide.  For neatness, we also  *)
        (* blank out the previous status message in case the window     *)
        (* becomes unhidden again.                                      *)

        owner := OS2.WinQueryWindow (hwnd, OS2.QW_PARENT);
        owner := OS2.WinQueryWindow (owner, OS2.QW_OWNER);
        OS2.WinSetDlgItemText (owner, DID.Status, "");
        OS2.WinShowWindow (owner, FALSE);

    END InitialiseNotebook;

(**************************************************************************)
(*                WINDOW PROCEDURE FOR SUBCLASSED CASE                    *)
(**************************************************************************)

PROCEDURE ["SysCall"] SubWindowProc (hwnd     : OS2.HWND;
                                     msg      : OS2.ULONG;
                                     mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    (* Window procedure to intercept some of the things that happen in  *)
    (* the notebook subwindow.  We want this here mainly so that we can *)
    (* detect a new font dropped on the notebook tabs.  If the message  *)
    (* is something we don't want to deal with here, we pass it         *)
    (* to the parent window procedure.                                  *)

    VAR OldWndProc: OS2.PFNWP;
        owner: OS2.HWND;  hini: INIData.HINI;
        length, AttrFound: CARDINAL;
        NewFontName: CommonSettings.FontName;
        app: ARRAY [0..4] OF CHAR;
        key: ARRAY [0..16] OF CHAR;

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

                IF NOT Strings.Equal (NewFontName, TabFontName) THEN
                    TabFontName := NewFontName;
                    hini := INIData.OpenINIFile (SetupINIname, UseTNI);
                    app := "Font";
                    key := "MainNotebookTabs";
                    INIData.INIPutString (hini, app, key, TabFontName);
                    INIData.CloseINIFile (hini);
                    OS2.WinSetPresParam (hwnd, OS2.PP_FONTNAMESIZE,CommonSettings.FontNameSize, TabFontName);
                END (*IF*);
                ChangeInProgress := FALSE;
                RETURN NIL;
            END (*IF*);

        END (*IF*);

        RETURN OldWndProc (hwnd, msg, mp1, mp2);

    END SubWindowProc;

(**************************************************************************)
(*                   WINDOW PROCEDURE FOR MAIN DIALOGUE                   *)
(**************************************************************************)

PROCEDURE ["SysCall"] MainDialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR bookwin: OS2.HWND;  lang: LangHandle;
        hini: INIData.HINI;  pageID: CARDINAL;  pg: Page;
        stringval: ARRAY [0..255] OF CHAR;
        app: ARRAY [0..12] OF CHAR;
        key: ARRAY [0..12] OF CHAR;

    BEGIN
        bookwin := OS2.WinWindowFromID (hwnd, DID.notebook);
        CASE msg OF
           |  OS2.WM_INITDLG:
                   stringval := "MainNotebook";
                   INIData.SetInitialWindowPosition (hwnd, SetupINIname,
                                                     stringval, UseTNI);
                   CommonSettings.CurrentLanguage (lang, stringval);

                   (* We need the language to put a label on the frame. *)

                   IF RINIData.RemoteOperation() THEN
                       IF SelectRemoteFile("WebServe.INI") THEN
                           StrToBuffer (lang, "Main.remote", stringval);
                       ELSE
                           StrToBuffer (lang, "Main.cantopen", stringval);
                       END (*IF*);
                   ELSE
                       StrToBuffer (lang, "Main.local", stringval);
                   END (*IF*);
                   OS2.WinSetWindowText (hwnd, stringval);
                   InitialiseNotebook (bookwin);
                   OS2.WinSetWindowPtr (bookwin, OS2.QWL_USER,
                               SYSTEM.CAST(SYSTEM.ADDRESS,
                                    OS2.WinSubclassWindow (bookwin,
                                                           SubWindowProc)));
                   RETURN NIL;

           |  CommonSettings.FONTCHANGED:

                   IF ChangeInProgress THEN
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   ELSE
                       ChangeInProgress := TRUE;
                       SetPageFonts (TRUE);
                       ChangeInProgress := FALSE;
                       RETURN NIL;
                   END (*IF*);

           |  CommonSettings.LANGCHANGED:

                   IF ChangeInProgress THEN
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   ELSE
                       ChangeInProgress := TRUE;
                       CommonSettings.CurrentLanguage (lang, stringval);
                       IF RINIData.RemoteOperation() THEN
                           StrToBuffer (lang, "Main.remote", stringval);
                       ELSE
                           StrToBuffer (lang, "Main.local", stringval);
                       END (*IF*);
                       OS2.WinSetWindowText (hwnd, stringval);
                       SetLanguage;
                       ChangeInProgress := FALSE;
                       RETURN NIL;
                   END (*IF*);

           |  OS2.WM_CLOSE:
                   CommonSettings.EnableFontChanges(FALSE);

                   (* Work out our current page. *)

                   bookwin := OS2.WinWindowFromID(hwnd, DID.notebook);
                   pageID := OS2.ULONGFROMMR(OS2.WinSendMsg (bookwin, OS2.BKM_QUERYPAGEID,
                                 OS2.MPFROMULONG(0),
                                  OS2.MPFROMSHORT(OS2.BKA_TOP)));
                   pg := MAX(Page);
                   WHILE (IDofPage[pg] <> pageID) AND (pg > MIN(Page)) DO
                       DEC (pg);
                   END (*WHILE*);

                   (* Store the current page and window position.  We don't *)
                   (* need to save the font for tabs because SubWindowProc  *)
                   (* looks after that.                                     *)

                   hini := INIData.OpenINIFile (SetupINIname, UseTNI);
                   app := "StartingPage";
                   key := "MainNotebook";
                   INIData.INIPut (hini, app, key, pg);
                   (*
                   app := "Font";
                   INIData.INIPutString (hini, app, "MainNotebookTabs", TabFontName);
                   *)
                   INIData.CloseINIFile (hini);
                   StoreWindowPosition (hwnd, "MainNotebook", FALSE);

                   (* Store the data for each notebook page. *)

                   SUPage1.StoreData (pagehandle[pbase]);
                   DomainPage.StoreData;
                   SULogging.StoreData;
                   (* Nothing to store on About page *)
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

        ELSE    (* default *)
           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);

    END MainDialogueProc;

(**************************************************************************)

PROCEDURE OpenBigFrame (owner: OS2.HWND;  TNImode: BOOLEAN);

    (* Creates the main dialogue box. *)

    VAR hini: INIData.HINI;
        app: ARRAY [0..12] OF CHAR;
        key: ARRAY [0..16] OF CHAR;

    BEGIN
        UseTNI := TNImode;
        IF UseTNI THEN
           SetupINIname := "Setup.TNI";
        ELSE
           SetupINIname := "Setup.INI";
        END (*IF*);
        ChangeInProgress := FALSE;
        CommonSettings.EnableFontChanges(FALSE);
        CommonSettings.SetINIFilename (UseTNI);

        (* Set the initial tab font and starting page. *)

        hini := INIData.OpenINIFile (SetupINIname, UseTNI);
        app := "Font";
        key := "MainNotebookTabs";
        IF NOT INIData.INIGetString (hini, app, key, TabFontName) THEN
            TabFontName := "8.Helv";
        END (*IF*);
        app := "StartingPage";
        key := "MainNotebook";
        IF (NOT INIData.INIGet(hini, app, key, StartingPage))
                   OR (StartingPage > MAX(Page)) THEN
            StartingPage := MIN(Page);
        END (*IF*);
        INIData.CloseINIFile (hini);

        (* Execute the dialogue. *)

        OS2.WinDlgBox(OS2.HWND_DESKTOP, owner,
                       MainDialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.BigFrame,        (* dialogue ID *)
                       NIL);                (* creation parameters *)
    END OpenBigFrame;

(**************************************************************************)

VAR pg: Page;

BEGIN
    UseTNI := FALSE;
    SetupINIname := "Setup.INI";
    ChangeInProgress := FALSE;
    PageFont := "";
    TabFontName := "";
    OurLanguage := "";
    LangCode := NIL;
    StartingPage := MIN(Page);
    FOR pg := MIN(Page) TO MAX(Page) DO
        IDofPage[pg] := 0;
    END (*FOR*);
END BigFrame.


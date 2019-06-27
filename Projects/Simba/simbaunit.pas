{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    Simba: GUI for the Mufasa Macro Library
}
unit SimbaUnit;

{$mode objfpc}{$H+}

{$I Simba.inc}

interface

uses
  {$IFDEF LINUX}cthreads, cmem, pthreads,{$ENDIF}
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, ComCtrls, ExtCtrls, SynEdit, SynHighlighterLape,
  mufasabase, MufasaTypes,
  synedittypes,
  script_thread, file_browser,

  {$IFDEF MSWINDOWS} os_windows, windows, {$ENDIF} //For ColorPicker etc.
  {$IFDEF LINUX} os_linux, {$ENDIF} //For ColorPicker etc.

  colourpicker, windowselector, Clipbrd, // We need these for the Colour Picker and Window Selector

  framescript,

  lcltype, ActnList,
  SynExportHTML, SynEditKeyCmds,
  SynEditMarkupHighAll, LMessages, Buttons, PairSplitter,
  mmisc, stringutil, mufasatypesutil,
  about, framefunctionlist, updateform, Simbasettingsold,
  Simbasettingssimple,
  v_ideCodeInsight, v_ideCodeParser, CastaliaPasLexTypes, // Code completion units
  CastaliaSimplePasPar, v_AutoCompleteForm,  // Code completion units
  
  virtualextension, extensionmanager,

  updater, simba.package_form,
  {$IFDEF USE_SCRIPTMANAGER}SM_Main,{$ENDIF}
  newsimbasettings;

const
  { Place the shortcuts here }
  {$IFDEF LINUX}
  shortcut_StartScript = '<Ctrl><Alt>R';
  shortcut_StopScript =  '<Ctrl><Alt>S';
  shortcut_PickColour =  '<Ctrl><Alt>P';
  {$ELSE}
  shortcut_StartScriptMod = MOD_CONTROL or MOD_ALT;
  shortcut_StartScriptKey = VK_R;
  shortcut_StopScriptMod  = MOD_CONTROL or MOD_ALT;
  shortcut_StopScriptKey  = VK_S;
  shortcut_PickColourMod  = MOD_CONTROL or MOD_ALT;
  shortcut_PickColourKey  = VK_P;
  {$ENDIF}

  {$I settings_const.inc}

type

  { TMufasaTab }

  TMufasaTab = class(Tobject)
  private
    PageCtrl : TPageControl;
  public
    TabSheet : TTabsheet;
    ScriptFrame : TScriptFrame;
    procedure Clear;//This will 'reset' the ScriptFrame
    constructor Create(Page : TPageControl);
    destructor Destroy; override;
  end;

  { TSimbaForm }

  TSimbaForm = class(TForm)
    ActionCodeComment: TAction;
    ActionColors: TAction;                     
    ActionFileBrowser: TAction;
    ActionFindPrev: TAction;
    ActionFont: TAction;
    ActionNotes: TAction;
    ActionDebugger: TAction;
    ActionLape: TAction;
    ActionGoto: TAction;
    ActionPascalScript: TAction;
    ActionExtensions: TAction;
    ActionSaveDef: TAction;
    ActionConsole: TAction;
    ActionCompileScript: TAction;
    ActionExit: TAction;
    ActionReplace: TAction;
    ActionFindNext: TAction;
    ActionRedo: TAction;
    ActionUndo: TAction;
    ActionSelectAll: TAction;
    ActionDelete: TAction;
    ActionPaste: TAction;
    ActionCopy: TAction;
    ActionCut: TAction;
    ActionFindStart: TAction;
    ActionClearDebug: TAction;
    ActionSaveAll: TAction;
    ActionStopScript: TAction;
    ActionSaveScript: TAction;
    ActionSaveScriptAs: TAction;
    ActionRunScript: TAction;
    ActionPauseScript: TAction;
    ActionNewScript: TAction;
    ActionOpenScript: TAction;
    ActionNewTab: TAction;
    ActionCloseTab: TAction;
    ActionTabLast: TAction;
    ActionTabNext: TAction;
    ActionList: TActionList;
    CheckBoxMatchCase: TCheckBox;
    FunctionList: TFunctionList_Frame;
    lblNotes: TLabel;
    FileBrowserLabel: TLabel;
    LabeledEditSearch: TLabeledEdit;
    MainMenu: TMainMenu;
    DebugMemo: TMemo;
    MenuDTMEditor: TMenuItem;
    MenuItem1: TMenuItem;
    MenuColors: TMenuItem;
    MenuItemColors: TMenuItem;
    MenuItemUnloadPlugin: TMenuItem;
    MenuItemDivider12: TMenuItem;
    FileBrowser_PopupItem_Open: TMenuItem;
    FileBrowser_PopupItem_OpenExternally: TMenuItem;
    NotesMemo: TMemo;
    MenuItemFileBrowser: TMenuItem;
    MenuItemACA: TMenuItem;
    MenuItemFindPrev: TMenuItem;
    MenuItemFont: TMenuItem;
    MenuItemDivider13: TMenuItem;
    MenuItemNotes: TMenuItem;
    MenuFile: TMenuItem;
    MenuEdit: TMenuItem;
    MenuHelp: TMenuItem;
    MenuItemFormDesigner: TMenuItem;
    MenuItemSettingsSimpleButton: TMenuItem;
    MenuItemReadOnlyTab: TMenuItem;
    MenuItemGoto: TMenuItem;
    MenuItemDivider14: TMenuItem;
    MenuItemOpenPluginsFolder: TMenuItem;
    MenuItemOpenIncludesFolder: TMenuItem;
    MenuItemOpenScriptsFolder: TMenuItem;
    MenuItemDivider11: TMenuItem;
    MenuItemSaveDef: TMenuItem;
    MenuItemBitmapConv: TMenuItem;
	MenuItemExtensions: TMenuItem;
    MenuItemSettingsButton: TMenuItem;
    MenuItemDivider10: TMenuItem;
    MenuTools: TMenuItem;
    MenuItemOpenRecent: TMenuItem;
    MenuItemCompile: TMenuItem;
    MenuItemHandbook: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemReportBug: TMenuItem;
    MenuItemExportHTML: TMenuItem;
    MenuItemDivider9: TMenuItem;
    MouseTimer: TTimer;
    PanelUtilites: TPairSplitter;
    FileBrowserPanel: TPairSplitterSide;
    PanelNotes: TPairSplitterSide;
    FileBrowserPopup: TPopupMenu;
    FileBrowserRefreshButton: TSpeedButton;
    SpeedButtonFindNext: TSpeedButton;
    SpeedButtonFindPrev: TSpeedButton;
    UtilitesSplitter: TSplitter;
    ToolButton5: TToolButton;
    TT_ScriptManager: TToolButton;
    ToolButton6: TToolButton;
    TB_Console: TToolButton;
    TT_Cut: TToolButton;
    TT_Copy: TToolButton;
    TT_Paste: TToolButton;
    ToolButton9: TToolButton;
    UpdateTimer: TTimer;
    ToolButton3: TToolButton;
    TT_Update: TToolButton;
    UpdateMenuButton: TMenuItem;
    MenuItemFunctionList: TMenuItem;
    MenuItemHide: TMenuItem;
    MenuItemDebugImage: TMenuItem;
    MenuItemMainExit: TMenuItem;
    MenuItemDivider6: TMenuItem;
    PopupItemReplace: TMenuItem;
    MenuItemReplace: TMenuItem;
    dlgReplace: TReplaceDialog;
    MenuItemColourHistory: TMenuItem;
    MenuView: TMenuItem;
    MenuItemFindNext: TMenuItem;
    PopupItemDelete: TMenuItem;
    MenuItemDelete: TMenuItem;
    MenuItemDivider5: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    PopupItemSelectAll: TMenuItem;
    PopupItemDivider2: TMenuItem;
    PopupItemPaste: TMenuItem;
    PopupItemCopy: TMenuItem;
    PopupItemCut: TMenuItem;
    PopupItemDivider1: TMenuItem;
    PopupItemRedo: TMenuItem;
    PopupItemUndo: TMenuItem;
    PopupItemDivider3: TMenuItem;
    PopupItemFind: TMenuItem;
    MenuItemFind: TMenuItem;
    MenuItemDivider4: TMenuItem;
    MenuItemDivider3: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemSaveAll: TMenuItem;
    MenuItemTabCloseOthers: TMenuItem;
    MenuItemTabAdd: TMenuItem;
    MenuItemTabClose: TMenuItem;
    MenuItemCloseTabs: TMenuItem;
    MenuItemCloseTab: TMenuItem;
    MenuItemNewTab: TMenuItem;
    MenuItemDivider2: TMenuItem;
    PageControl1: TPageControl;
    ScriptPopup: TPopupMenu;
    SearchPanel: TPanel;
    ScriptPanel: TPanel;
    SpeedButtonSearch: TSpeedButton;
    SplitterFunctionList: TSplitter;
    TabPopup: TPopupMenu;
    TB_SaveAll: TToolButton;
    TrayDivider: TMenuItem;
    TrayPlay: TMenuItem;
    TrayStop: TMenuItem;
    TrayPause: TMenuItem;
    MenuItemPause: TMenuItem;
    MenuItemStop: TMenuItem;
    MenuItemShow: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemNew: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemRedo: TMenuItem;
    MenuItemUndo: TMenuItem;
    MenuItemSave: TMenuItem;
    Mufasa_Image_List: TImageList;
    MenuItemScript: TMenuItem;
    MenuItemRun: TMenuItem;
    PanelMemo: TPanel;
    SplitterMemoSynedit: TSplitter;
    TrayPopup: TPopupMenu;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    TB_Run: TToolButton;
    TB_Pause: TToolButton;
    TB_Stop: TToolButton;
    ToolButton1: TToolButton;
    TB_Tray: TToolButton;
    TB_NewTab: TToolButton;
    TB_CloseTab: TToolButton;
    TB_New: TToolButton;
    ToolButton2: TToolButton;
    TB_Open: TToolButton;
    TB_Save: TToolButton;
    ToolButton4: TToolButton;
    TB_ClearDebug: TToolButton;
    TB_PickColour: TToolButton;
    TB_SelectClient: TToolButton;
    ToolButton8: TToolButton;
    MTrayIcon: TTrayIcon;
    FunctionListHint: THintWindow;
    procedure ActionCodeCommentExecute(Sender: TObject);
    procedure ActionColorsExecute(Sender: TObject);
    procedure ActionClearDebugExecute(Sender: TObject);
	procedure ActionExtensionsExecute(Sender: TObject);
	procedure ActionExtensionsUpdate(Sender: TObject);
    procedure ActionCloseTabExecute(Sender: TObject);
    procedure ActionCompileScriptExecute(Sender: TObject);
    procedure ActionConsoleExecute(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionCutExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionFindNextExecute(Sender: TObject);
    procedure ActionFindPrevExecute(Sender: TObject);
    procedure ActionFindStartExecute(Sender: TObject);
    procedure ActionFontExecute(Sender: TObject);
    procedure ActionGotoExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionNewTabExecute(Sender: TObject);
    procedure ActionFileBrowserExecute(Sender: TObject);
    procedure ActionNotesExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionPauseExecute(Sender: TObject);
    procedure ActionRedoExecute(Sender: TObject);
    procedure ActionReplaceExecute(Sender: TObject);
    procedure ActionRunExecute(Sender: TObject);
    procedure ActionSaveAllExecute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ActionSaveDefExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure ActionStopExecute(Sender: TObject);
    procedure ActionTabLastExecute(Sender: TObject);
    procedure ActionTabNextExecute(Sender: TObject);
    procedure ActionUndoExecute(Sender: TObject);
    procedure ChangeMouseStatus(Sender: TObject);
    procedure CheckBoxMatchCaseClick(Sender: TObject);
    procedure CloseFindPanel;
    procedure doOnHide(Sender: TObject);
    procedure FileBrowserDoubleClick(Sender: TObject);
    procedure FileBrowser_Popup_OnPopup(Sender: TObject);
    procedure FileBrowserRefresh(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure DebugMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ShowDTMEditor(Sender: TObject);
    procedure ShowFormDesigner(Sender: TObject);
    procedure UnloadPlugin(Sender: TObject);
    procedure MenuToolsClick(Sender: TObject);
    procedure FileBrowser_Popup_Open(Sender: TObject);
    procedure FileBrowser_Popup_OpenExternally(Sender: TObject);
    procedure MenuItemReadOnlyTabClick(Sender: TObject);
    procedure MenuItemBitmapConvClick(Sender: TObject);
    procedure MenuItemHandbookClick(Sender: TObject);
    procedure MenuItemColourHistoryClick(Sender: TObject);
    procedure dlgReplaceFind(Sender: TObject);
    procedure dlgReplaceReplace(Sender: TObject);
    procedure EditSearchChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShortCuts(var Msg: TLMKey; var Handled: Boolean);
    procedure LabeledEditSearchEnter(Sender: TObject);
    procedure LabeledEditSearchExit(Sender: TObject);
    procedure LabeledEditSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LabeledEditSearchKeyPress(Sender: TObject; var Key: char);
    procedure MenuEditClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemCloseTabsClick(Sender: TObject);
    procedure MenuItemDebugImageClick(Sender: TObject);
    procedure MenuItemExportHTMLClick(Sender: TObject);
    procedure MenuitemFillFunctionListClick(Sender: TObject);
    procedure MenuItemHideClick(Sender: TObject);
    procedure MenuItemOpenIncludesFolderClick(Sender: TObject);
    procedure MenuItemOpenPluginsFolderClick(Sender: TObject);
    procedure MenuItemOpenScriptsFolderClick(Sender: TObject);
    procedure MenuItemReportBugClick(Sender: TObject);
    procedure MenuItemSettingsButtonClick(Sender: TObject);
    procedure MenuItemSettingsSimpleButtonClick(Sender: TObject);
    procedure MenuItemShowClick(Sender: TObject);
    procedure MenuItemTabCloseClick(Sender: TObject);
    procedure MenuItemTabCloseOthersClick(Sender: TObject);
    procedure MenuItemFunctionListClick(Sender: TObject);
    procedure MTrayIconClick(Sender: TObject);
    procedure ButtonPickClick(Sender: TObject);
    procedure ButtonSelectorDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PageControl1Change(Sender: TObject);
    procedure ButtonTrayClick(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure PageControl1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure PageControl1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PageControl1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure PageControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PageControl1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PopupItemFindClick(Sender: TObject);
    procedure RecentFileItemsClick(Sender: TObject);
    procedure ScriptPanelDockDrop(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer);
    procedure ScriptPanelDockOver(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ScriptPopupPopup(Sender: TObject);
    procedure ShowACA(Sender: TObject);
    procedure SpeedButtonSearchClick(Sender: TObject);
    procedure SplitterFunctionListCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure ThreadOpenConnectionEvent(Sender: TObject; var url: string;
      var Continue: boolean);
    procedure ThreadOpenFileEvent(Sender: TObject; var Filename: string;
      var Continue: boolean);
    procedure ThreadWriteFileEvent(Sender: TObject; var Filename: string;
      var Continue: boolean);
    procedure ScriptStartEvent(Sender: TObject; var Script : string; var Continue : boolean);
    procedure ScriptOpenEvent(Sender: TObject; var Script : string);
    procedure TT_ScriptManagerClick(Sender: TObject);
    procedure TrayPopupPopup(Sender: TObject);
    procedure TT_UpdateClick(Sender: TObject);
    procedure UpdateMenuButtonClick(Sender: TObject);
    procedure UpdateTimerCheck(Sender: TObject);

    procedure OnCCMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: string; X, Y: Integer);
    procedure OnCompleteCode(Str: string);
    function OnCCFindInclude(Sender: TObject; var FileName: string): Boolean;
    function OnCCLoadLibrary(Sender: TObject; var Argument: string; out Parser: TCodeInsight): Boolean;
    procedure UpdateUtilties;
  private
    PopupTab : integer;
    RecentFileItems : array of TMenuItem;
    RecentFiles : TStringList;
    SearchStart : TPoint;
    LastTab  : integer;
    OpenConnectionData : TOpenConnectionData;
    OpenFileData : TOpenFileData;
    WriteFileData : TWriteFileData;
    ScriptStartData : TScriptStartData;
    ScriptOpenData : TScriptOpenData;
    News: String;

    function SilentUpdateBeat: Boolean;

    function GetScriptState: TScriptState;
    function GetShowParamHintAuto: boolean;
    function GetShowCodeCompletionAuto: Boolean;
    procedure GetSimbaNews;
    procedure WriteSimbaNews(Sender: TObject);

    procedure SetSourceEditorFont(obj: TSetting);
    procedure SetTrayVisiblity(obj: TSetting);

    procedure SetShowParamHintAuto(const AValue: boolean);
    procedure SetShowCodeCompletionAuto(const AValue: boolean);
    procedure SetScriptState(const State: TScriptState);
    function CreateSetting(const Key, Value : string) : string;
    procedure SetSetting(const key,Value : string; save : boolean = false);
    function SettingExists(const key : string) : boolean;
    procedure ParseInternals;
    procedure FillFunctionList(Sender: TObject);
    procedure CustomExceptionHandler(Sender: TObject; E: Exception);
    procedure RegisterSettingsOnChanges;
	
	procedure SetExtensionsPath(obj: TSetting);
  public
    { Required to work around using freed resources on quit }
    Exiting: Boolean;

    DebugStream: String;
    SearchString : string;
    CurrScript: TScriptFrame; //The current scriptframe
    CurrTab: TMufasaTab; //The current TMufasaTab
    CodeCompletionForm: TAutoCompletePopup;
    CodeCompletionStart: TPoint;
    ParamHint : TParamHint;
    Tabs: TList;
    Manager: TIOManager;
    Picker: TMColorPicker;
    Selector: TMWindowSelector;
    OnScriptStart : TScriptStartEvent;
    OnScriptOpen : TScriptOpenEvent;
    Highlighter: TSynFreePascalSyn;
    FileBrowser: TSimbaFileBrowser;

    function LoadSettingDef(const Key, Def : string) : string;
    procedure FunctionListShown( ShowIt : boolean);
    property ScriptState : TScriptState read GetScriptState write SetScriptState;
    procedure UpdateTitle;
    function OpenScript : boolean;
    function LoadScriptFile(filename : string; AlwaysOpenInNewTab : boolean = false; CheckOtherTabs : boolean = true) : boolean;
    function SaveCurrentScript : boolean;
    function SaveCurrentScriptAs : boolean;
    function SaveCurrentScriptAsDefault : boolean;
    function CanExitOrOpen : boolean;
    function ClearScript : boolean;
    procedure RunScript;
    procedure CompileScript;
    procedure PauseScript;
    procedure StopScript;
    procedure AddTab;
    procedure StopCodeCompletion;
    function FindTab(filename : string) : integer;
    function DeleteTab( TabIndex : integer; CloseLast : boolean; Silent : boolean = false) : boolean;
    procedure ClearTab( TabIndex : integer);
    procedure CloseTabs(Exclude: integer = -1; Silent : boolean = false); //-1 for no exclusion
    procedure SetEditActions;
    procedure DoSearch(SearchOptions: TSynSearchOptions; HighlightAll: Boolean);
    procedure RefreshTab;//Refreshes all the form items that depend on the Script (Panels, title etc.)
    procedure LoadFormSettings;
    procedure SaveFormSettings;
    procedure AddRecentFile(const filename : string);
    procedure InitializeTMThread(out Thread : TMMLScriptThread);
    procedure DoRefreshTab(Sender: PtrInt);
    procedure DoSimbaUpdateCheck(Sender: PtrInt);
    procedure DoHandleParameters(Data: PtrInt);
    procedure DoRunTest(Data: PtrInt);
    procedure DoParseInternals(Data: PtrInt);
    procedure DoSimbaNews(Data: PtrInt);
    procedure OnSaveScript(const Filename : string);
    property ShowParamHintAuto : boolean read GetShowParamHintAuto write SetShowParamHintAuto;
    property ShowCodeCompletionAuto: Boolean read GetShowCodeCompletionAuto write SetShowCodeCompletionAuto;
    function DefaultScript : string;
    procedure DefaultHighlighter;
    procedure UpdateSimbaSilent(Force: Boolean);
	procedure LoadExtensions;
  end;

  procedure formWriteln(constref S: String);

const
  WindowTitle = 'Simba - %s';//Title, where %s = the place of the filename.

  Panel_State = 0;
  Panel_Coords = 1;
  Panel_ScriptName = 2;
  Panel_ScriptPath = 3;
  Panel_General = 3;


  Image_Stop = 7;
  Image_Terminate = 19;
var
  SimbaForm: TSimbaForm;
  {$IFDEF WINDOWS}
  PrevWndProc : WNDPROC;
  {$ENDIF}

implementation
uses
   LCLIntf,
   LazUTF8,
   LazFileUtils,
   process,
   debugimage,
   files,
   bitmapconv,
   colourhistory,
   math,
   script_imports, script_plugins,
   simba.environment, simba.httpclient,
   aca, dtm_editor, scriptcommenter, colorscheme
   {$IFDEF USE_FORMDESIGNER}, frmdesigner{$ENDIF}
   
   extensionmanagergui,

   {$IFDEF LINUX_HOTKEYS}, keybinder{$ENDIF};

procedure TSimbaForm.CustomExceptionHandler(Sender: TObject; E: Exception);

  function DumpExceptionCallStack(E: Exception): string;
  var
    I: Integer;
    Frames: PPointer;
    Report: string;
  begin
    Report := 'Program exception! ' + LineEnding +
      'Stacktrace:' + LineEnding + LineEnding;
    if E <> nil then begin
      Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
      'Message: ' + E.Message + LineEnding;
    end;
    Report := Report + BackTraceStrFunc(ExceptAddr);
    Frames := ExceptFrames;
    for I := 0 to ExceptFrameCount - 1 do
      Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);

    result := report;
  end;

var
  Trace, LogName: string;
  MsgDlgRet: Integer;

begin
  mDebugLn('');
  mDebugLn('Something went wrong...');
  mDebugLn('');
  
  Trace := DumpExceptionCallStack(E);
  Trace += LineEnding + 'Simba Version: ' + IntToStr(SimbaVersion) + LineEnding;

  LogName := SimbaEnvironment.DataPath + 'ErrorLog_' + FormatDateTime('dd-mm-yy_hh-nn-ss', Now) + '.txt';
  mDebugLn(Format('Going to try to save the error information to "%s".', [LogName]));

  try
    with TFileStream.Create(UTF8ToSys(logname), fmOpenWrite or fmOpenReadWrite or fmCreate) do
    try
      Write(Trace[1], Length(Trace));
    finally
      Free;
    end;
  except
    mDebugLn(Format('Unable to save log file! [%s]', [LogName]));
  end;
  
  MsgDlgRet := mrOk;
  Application.DisableIdleHandler;
  try
     MsgDlgRet := MessageDlg('Something went wrong in Simba. ' +
     'If you press OK, Simba will try to save your scripts and then close. (Recommended) ' +
     'See ' + LogName + ' for more information.' , mtError, mbOKCancel, 0);
  finally
    Application.EnableIdleHandler;
  end;

  if MsgDlgRet = mrOK then
  begin
    try
      Self.CloseTabs; // Save scripts
      SimbaForm.Free;
    finally
      mDebugLn('Finally Free...');
    end;

    { Stop Simba }
    Halt(1); // Error code 1
  end;
end;

{$IFDEF WINDOWS}

{ Used for global callbacks on WINDOWS }
function WndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam;
  lParam: LParam): LRESULT stdcall;
begin
  if uMsg = WM_HOTKEY then //Found an hotkey!
  begin
    if (lo(lParam) = shortcut_StartScriptMod) and (hi(lParam) =shortcut_StartScriptKey) then
      SimbaForm.ActionRunScript.Execute;
    if (lo(lParam) = shortcut_StopScriptMod) and (hi(lParam) =shortcut_StopScriptKey) then
      SimbaForm.ActionStopScript.Execute;
    if (lo(lParam) = shortcut_PickColourMod) and (hi(lParam) =shortcut_PickColourKey) then
      SimbaForm.ButtonPickClick(nil);
    Result := 0;
  end else
    Result := Windows.CallWindowProc(PrevWndProc,Ahwnd, uMsg, WParam, LParam);
end;

procedure Bind_Windows_Keys;

begin
  PrevWndProc := Windows.WNDPROC(GetWindowLong(SimbaForm.handle,GWL_WNDPROC));
  SetWindowLong(SimbaForm.Handle,GWL_WNDPROC,PtrInt(@WndCallback));
  if not RegisterHotkey(SimbaForm.Handle,0,shortcut_StartScriptMod,shortcut_StartScriptKey) then
    mDebugLn('Unable to register start script global hotkey');
  if not RegisterHotkey(SimbaForm.Handle,1,shortcut_StopScriptMod,shortcut_StopScriptKey) then
    mDebugLn('Unable to register stop script global hotkey');
  if not RegisterHotkey(SimbaForm.Handle,2,shortcut_PickColourMod,shortcut_PickColourKey) then
    mDebugLn('Unable to register pick colour global hotkey');
end;

procedure Unbind_Windows_Keys;
var
  i : integer;
begin
  for i := 0 to 2 do
    if not UnRegisterHotkey(SimbaForm.Handle,i) then
      mDebugLn('Unable to unregister '+ inttostr(i) + ' global hotkey');
end;


{$ELSE}
  {$IFDEF LINUX_HOTKEYS}
  {$WARNING This will probably not work if people don't have libkeybinder installed. Perhaps ship it with Simba? }

{ Used for global callbacks on LINUX }
procedure keybinder_callback(keystring: PChar; user_data: PtrUInt); cdecl;
begin
  if keystring = shortcut_StartScript then
    SimbaForm.ActionRunScript.Execute
  else if keystring = shortcut_StopScript then
    SimbaForm.ActionStopScript.Execute
  else if keystring = shortcut_PickColour then
    SimbaForm.ButtonPickClick(nil);
end;

{ XXX, TODO: Pressing the stop shortcut twice (quickly) may crash Simba. }
procedure Bind_Linux_Keys;
begin
  if KeybinderLoaded() then
  begin
    keybinder_init(); { Initialise keybinder }

    { Bind keys }
    if not keybinder_bind(PChar(shortcut_StartScript), @keybinder_callback, PtrUInt(0)) then
      mDebugLn('Unable to register '+ shortcut_StartScript + ' as global hotkey');
    if not keybinder_bind(PChar(shortcut_StopScript), @keybinder_callback, PtrUInt(0)) then
      mDebugLn('Unable to register '+ shortcut_StopScript + ' as global hotkey');
    if not keybinder_bind(PChar(shortcut_PickColour), @keybinder_callback, PtrUInt(0)) then
      mDebugLn('Unable to register '+ shortcut_PickColour + ' as global hotkey');
  end;
end;

procedure Unbind_Linux_Keys;
begin
  if KeybinderLoaded() then
  begin
    keybinder_unbind(PChar(shortcut_StartScript), @keybinder_callback, PtrUInt(0));
    keybinder_unbind(PChar(shortcut_StopScript), @keybinder_callback, PtrUInt(0));
    keybinder_unbind(PChar(shortcut_PickColour), @keybinder_callback, PtrUInt(0));
  end;
end;
  {$ENDIF}

{$ENDIF}


procedure TSimbaForm.OnCCMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: string; X, Y: Integer);
begin
  if (Typ = meNotSupported) then
    Exit;
  if (Sender is TmwSimplePasPar) then
    if (TmwSimplePasPar(Sender).Lexer.TokenID = tok_DONE) then
      Exit;
  mDebugLn('CC ERROR: '+Format('%d:%d %s', [Y + 1, X, Msg])+' in '+TCodeInsight(Sender).FileName);
end;

procedure TSimbaForm.OnCompleteCode(Str: string);
var
  sp, ep: Integer;
  s: string;
begin
  sp := -1;
  ep := -1;

  if (Str <> '') then
  begin
    s := WordAtCaret(CurrScript.SynEdit, sp, ep);
    if (s <> '') then
    begin
      CurrScript.SynEdit.SelStart := CurrScript.SynEdit.SelStart + (sp - CurrScript.SynEdit.CaretX);
      CurrScript.SynEdit.SelEnd := CurrScript.SynEdit.SelStart + (ep - CurrScript.SynEdit.CaretX) + 1;
      CurrScript.SynEdit.SelText := Str;
    end
    else
      CurrScript.SynEdit.InsertTextAtCaret(Str);
  end;
end;

function TSimbaForm.OnCCFindInclude(Sender: TObject; var FileName: string): Boolean;
begin
  Result := FindFile(Filename, [Application.Location, SimbaEnvironment.IncludePath, SimbaSettings.Extensions.Path.Value]);
end;

function TSimbaForm.OnCCLoadLibrary(Sender: TObject; var Argument: string; out Parser: TCodeInsight): Boolean;
var
  Dump: String;
  Plugin: TMPlugin;
  i: Int32;
  MS: TMemoryStream;
begin
  Dump := '';

  try
    Plugin := Plugins.Get(TCodeInsight(Sender).FileName, Argument, False);
    for i := 0 to Plugin.Declarations.Count - 1 do
      Plugin.Declarations[i].Dump(Dump);
  except
    on e: Exception do
      mDebugLn(e.Message);
  end;

  if (Dump <> '') then
  begin
    MS := TMemoryStream.Create();
    MS.Write(Dump[1], Length(Dump));

    Parser := TCodeInsight.Create();
    Parser.FileName := Plugin.FilePath;
    Parser.OnMessage := @OnCCMessage;

    try
      Parser.Run(MS, nil, -1, True);

      Exit(True);
    except
      on e: Exception do
        Parser.Free();
    end;
  end;

  Exit(False);
end;

procedure TSimbaForm.HandleConnectionData;
var
  Args : TVariantArray;
  Called: Boolean;
begin
  SetLength(Args,2);
  Args[0] := OpenConnectionData.URL^;
  Args[1] := OpenConnectionData.Continue^;
  try
    ExtManager.HandleHook(EventHooks[SExt_onOpenConnection].HookName, Args,
        Called);
    OpenConnectionData.URL^ := Args[0];
    OpenConnectionData.Continue^ := Args[1];
  except
    on e : Exception do
      mDebugLn('ERROR in HandleConnectiondata: ' + e.message);
  end;
end;

procedure TSimbaForm.SetSourceEditorFont(obj: TSetting);
var
  I: LongInt;
begin
  if (TFontSetting(obj).Color.Value <> clDefault) then
    TFontSetting(obj).Color.Value := clDefault;

  for I := 0 to Tabs.Count - 1 do
    TMufasaTab(Tabs[I]).ScriptFrame.SynEdit.Font.Assign(TFontSetting(obj).Value);
end;

procedure TSimbaForm.SetExtensionsPath(obj: TSetting);
begin
  {$IFDEF SIMBA_VERBOSE}
  writeln('--- SetExtensionPath with value: ' + TPathSetting(obj).Value);
  {$ENDIF}
end;

procedure TSimbaForm.SetTrayVisiblity(obj: TSetting);
begin
  MTrayIcon.Visible := TBooleanSetting(obj).Value;
end;

procedure TSimbaForm.HandleOpenFileData;
var
  Args : TVariantArray;
  Called: Boolean;
begin
  SetLength(Args,2);
  Args[0] := OpenFileData.FileName^;
  Args[1] := OpenFileData.Continue^;
  try
    ExtManager.HandleHook(EventHooks[SExt_onOpenFile].HookName, Args, Called);
    OpenFileData.FileName^ := Args[0];
    OpenFileData.Continue^ := Args[1];
  except
    on e : Exception do
      mDebugLn('ERROR in HandleOpenFileData: ' + e.message);
  end;
end;

procedure TSimbaForm.HandleWriteFileData;
var
  Args : TVariantArray;
  Called: Boolean;
begin
  SetLength(Args,2);
  Args[0] := WriteFileData.FileName^;
  Args[1] := WriteFileData.Continue^;
  try
    ExtManager.HandleHook(EventHooks[SExt_onWriteFile].HookName, Args, Called);
    WriteFileData.FileName^ := Args[0];
    WriteFileData.Continue^ := Args[1];
  except
    on e : Exception do
      mDebugLn('ERROR in HandleWriteFileData: ' + e.message);
  end;
end;

procedure TSimbaForm.HandleScriptStartData;
var
  Args : TVariantArray;
  s: String;
  Called: Boolean;
begin
  SetLength(Args,2);
  Args[0] := ScriptStartData.Script^;
  Args[1] := ScriptStartData.Continue^;
  try
    s := ExtManager.HandleHook(EventHooks[SExt_onScriptStart].HookName, Args, Called);
    if Called then
    begin
      ScriptStartData.Script^ := s;
      ScriptStartData.Continue^ := Args[1];
    end;
  except
    on e : Exception do
      mDebugLn('ERROR in HandleScriptStartData: ' + e.message);
  end;
end;

procedure TSimbaForm.HandleScriptOpenData;
var
  Args : TVariantArray;
  s: String;
  Called: Boolean;
begin
  SetLength(Args,1);
  Args[0] := ScriptOpenData.Script^;
  try
    s := ExtManager.HandleHook(EventHooks[SExt_onScriptOpen].HookName, Args, Called);
    if Called then
      ScriptOpenData.Script^ := s;
  except
    on e : Exception do
      mDebugLn('ERROR in HandleScriptOpenData: ' + e.message);
  end;
end;

procedure TSimbaForm.RecentFileItemsClick(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to high(RecentFileItems) do
    if RecentFileItems[i] = sender then
    begin;
      LoadScriptFile(RecentFiles[RecentFiles.Count - 1 -i]);//Inverse order
      exit;
    end;
end;

procedure TSimbaForm.ScriptPanelDockDrop(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer);
begin
  if(X <= (ScriptPanel.Width div 2))then
  begin
    FunctionList.Align := alLeft;
    PageControl1.Align := alRight;
    SplitterFunctionList.ResizeAnchor := akLeft;
    SplitterFunctionList.Align := alLeft;
    SplitterFunctionList.Left := FunctionList.Left + FunctionList.Width;
  end else begin
    FunctionList.Align := alRight;
    PageControl1.Align := alLeft;
    SplitterFunctionList.ResizeAnchor := akRight;
    SplitterFunctionList.Align := alRight;
    SplitterFunctionList.Left := FunctionList.Left;
  end;
  PageControl1.Width := ScriptPanel.Width - (Source.DockRect.Right - Source.DockRect.Left);
  FunctionList.Width := ScriptPanel.Width - PageControl1.Width;
  PageControl1.Align := alClient;
  SplitterFunctionList.Show;
end;

procedure TSimbaForm.ScriptPanelDockOver(Sender: TObject; Source: TDragDockObject; //is there a better way to do all of this?
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
   P: TPoint;
begin
  Accept := FunctionList.DragKind = dkDock;
  if(Accept)then
  begin
    P := ScriptPanel.ClientToScreen(Classes.Point(0, 0));
    if(X <= (ScriptPanel.Width div 2))then
      Source.DockRect := Classes.Rect(P.x, P.y, min(P.x + FunctionList.Width, P.x + (ScriptPanel.Width div 2)), P.y + ScriptPanel.Height)
    else
      Source.DockRect := Classes.Rect(max(P.x + ScriptPanel.Width - FunctionList.Width, P.x + (ScriptPanel.Width div 2)), P.y, P.x + ScriptPanel.Width, P.y + ScriptPanel.Height);
  end;
end;

procedure TSimbaForm.ScriptPopupPopup(Sender: TObject);
begin
  SetEditActions;
end;

procedure TSimbaForm.ShowACA(Sender: TObject);
begin
  TACAForm.Create(Manager).Show();
end;

procedure TSimbaForm.SpeedButtonSearchClick(Sender: TObject);
begin
  CloseFindPanel;
end;

procedure TSimbaForm.SplitterFunctionListCanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
begin
  if(NewSize > ScriptPanel.Width div 2)then
    NewSize := ScriptPanel.Width div 2;
end;

procedure TSimbaForm.ThreadOpenConnectionEvent(Sender: TObject; var url: string;var Continue: boolean);
begin
  OpenConnectionData.Sender := Sender;
  OpenConnectionData.URL:= @URL;
  OpenConnectionData.Continue:= @Continue;
end;

procedure TSimbaForm.ThreadOpenFileEvent(Sender: TObject; var Filename: string;
  var Continue: boolean);
begin
  OpenFileData.Sender := Sender;
  OpenFileData.FileName:= @FileName;
  OpenFileData.Continue:= @Continue;
end;

procedure TSimbaForm.ThreadWriteFileEvent(Sender: TObject; var Filename: string;
  var Continue: boolean);
begin
  WriteFileData.Sender := Sender;
  WriteFileData.FileName:= @FileName;
  WriteFileData.Continue:= @Continue;
end;

procedure TSimbaForm.TrayPopupPopup(Sender: TObject);
begin
  { XXX: What's up with this? }
  MenuItemHide.enabled:= SimbaForm.Visible;
  {$ifdef MSWindows}
  MenuItemShow.Enabled:= not SimbaForm.Visible;
  if SimbaForm.Visible then
    if SimbaForm.CanFocus then
      SimbaForm.SetFocus;
  {$endif}
end;

procedure TSimbaForm.TT_UpdateClick(Sender: TObject);
begin
  SimbaUpdateForm.ShowModal;
  TT_Update.Visible:=False;
end;

function TSimbaForm.SilentUpdateBeat: Boolean;
begin
  Application.ProcessMessages;
  Result := False;

  if exiting then
  begin
     mDebugLn('SilentUpdateBeat: Exiting=true; stop update.');
     result := true;
  end;
end;

procedure TSimbaForm.UpdateSimbaSilent(Force: Boolean);

var
  Updater: TMMLFileDownloader;
  LatestVersion: Integer;

begin
  if not Force then
  begin
    LatestVersion:= SimbaUpdateForm.GetLatestSimbaVersion;
    if not (LatestVersion > SimbaVersion) then
    begin
      mDebugLn('Not updating; Force = False and LatestVersion <= SimbaVersion');
      exit;
    end;
  end;

  try
    Updater := TMMLFileDownloader.Create;

    Updater.FileURL := SimbaSettings.Updater.RemoteLink.GetDefValue(
          SimbaURL + 'Simba'{$IFDEF WINDOWS} +'.exe'{$ENDIF}
    );

    Updater.ReplacementFile := ExtractFileName(Application.ExeName);
    Updater.BasePath := ExtractFilePath(Application.ExeName);
    Updater.OnBeat:= @SimbaForm.SilentUpdateBeat;

    try
      Updater.DownloadAndSave;
      Updater.Replace;
      mDebugLn('Simba update succesfull!');
    except
      mDebugLn('Simba update failed!');
    end;
  finally
    Updater.Free;
  end;
end;

procedure TSimbaForm.UpdateTimerCheck(Sender: TObject);
var
   time:integer;
  LatestVersion : integer;
begin
  UpdateTimer.Interval := MaxInt;

  if not SimbaSettings.Updater.CheckForUpdates.GetDefValue(True) then
    Exit;

  LatestVersion:= SimbaUpdateForm.GetLatestSimbaVersion;
  if LatestVersion > SimbaVersion then
  begin
    if SimbaSettings.Updater.AutomaticallyUpdate.GetDefValue(True) then
    begin
      mDebugLn('Performing automatic background update.');
      mDebugLn(format('Current version is %d. Latest version is %d',[SimbaVersion,LatestVersion]));

      UpdateSimbaSilent(True); // Force can be true; we already checked versions.
    end else
    begin
      TT_Update.Visible:=True;
      formWriteLn('A new update of Simba is available!');
      formWriteLn(format('Current version is %d. Latest version is %d',[SimbaVersion,LatestVersion]));
    end;


  end else
  begin
    mDebugLn(format('Current Simba version: %d',[SimbaVersion]));
    mDebugLn('Latest Simba Version: ' + IntToStr(LatestVersion));
  end;
  time := SimbaSettings.Updater.CheckEveryXMinutes.GetDefValue(30);

  UpdateTimer.Interval:= time {mins} * 60 {secs} * 1000 {ms};//Every half hour
end;

procedure TSimbaForm.UpdateMenuButtonClick(Sender: TObject);
begin
  if SimbaUpdateForm.CanUpdate then
    SimbaUpdateForm.ShowModal
  else if MessageDlg('Your Simba seems to be up to date. ' +
         'Do you want to force an update?', mtConfirmation, mbOKCancel, 0) = mrOk then
  begin
    SimbaUpdateForm.ShowModal;
  end;
end;

procedure formWriteln(constref S: String);
begin
  if (GetCurrentThreadId() <> MainThreadID) then
    mDebugLn('formWriteLn called off main thread')
  else
    SimbaForm.DebugMemo.Lines.Add(s);
end;

procedure TSimbaForm.RunScript;
begin
  case ScriptState of
    ss_Paused:
      begin
        CurrScript.ScriptThread.State := ssRun;

        ScriptState := ss_Running;
      end;

   ss_None:
    begin
      InitializeTMThread(CurrScript.ScriptThread);

      if (CurrScript.ScriptThread <> nil) then
      begin
        CurrScript.ScriptThread.OnTerminate := @CurrScript.ScriptThreadTerminate;
        CurrScript.ScriptThread.Start();

        ScriptState := ss_Running;
      end;
    end;
  end;
end;

procedure TSimbaForm.CompileScript;
begin
  InitializeTMThread(CurrScript.ScriptThread);

  if (CurrScript.ScriptThread <> nil) then
  begin
    CurrScript.ScriptThread.Options := CurrScript.ScriptThread.Options + [soCompileOnly];
    CurrScript.ScriptThread.OnTerminate := @CurrScript.ScriptThreadTerminate;
    CurrScript.ScriptThread.Start();

    ScriptState := ss_Running;
  end;
end;

procedure TSimbaForm.PauseScript;
begin
  if (ScriptState = ss_Running) then
  begin
    CurrScript.ScriptThread.State := ssPause;

    ScriptState := ss_Paused;
  end;
end;

procedure TSimbaForm.StopScript;
begin
  case ScriptState of
    ss_Stopping:
      begin
        Sleep(500); // Give a little time to terminate on it's own.

        if (CurrScript.ScriptThread <> nil) then
        begin
          mDebugLn('Forcefully terminating the script thread');

          if (not CurrScript.ScriptThread.Kill()) then
            formWriteLn('Failed to forcefully kill the script thread');
        end;
      end;

    ss_Running:
      begin
        CurrScript.ScriptThread.TerminateOptions := CurrScript.ScriptThread.TerminateOptions + [stoUserTerminated];
        CurrScript.ScriptThread.State := ssStop;

        ScriptState := ss_Stopping;
      end;

    ss_Paused:
      begin
        CurrScript.ScriptThread.State := ssStop;

        ScriptState := ss_Stopping;
      end;
  end;
end;

{ Tab management }

procedure TSimbaForm.AddTab;
var
  Tab : TMufasaTab;
begin;
  Tab := TMufasaTab.Create(Self.PageControl1);
  Tabs.Add(Tab);
  Tab.TabSheet.ImageIndex:= 8;
//  Tab.TabSheet.OnContextPopup:= @TabPopup;
  PageControl1.TabIndex:= Tabs.Count - 1;
  RefreshTab;
  if tabs.count > 1 then
  begin;
    TB_SaveAll.Enabled:= True;
    MenuItemSaveAll.Enabled:= True;
  end;
end;

function TSimbaForm.DeleteTab(TabIndex: integer; CloseLast : boolean; Silent : boolean = false) : boolean;
var
  Tab : TMufasaTab;
  OldIndex : integer;//So that we can switch back, if needed.
begin
  if not Silent then
  begin;
    OldIndex := PageControl1.TabIndex;
    if TabIndex = OldIndex then
    begin;
      if SimbaSettings.Tab.OpenNextOnClose.GetDefValue(False) = False then
        OldIndex := LastTab //We are closing the 'current'  tab, lets go back in history
      else
        OldIndex := Min(Tabs.Count - 1,OldIndex + 1);
    end;
    PageControl1.TabIndex:= TabIndex;
  end;
  //ScriptFrame now is now correct ;-D
  result := CanExitOrOpen;
  if not result then
    exit;
  Tab := TMufasaTab(Tabs[TabIndex]);
  if (Tabs.Count = 1) and (not CloseLast) then
    Tab.Clear
  else
  begin;
    Tab.Free;
    Tabs.Delete(TabIndex);
    if not Silent then
    begin;
      if OldIndex > TabIndex then
        PageControl1.TabIndex := OldIndex - 1
      else if OldIndex < TabIndex then
        PageControl1.TabIndex := OldIndex;
    end;
  end;
  if tabs.count <= 1 then
  begin;
    TB_SaveAll.Enabled:= false;
    MenuItemSaveAll.Enabled:= false;
  end;
  if not silent then
    RefreshTab;
end;

procedure TSimbaForm.ClearTab(TabIndex: integer);
begin
  TMufasaTab(Tabs[TabIndex]).Clear;
end;

procedure TSimbaForm.CloseTabs(Exclude: integer = -1; Silent : boolean = false);
var
  I : integer;
begin
  for i := tabs.count - 1 downto 0 do
    if i <> exclude then
      if not DeleteTab(i,false,silent) then
        exit;
end;

procedure TSimbaForm.SetEditActions;

  procedure EditActions(Undo,Redo,Cut,Copy,Paste,Delete : boolean);
  begin;
    ActionUndo.Enabled:= Undo;
    ActionRedo.Enabled:= Redo;
    ActionCut.Enabled:= Cut;
    ActionCopy.Enabled:= Copy;
    ActionPaste.Enabled:= Paste;
    ActionDelete.Enabled:= Delete;
    {$ifdef UpdateEditButtons}
    TT_Cut.Enabled:= Cut;
    TT_Paste.Enabled:=Paste;
    TT_Copy.enabled := Copy;
    {$endif}
  end;

var
  S: String;
  B: Boolean;
begin
  {
    TODO: This causes segfaults in EditActions(...) in certain cases.
    Simply checking if CurrScript and CurrScript.SynEdit is assigned is not
    enough.)
  }

  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
  begin
    with CurrScript.SynEdit do
    begin
      EditActions(CanUndo,CanRedo,SelText <> '',SelText <> '',CanPaste,SelText <> '');
//      B:= SelText <> '';
      B := SelAvail;
      PopupItemFind.Enabled:= B;
      PopupItemReplace.Enabled:= B;
      if(B)then
      begin
        s := SelText;
        if(Length(S) > 13)then
          S:= Format('"%s"', [Copy(S, 1, 10) + '...'])
        else
          S:= Format('"%s"', [S]);
        PopupItemFind.Caption:= 'Find next: ' + S;
        PopupItemReplace.Caption:= 'Replace:   ' + S;
      end;
    end
  end
  else if DebugMemo.Focused then
    with DebugMemo do
      EditActions(CanUndo,False,SelText <>'',SelText <> '',True,SelText <> '')
  else
    EditActions(false,false,false,false,false,false);
end;

procedure TSimbaForm.DoSearch(SearchOptions: TSynSearchOptions; HighlightAll: Boolean);
var
  Res : integer;
begin
  if CheckBoxMatchCase.Checked then
    Include(SearchOptions, ssoMatchCase);

  if SearchString = '' then
  begin
    res := -1;
    CurrScript.Synedit.SetHighlightSearch('',[]);
    CurrScript.SynEdit.LogicalCaretXY := SearchStart;
  end
  else
  begin
    if (not (ssoFindContinue in SearchOptions)) and CurrScript.SynEdit.SelAvail then
      if (ssoBackwards in SearchOptions) then
        CurrScript.SynEdit.LogicalCaretXY := CurrScript.SynEdit.BlockEnd
      else
        CurrScript.SynEdit.LogicalCaretXY := CurrScript.SynEdit.BlockBegin;

    mDebugLn('Searching: ' + SearchString);
    res := CurrScript.SynEdit.SearchReplace(SearchString, '', SearchOptions);

    if (res = 0) then
    begin
      mDebugLn('Search wrap around');
      Include(SearchOptions, ssoEntireScope);
      res := CurrScript.SynEdit.SearchReplace(SearchString, '', SearchOptions);
    end;
  end;

  if res = 0 then
  begin;
    LabeledEditSearch.Color := 6711039;
    LabeledEditSearch.Font.Color:= clWhite;
    CurrScript.Synedit.SetHighlightSearch('',[]);
    CurrScript.SynEdit.LogicalCaretXY := SearchStart;
  end
  else
  begin
    LabeledEditSearch.Color:= clWindow;
    LabeledEditSearch.Font.Color:= clWindowText;

    with CurrScript.SynEdit do
    begin
      HighlightAllColor.Background:= clYellow;
      if HighlightAll then
        SetHighlightSearch(SearchString, SearchOptions)
      else
        SetHighlightSearch('',[]);
    end;
  end;
end;

procedure TSimbaForm.RefreshTab;
var
  Tab : TMufasaTab;
  Script : TScriptFrame;
  NewTab : integer;
begin
  if tabs.Count < 1 then
  begin;
    mDebugLn('Cannot refresh tab, since there are no tabs.');
    exit;
  end;
  NewTab := PageControl1.TabIndex;
  if NewTab < 0 then
    Exit;
  Tab := TMufasaTab(Tabs[Newtab]);
  Script := Tab.ScriptFrame;
  Self.CurrScript := Script;
  Self.CurrTab := Tab;
  SetScriptState(Tab.ScriptFrame.FScriptState);//To set the buttons right
  // Only focus editor if Simba is focused and tab is currently showing.
  if Self.Focused and (Tab.TabSheet.TabIndex = Self.PageControl1.TabIndex) and CurrScript.SynEdit.CanFocus() then
    CurrScript.SynEdit.SetFocus();

  StopCodeCompletion;//To set the highlighting back to normal;

  with CurrScript.SynEdit do
  begin
    SetHighlightSearch('',[]);
    UseIncrementalColor:= false;
    MarkupByClass[TSynEditMarkupHighlightAllCaret].TempEnable;
    Invalidate;
  end;
  LabeledEditSearch.SelLength:= 0;
  LabeledEditSearch.Color:= clWindow;
  LabeledEditSearch.Font.Color:= clWindowText;

  //Set tha edit buttons right
  SetEditActions;
end;

procedure TSimbaForm.DoRefreshTab(Sender: PtrInt);
begin
  RefreshTab();
end;

procedure TSimbaForm.DoSimbaUpdateCheck(Sender: PtrInt);
begin
  UpdateTimer.Interval := SimbaSettings.Updater.CheckEveryXMinutes.Value;
  UpdateTimer.Enabled := True;
end;

{ Load settings }
procedure TSimbaForm.LoadFormSettings;
var
  Str: String;
  Data: TStringArray;
  i: Int32;
begin
  Data := Explode(':', SimbaSettings.LastConfig.MainForm.Position.GetDefValue(''));
  if (Length(Data) = 4) then
  begin
    Position := poDesigned;

    SetBounds(StrToInt(Data[0]), StrToInt(Data[1]), StrToInt(Data[2]), StrToInt(Data[3]));
  end;

  Str := LowerCase(SimbaSettings.LastConfig.MainForm.State.GetDefValue('normal'));
  if (Str = 'maximized') then
    Self.WindowState := wsMaximized
  else
    Self.WindowState := wsNormal;

  for i := 0 to StrToIntDef(SimbaSettings.MMLSettings.GetKeyValue(ssRecentFilesCount), -1) do
  begin
    Str := SimbaSettings.MMLSettings.GetKeyValue(ssRecentFile + IntToStr(i));

    if FileExists(Str) then
      AddRecentFile(Str);
  end;

  if SimbaSettings.CodeInsight.FunctionList.ShowOnStart.GetDefValue(True) or SimbaSettings.LastConfig.MainForm.FunctionListShown.GetDefValue(True) then
    FunctionListShown(True)
  else
    FunctionListShown(False);

  ActionConsoleExecute(nil);

  if not SimbaSettings.Tray.AlwaysVisible.GetDefValue(True) then
    MTrayIcon.Hide();
end;

{ Save Settings }
procedure TSimbaForm.SaveFormSettings;
var
  Data: TStringArray;
  i: integer;
begin
  if (not Assigned(SimbaSettings)) then
    Exit;

  with SimbaSettings.MMLSettings do
  begin
    if Self.WindowState = wsMaximized then
      SimbaSettings.LastConfig.MainForm.State.Value := 'maximized'
    else
    begin; //Only save the form position if its not maximized.
      SimbaSettings.LastConfig.MainForm.State.Value := 'normal';
      Data := ConvArr([inttostr(Self.left),inttostr(self.top),inttostr(self.width),inttostr(self.height)]);
      SimbaSettings.LastConfig.MainForm.Position.Value := Implode(':', Data);
    end;
    DeleteKey(ssRecentFiles);
    if RecentFiles.Count > 0 then
    begin
      SetSetting(ssRecentFiles + '/Count', inttostr(RecentFiles.Count));
      SetLength(data,RecentFiles.Count);
      for i := 0 to RecentFiles.Count - 1 do
        SetSetting(ssRecentFile + inttostr(i),RecentFiles[i]);
    end;
    SimbaSettings.LastConfig.MainForm.FunctionListShown.Value := MenuItemFunctionList.Checked;
    SaveToXML(SimbaSettingsFile);
  end;
end;

procedure TSimbaForm.AddRecentFile(const filename: string);
var
  MaxRecentFiles : integer;
  Len,i : integer;
begin
  MaxRecentFiles := SimbaSettings.General.MaxRecentFiles.GetDefValue(10);
  i := RecentFiles.IndexOf(filename);
  if i <> -1 then
    RecentFiles.Delete(i);
  if RecentFiles.Count = MaxRecentFiles then
    RecentFiles.Delete(0);
  RecentFiles.Add(filename);
  Len := RecentFiles.Count;
  if len <> length(RecentFileItems) then //Not reached maximum yet, add those files!
  begin
    SetLength(RecentFileItems,len);
    RecentFileItems[len-1] := TMenuItem.Create(MenuItemOpenRecent);
    RecentFileItems[len-1].OnClick:=@RecentFileItemsClick;
    MenuItemOpenRecent.Add(RecentFileItems[len-1]);
  end;
  for i := 0 to len - 1 do
    RecentFileItems[len - 1-i].Caption:= ExtractFileName(RecentFiles[i]);
end;

procedure TSimbaForm.InitializeTMThread(out Thread: TMMLScriptThread);
var
  Script: String;
  Continue: Boolean;
begin
  Thread := nil;

  if SimbaSettings.Misc.SaveScriptOnCompile.Value then
    SaveCurrentScript();

  Script := CurrScript.SynEdit.Lines.Text;

  if Assigned(OnScriptStart) then
  begin
    Continue := True;
    OnScriptStart(Self, Script, Continue);
    if (not Continue) then
      Exit;
  end;

  try
    Thread := TMMLScriptThread.Create(Script, CurrScript.ScriptFile);
    Thread.Output := DebugMemo.Lines;
    Thread.Error.Data := @CurrScript.ErrorData;
    Thread.Error.Callback := @CurrScript.HandleErrorData;

    Thread.ScriptFile := ExtractFileName(CurrScript.ScriptFile);
    Thread.ScriptPath := ExtractFilePath(CurrScript.ScriptFile);
    Thread.FontPath := SimbaEnvironment.FontPath;
    Thread.PluginPath := SimbaEnvironment.PluginPath;
    Thread.IncludePath := SimbaEnvironment.IncludePath;
    Thread.AppPath := SimbaEnvironment.SimbaPath;

    if Selector.HasPicked then
      Thread.Client.IOManager.SetTarget(Selector.LastPick);

    Thread.SetSettings(SimbaSettings.MMLSettings);
    Thread.SetFonts(SimbaEnvironment.FontPath); // Create font constants

    CurrScript.ScriptErrorLine := -1;
  except
    on e: Exception do
      formWriteln('Failed to initialise script thread: ' + e.ClassName + '::' + e.Message);
  end;
end;

procedure TSimbaForm.DoHandleParameters(Data: PtrInt);
begin
  if Application.HasOption('h', 'help') then
  begin
    WriteLn('');
    WriteLn('Options:');
    WriteLn('  -open, -o: opens the given script');
    WriteLn('  -run, -r: runs the script opened with -open');
    WriteLn('  -compile, -c: compiles the script opened with -open');
    WriteLn('  -test: will wait for script to run then terminates Simba, exit code 1 if errored. requires -open and -run or -compile');
    WriteLn('  -settings, -s: uses the given settings file');
    WriteLn('');
  end;

  if (Application.ParamCount = 1) then
    LoadScriptFile(Application.Params[1]);
  if Application.HasOption('o', 'open') then
    LoadScriptFile(Application.GetOptionValue('o', 'open'));

  if Application.HasOption('r', 'run') then
    Self.RunScript();
  if Application.HasOption('c', 'compile') then
    Self.CompileScript();
end;

procedure TSimbaForm.DoRunTest(Data: PtrInt);
begin
  if Application.HasOption('test') then
  begin
    Application.RemoveASyncCalls(Self);

    while (Self.GetScriptState() in [ss_Running, ss_Stopping]) do
      Application.ProcessMessages();

    WriteLn('Testing ' + CurrScript.ScriptFile);
    Writeln(DebugMemo.Text);

    // This whole testing interface is a hack right now until out of process scripts are implemented.
    // But terminating like this allows nothing else to raise a exception while freeing (halt calls finalization sections).
    {$IFDEF WINDOWS}
    if (Self.CurrScript.ScriptErrorLine > -1) then
      TerminateProcess(OpenProcess(PROCESS_TERMINATE, True, GetProcessID()), 1)
    else
      TerminateProcess(OpenProcess(PROCESS_TERMINATE, True, GetProcessID()), 0);
    {$ELSE}
    if (Self.CurrScript.ScriptErrorLine > -1) then
      Halt(1)
    else
      Halt(0);
    {$ENDIF}
  end;
end;

procedure TSimbaForm.DoParseInternals(Data: PtrInt);
begin
  with TProcThread.Create() do
  begin
    ClassProc := @ParseInternals;
    OnTerminate := @FillFunctionList;
    Start();
  end;
end;

procedure TSimbaForm.DoSimbaNews(Data: PtrInt);
begin
  with TProcThread.Create() do
  begin
    ClassProc := @GetSimbaNews;
    OnTerminate := @WriteSimbaNews;
    Start();
  end;
end;

procedure TSimbaForm.OnSaveScript(const Filename: string);
begin
  with CurrScript do
  begin
    ScriptFile:= SetDirSeparators(Filename);
    ScriptName:= ExtractFileNameOnly(ScriptFile);
    mDebugLn('Script name will be: ' + ScriptName);
    formWriteLn('Successfully saved: ' + ScriptFile);
    StartText:= SynEdit.Lines.Text;
    ScriptChanged := false;
    SynEdit.MarkTextAsSaved;
    CurrTab.TabSheet.Caption:= ScriptName;
    Self.AddRecentFile(ScriptFile);
    UpdateTitle;
  end;
end;

function TSimbaForm.DefaultScript: string;
begin
  Result := 'program new;' + LineEnding +
            'begin' + LineEnding +
            'end.';

  if FileExistsUTF8(SimbaSettings.SourceEditor.DefScriptPath.Value) then
  begin
    try
      with TStringList.Create do
        try
          LoadFromFile(SimbaSettings.SourceEditor.DefScriptPath.Value);
          Result := Text;
        finally
          Free;
        end;
    except
      mDebugLn('Couldn''t load default script file.');
    end;
  end;
end;

procedure TSimbaForm.DefaultHighlighter;
begin
  if (Highlighter = nil) then
    Highlighter := TSynFreePascalSyn.Create(Self);

  with Highlighter do
  begin
    NestedComments := False;
    StringKeywordMode := spsmNone;
  end;
end;

procedure TSimbaForm.ActionTabLastExecute(Sender: TObject);
var
  CurrIndex : integer;
begin
  CurrIndex := PageControl1.TabIndex;
  if CurrIndex = 0 then
    CurrIndex := Tabs.count - 1
  else
    Dec(CurrIndex);
  PageControl1.TabIndex:= CurrIndex;
end;

procedure TSimbaForm.ActionCloseTabExecute(Sender: TObject);
begin
  if(PageControl1.PageCount > 1) then
  begin
    if Sender is TTabSheet then
      Self.DeleteTab(TTabSheet(Sender).TabIndex,false)
    else
      Self.DeleteTab(PageControl1.TabIndex,false)
  end
  else
    Self.ClearScript;  //DeleteTab would take care of this already, but yeah, it's neater this way.
end;

procedure TSimbaForm.ActionCompileScriptExecute(Sender: TObject);
begin
  Self.CompileScript();
end;

{$IFDEF WINDOWS}
function ConsoleHandler(Event: DWord): WINBOOL; stdcall;
begin
  Result := TRue;

  TThread.Synchronize(nil, @SimbaForm.Close);
end;
{$ENDIF}

procedure TSimbaForm.ActionConsoleExecute(Sender: TObject);

  function GetConsoleWindow: HWND;
  var
    PID: UInt32 = 0;
  begin
    Result := 0;

    {$IFDEF WINDOWS}
    GetWindowThreadProcessId(Windows.GetConsoleWindow(), PID);
    if (PID = GetCurrentProcessID()) then
      Result := Windows.GetConsoleWindow();
    {$ENDIF}
  end;

  function GetConsoleHandle: THandle;
  begin
    Result := 0;

    {$IFDEF WINDOWS}
    Result := GetStdHandle(STD_INPUT_HANDLE);
    {$ENDIF}
  end;

var
  Mode: UInt32;
begin
  if (Sender <> nil) then
    SimbaSettings.LastConfig.MainForm.ConsoleVisible.Value := not SimbaSettings.LastConfig.MainForm.ConsoleVisible.Value;

  {$IFDEF WINDOWS}
  SetConsoleCtrlHandler(@ConsoleHandler, True); // close simba when command prompt is closed.

  // Disable silly windows features that freeze the application
  GetConsoleMode(GetConsoleHandle(), Mode);
  SetConsoleMode(GetConsoleHandle, Mode and not (ENABLE_QUICK_EDIT_MODE or ENABLE_INSERT_MODE));

  case SimbaSettings.LastConfig.MainForm.ConsoleVisible.Value of
    True: ShowWindow(GetConsoleWindow(), SW_SHOWNORMAL);
    False: ShowWindow(GetConsoleWindow(), SW_HIDE);
  end;
  {$ENDIF}
end;

procedure TSimbaForm.ActionCopyExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.SynEdit.CopyToClipboard
  else if DebugMemo.Focused then
    DebugMemo.CopyToClipboard;
end;

procedure TSimbaForm.ActionCutExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.SynEdit.CutToClipboard
  else if DebugMemo.Focused then
    DebugMemo.CutToClipboard;
end;

procedure TSimbaForm.ActionDeleteExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.SynEdit.ClearSelection
  else if DebugMemo.Focused then
    DebugMemo.ClearSelection;
end;

procedure TSimbaForm.ActionExitExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TSimbaForm.ActionFindNextExecute(Sender: TObject);
begin
  DoSearch([ssoFindContinue], False);
end;

procedure TSimbaForm.ActionFindPrevExecute(Sender: TObject);
begin
  DoSearch([ssoFindContinue, ssoBackwards], False);
end;

procedure TSimbaForm.ActionFindStartExecute(Sender: TObject);
begin
  if (CurrScript.SynEdit.SelAvail) then
      LabeledEditSearch.Text := CurrScript.SynEdit.SelText;

  SearchPanel.Visible := True;
  if LabeledEditSearch.CanFocus then
    LabeledEditSearch.SetFocus;
end;

procedure TSimbaForm.ActionFontExecute(Sender: TObject);
var
  Dialog: TFontDialog;
begin
  Dialog := TFontDialog.Create(nil);
  with Dialog do
  try
    Options := [fdEffects, fdFixedPitchOnly];
    Title := 'Font Editor';
    Font := SimbaSettings.SourceEditor.Font.Value;

    if (Execute) then
      SimbaSettings.SourceEditor.Font.Value := Font;
  finally
    Dialog.Free;
  end;
end;

procedure TSimbaForm.ActionColorsExecute(Sender: TObject);
begin
  SimbaColors.Show();
end;

procedure TSimbaForm.ActionCodeCommentExecute(Sender: TObject);
var
  CurPos: TPoint;
begin
  try
    CurPos := CurrScript.SynEdit.CaretXY;
    TScriptCommenter.Process(CurrScript.SynEdit);
    CurrScript.SynEdit.CaretXY := CurPos;
  except
    mDebugLn('Cannot comment the selected code!');
  end;
end;

procedure TSimbaForm.ActionGotoExecute(Sender: TObject);
var
  Value : string;
  P : TPoint;
begin
  Value := '';
  if InputQuery('Goto line','Goto line:',Value) then
  begin
    P.x := 1;
    P.y := StrToIntDef(Value,-1);
    if p.y < 1 then p.y :=1;
    CurrScript.SynEdit.CaretXY := p;
    CurrScript.SynEdit.TopLine:= max(P.y - (CurrScript.SynEdit.LinesInWindow div 2),1);
  end;
end;

procedure TSimbaForm.ActionClearDebugExecute(Sender: TObject);
begin
  DebugMemo.Clear;
end;

procedure TSimbaForm.ActionNewExecute(Sender: TObject);
begin
  //Self.ClearScript;
  Self.AddTab;
end;

procedure TSimbaForm.ActionNewTabExecute(Sender: TObject);
begin
  Self.AddTab;
end;

procedure TSimbaForm.ActionFileBrowserExecute(Sender: TObject);
begin
  SimbaSettings.FileBrowser.Visible.Value := not SimbaSettings.FileBrowser.Visible.Value;
  MenuItemFileBrowser.Checked := SimbaSettings.FileBrowser.Visible.Value;

  UpdateUtilties();
end;

procedure TSimbaForm.ActionNotesExecute(Sender: TObject);
begin
  SimbaSettings.Notes.Visible.Value := not SimbaSettings.Notes.Visible.Value;
  MenuItemNotes.Checked := SimbaSettings.Notes.Visible.Value;

  UpdateUtilties();
end;

procedure TSimbaForm.UpdateUtilties;
var
  Browser, Notes: TPairSplitterSide;
  i: Int32;
begin
  if (not PanelUtilites.HandleAllocated) then
    PanelUtilites.HandleNeeded();

  Browser := PanelUtilites.Sides[0];
  Browser.Visible := SimbaSettings.FileBrowser.Visible.Value;

  Notes := PanelUtilites.Sides[1];
  Notes.Visible := SimbaSettings.Notes.Visible.Value;

  for i := 0 to PanelUtilites.ControlCount - 1 do
    if PanelUtilites.Controls[i] is TSplitter then
      TSplitter(PanelUtilites.Controls[i]).Visible := Browser.Visible and Notes.Visible;

  if Browser.Visible and Notes.Visible then
  begin
    Browser.Align := alTop;
    Notes.Align := alClient;
  end else
  if Browser.Visible and (not Notes.Visible) then
    Browser.Align := alClient
  else
  if Notes.Visible and (not Browser.Visible) then
    Notes.Align := alClient;

  PanelUtilites.Visible := Browser.Visible or Notes.Visible;
  UtilitesSplitter.Visible := Browser.Visible or Notes.Visible;
end;

procedure TSimbaForm.ActionOpenExecute(Sender: TObject);
begin
  Self.OpenScript;
end;

procedure TSimbaForm.ActionPasteExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.SynEdit.PasteFromClipboard
  else if DebugMemo.Focused then
    DebugMemo.PasteFromClipboard
  else if LabeledEditSearch.Focused then
    LabeledEditSearch.PasteFromClipboard;
end;

procedure TSimbaForm.ActionPauseExecute(Sender: TObject);
begin
  Self.PauseScript;
end;

procedure TSimbaForm.ActionRedoExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.Redo
  else if DebugMemo.Focused then
    DebugMemo.Undo; //?
end;

procedure TSimbaForm.ActionReplaceExecute(Sender: TObject);
begin
  if (ScriptPopup.HandleAllocated) and (CurrScript.SynEdit.SelAvail) then
    dlgReplace.FindText:= CurrScript.SynEdit.SelText;
  dlgReplace.Execute;
end;

procedure TSimbaForm.ActionRunExecute(Sender: TObject);
begin
  Self.RunScript;
end;

procedure TSimbaForm.ActionSaveAllExecute(Sender: TObject);
var
  i : integer;
  OldIndex : integer;
begin
  OldIndex := PageControl1.TabIndex;
  for i := 0 to Tabs.Count - 1 do
  begin;
    PageControl1.TabIndex:= i;
    SaveCurrentScript;
  end;
  PageControl1.TabIndex:= oldindex;
end;

procedure TSimbaForm.ActionSaveAsExecute(Sender: TObject);
begin
  Self.SaveCurrentScriptAs;
end;

procedure TSimbaForm.ActionSaveDefExecute(Sender: TObject);
begin
  Self.SaveCurrentScriptAsDefault;
end;

procedure TSimbaForm.ActionSaveExecute(Sender: TObject);
begin
  Self.SaveCurrentScript;
end;

procedure TSimbaForm.ActionSelectAllExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.SynEdit.SelectAll
  else if DebugMemo.Focused then
    DebugMemo.SelectAll
  else if LabeledEditSearch.Focused then
    LabeledEditSearch.SelectAll;

end;

procedure TSimbaForm.ActionStopExecute(Sender: TObject);
begin
  Self.StopScript;
end;

procedure TSimbaForm.ActionTabNextExecute(Sender: TObject);
var
  CurrIndex : integer;
begin
  CurrIndex := PageControl1.TabIndex;
  if CurrIndex = Tabs.count - 1 then
    CurrIndex := 0
  else
    Inc(CurrIndex);
  PageControl1.TabIndex:= CurrIndex;
end;

procedure TSimbaForm.ActionUndoExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.Undo
  else if DebugMemo.Focused then
    DebugMemo.Undo;
end;

procedure TSimbaForm.ChangeMouseStatus(Sender: TObject);
var
  x: integer = -1;
  y: integer = -1;
begin;
  if (not Self.Manager.TargetValid()) then
    Self.Manager.SetDesktop();
  Self.Manager.GetMousePos(x, y);

  StatusBar.Panels[Panel_Coords].Text := Format('(%d, %d)', [x, y]);
end;

procedure TSimbaForm.CheckBoxMatchCaseClick(Sender: TObject);
begin
  RefreshTab;
  CurrScript.SynEdit.MarkupByClass[TSynEditMarkupHighlightAllCaret].TempDisable;
  SearchString := LabeledEditSearch.Text;
  DoSearch([], true);
  CurrScript.SynEdit.UseIncrementalColor:= true;

  if LabeledEditSearch.CanFocus then
    LabeledEditSearch.SetFocus;
end;

procedure TSimbaForm.CloseFindPanel;
begin
  SearchPanel.Visible:= false;
  if CurrScript.SynEdit.CanFocus then
    CurrScript.SynEdit.SetFocus;
end;

{
  If we are being sent to the background; then minimize other active windows as
  well.
}
procedure TSimbaForm.doOnHide(Sender: TObject);
begin
  if (not (csDestroying in ComponentState)) and (DebugImgForm <> nil) and DebugImgForm.Showing then
    DebugImgForm.Hide;
end;

procedure TSimbaForm.FileBrowserDoubleClick(Sender: TObject);
begin
  if (FileBrowser.Selected <> nil) then
    LoadScriptFile(TSimbaFileBrowser_Node(FileBrowser.Selected).Path, True, True);
end;

procedure TSimbaForm.FileBrowser_Popup_OnPopup(Sender: TObject);
begin
  if (FileBrowser.Selected <> nil) then
  begin
    FileBrowser_PopupItem_OpenExternally.Enabled := True;
    FileBrowser_PopupItem_Open.Enabled := not TSimbaFileBrowser_Node(FileBrowser.Selected).IsDirectory;
  end else
  begin
    FileBrowser_PopupItem_OpenExternally.Enabled := False;
    FileBrowser_PopupItem_Open.Enabled := False;
  end;
end;

procedure TSimbaForm.FileBrowserRefresh(Sender: TObject);
begin
  FileBrowser.Refresh();
end;

procedure TSimbaForm.StopCodeCompletion;
begin
  CodeCompletionForm.Hide;
end;

function TSimbaForm.FindTab(filename: string): integer;
var
  i : integer;
begin
  FileName := SetDirSeparators(filename);
  for i := 0 to SimbaForm.Tabs.Count - 1 do
    {$ifdef MSWindows} //Case insensitive
    if lowercase(TMufasaTab(Tabs[i]).ScriptFrame.ScriptFile) = lowercase(filename) then
    {$else}
    if TMufasaTab(Tabs[i]).ScriptFrame.ScriptFile = filename then
    {$endif}
      exit(i);
  result := -1;
end;

procedure TSimbaForm.FormDropFiles(Sender: TObject; const FileNames: array of String
  );
var
  i : integer;
begin
  if (length(FileNames) = 1) then
  begin
    LoadScriptFile(FileNames[0]); //One file saves us some work
    exit;
  end;
  if (length(FileNames) > 5) then //> 5 seems nice to me, cant imagine you want to open that many scripts on a regular base
    case MessageDlg('Are you sure you want to open '+inttostr(length(filenames))+
                    ' scripts?', mtConfirmation, mbYesNo, 0)  of
      IDNO: exit;
    end;
  {$IfDef WINDOWS}
  //Fix for the really old Windows kernel bug which probably will never be fixed
  for i := 1 to high(filenames) do
   LoadScriptFile(FileNames[i],true);
  LoadScriptFile(FileNames[0],true);
  {$Else} //in this case its tolerable as Windows is the only OS with this bug
  for i := 0 to high(filenames) do
   LoadScriptFile(FileNames[i],true);
  {$EndIf};
end;

procedure TSimbaForm.DebugMemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((ssCtrl in Shift) and (not ((ssShift in Shift) or (ssAlt in Shift)))) then
  begin
    if (Key = VK_A) then DebugMemo.SelectAll;
  end;
  //Are there any more?
end;

procedure TSimbaForm.ShowDTMEditor(Sender: TObject);
begin
  TDTMForm.Create(Manager, DebugMemo).Show();
end;

procedure TSimbaForm.ShowFormDesigner(Sender: TObject);
begin
  {$IFDEF USE_FORMDESIGNER}
  CompForm.ShowOnTop();
  {$ENDIF}
end;

procedure TSimbaForm.UnloadPlugin(Sender: TObject);
begin
  Plugins.Unload(TMenuItem(Sender).Caption);
end;

// Populate unload plugin items
procedure TSimbaForm.MenuToolsClick(Sender: TObject);
var
  Files: TStringArray;
  RefCounts: TIntegerArray;
  i: Int32;
  Item: TMenuItem;
begin
  MenuItemUnloadPlugin.Clear();

  Plugins.GetAll(Files, RefCounts);

  if (Length(Files) > 0) then
  begin
    for i := 0 to High(Files) do
    begin
      Item := TMenuItem.Create(MenuItemUnloadPlugin);
      Item.Caption := Files[i];
      Item.Enabled := RefCounts[i] = 0;
      Item.OnClick := @UnloadPlugin;
      if (not Item.Enabled) then
        Item.Caption := Item.Caption + ' [' + IntToStr(RefCounts[i]) + ']';

      MenuItemUnloadPlugin.Add(Item);
    end;
  end else
  begin
    Item := TMenuItem.Create(MenuItemUnloadPlugin);
    Item.Caption := '(none)';
    Item.Enabled := False;

    MenuItemUnloadPlugin.Add(Item);
  end;
end;

procedure TSimbaForm.FileBrowser_Popup_Open(Sender: TObject);
var
  Path: String;
begin
  if (FileBrowser.Selected <> nil) then
  begin
    Path := TSimbaFileBrowser_Node(FileBrowser.Selected).Path;
    if FileExists(Path) then
      LoadScriptFile(Path, True, True);
  end;
end;

procedure TSimbaForm.FileBrowser_Popup_OpenExternally(Sender: TObject);

  procedure OpenDirectory(Path: String);
  var
    Executable: String = '';
    Output: String;
    ExitStatus: Int32;
  begin
    {$IFDEF WINDOWS}
    Executable := 'explorer.exe';
    Path := '/root,"' + Path + '"';
    {$ENDIF}
    
    {$IFDEF LINUX}
    Executable := 'xdg-open';
    {$ENDIF}
    
    if Executable = '' then
      raise Exception.Create('OpenDirectory is unsupported on your OS');
    
    if RunCommandInDir('', Executable, [Path], Output, ExitStatus) <> 0 then
      ShowMessage('Unable to open ' + Executable + ': ' + Output);
  end;

var
  Path: String;
begin
  if (FileBrowser.Selected <> nil) then
  begin
    Path := ExpandFileName((FileBrowser.Selected as TSimbaFileBrowser_Node).Path);

    if (DirectoryExists(Path)) then
      OpenDirectory(Path)
    else if (FileExists(Path)) then
      OpenDocument(Path)
    else
      ShowMessage('Unable to open "' + Path + '", it either does not exist or is not a file or a directory.');
  end;
end;

procedure TSimbaForm.MenuItemBitmapConvClick(Sender: TObject);
begin
  BitmapConvForm.Show;
end;

procedure TSimbaForm.MenuItemHandbookClick(Sender: TObject);
begin
  OpenURL('http://docs.villavu.com/simba/');
end;

procedure TSimbaForm.MenuItemColourHistoryClick(Sender: TObject);
begin
  MenuItemColourHistory.Checked := not ColourHistoryForm.Visible;
  if MenuItemColourHistory.Checked then
    ColourHistoryForm.Show
  else
    ColourHistoryForm.Hide;
end;

procedure TSimbaForm.dlgReplaceFind(Sender: TObject);
begin
  SearchString := dlgReplace.FindText;
  DoSearch([ssoFindContinue], False);
end;

procedure TSimbaForm.dlgReplaceReplace(Sender: TObject);
var
  SOptions: TSynSearchOptions;
  P: TPoint;
  Y: Boolean;
  Btns: TMsgDlgButtons;

 procedure Replace;
 begin
   CurrScript.SynEdit.SearchReplaceEx(dlgReplace.FindText, dlgReplace.ReplaceText, SOptions + [ssoReplace], P);
 end;

begin
  Y:= not (frPromptOnReplace in dlgReplace.Options);
  SOptions:= [];
  if(frMatchCase in dlgReplace.Options)then SOptions:= [ssoMatchCase];
  if(frWholeWord in dlgReplace.Options)then SOptions+= [ssoWholeWord];
  with CurrScript.SynEdit do
  begin
    Btns:= [mbYes, mbNo];
    if(frReplaceAll in dlgReplace.Options)then Btns+= [mbYesToAll];
    if(frEntireScope in dlgReplace.Options)then P:= Classes.Point(0, 0) else P:= CaretXY;
    while SearchReplaceEx(dlgReplace.FindText, '', SOptions, P) > 0 do
    begin
      if(Y)then
        Replace
      else case MessageDlg('Replace', Format('Do you want to replace "%s" with "%s"?', [dlgReplace.FindText, dlgReplace.ReplaceText]), mtConfirmation, Btns, 0) of
        mrYes: Replace;
        mrYesToAll: begin
                      Replace;
                      Y:= True;
                    end;
      end;
      if(not(frReplaceAll in dlgReplace.Options))then exit;
      P:= CaretXY;
    end;
  end;
end;

procedure TSimbaForm.EditSearchChange(Sender: TObject);
begin
  SearchString := LabeledEditSearch.Text;
  DoSearch([], true);
end;

procedure TSimbaForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Int32;
begin
  Exiting := True;

  FunctionList.Stop();
  SaveFormSettings();

  if (Assigned(Tabs)) then
    for i := Tabs.Count - 1 downto 0 do
      if not DeleteTab(i,true) then
      begin;
        CloseAction := caNone;
        Exit;
      end;

  CloseAction := caFree;
end;

procedure TSimbaForm.ParseInternals;

  function Parse(Section, Text: String): TCodeInsight;
  var
    Stream: TMemoryStream;
  begin
    Stream := TMemoryStream.Create();
    Stream.Write(Text[1], Length(Text));

    Result := TCodeInsight.Create();
    Result.FileName := Section;
    Result.OnMessage := @OnCCMessage;
    Result.Run(Stream, nil, -1, True);
  end;

var
  Imports: TScriptImports_Dump;
  i: Int32;
begin
  try
    Imports := TScriptImports_Dump.Create();

    try
      for i := 0 to Imports.Dump.Count - 1 do
      begin
        SetLength(CoreBuffer, Length(CoreBuffer) + 1);
        CoreBuffer[High(CoreBuffer)] := Parse(Imports.Dump.Names[i], Imports.Dump.ValueFromIndex[i]);
      end;
    finally
      Imports.Free();
    end;
  except
    on e: Exception do
      mDebugLn('ERROR parsing internals: ' + e.ClassName + ' :: ' + e.Message);
  end;
end;

procedure TSimbaForm.FillFunctionList(Sender: TObject);
var
  i: Int32;
begin
  for i := 0 to High(CoreBuffer) do
  begin
    if (CoreBuffer[i].FileName = 'LCLClasses') or (CoreBuffer[i].FileName = 'MMLClasses') then
      Continue;

    with FunctionList do
      addDeclarations(CoreBuffer[i].Items, addSimbaSection(CoreBuffer[i].FileName), False, True);
  end;
end;

procedure TSimbaForm.FormCreate(Sender: TObject);

  procedure LoadSettings;
  begin
    if Application.HasOption('s', 'settings') then
      SimbaSettingsFile := Application.GetOptionValue('s', 'settings')
    else
      SimbaSettingsFile := SimbaEnvironment.DataPath + 'settings.xml';

    CreateSimbaSettings(SimbaSettingsFile);

    LoadFormSettings();
    RegisterSettingsOnChanges();
  end;

  procedure LoadUtilites;
  begin
    FileBrowser := TSimbaFileBrowser.Create(FileBrowserPanel);
    FileBrowser.Parent := FileBrowserPanel;
    FileBrowser.Align := alClient;
    FileBrowser.BorderStyle := bsNone;
    FileBrowser.Root := Application.Location;
    FileBrowser.OnDblClick := @FileBrowserDoubleClick;
    FileBrowser.PopupMenu := FileBrowserPopup;

    NotesMemo.Lines.Text := DecompressString(Base64Decode(SimbaSettings.Notes.Content.Value));

    MenuItemNotes.Checked := SimbaSettings.Notes.Visible.Value;
    MenuItemFileBrowser.Checked := SimbaSettings.FileBrowser.Visible.Value;

    UpdateUtilties();
  end;

begin
  BeginFormUpdate();

  try
    Application.OnException := @CustomExceptionHandler;
    Application.QueueAsyncCall(@DoHandleParameters, 0);
    Application.QueueAsyncCall(@DoRunTest, 0);
    Application.QueueAsyncCall(@DoParseInternals, 0);
    Application.QueueAsyncCall(@DoRefreshTab, 0);
    Application.QueueAsyncCall(@DoSimbaNews, 0);
    Application.QueueAsyncCall(@DoSimbaUpdateCheck, 0);

    if (not DirectoryIsWritable(Application.Location)) then
      ShowMessage('No permission to write to Simba''s directory. Run as ' + {$IFDEF WINDOWS} 'administrator' {$ELSE} 'sudo' {$ENDIF} + ' if this causes issues.');

    RecentFiles := TStringList.Create();

    LoadSettings();
    LoadUtilites();

    Plugins.Paths.Add(SimbaSettings.Plugins.Path.Value);

    SimbaColors := TSimbaColors.Create(SimbaForm);
    SimbaPackageForm := TSimbaPackageForm.Create(Self, ToolBar);

    CodeCompletionForm := TAutoCompletePopup.Create(Self);
    CodeCompletionForm.InsertProc := @OnCompleteCode;
    ParamHint := TParamHint.Create(Self);

    DefaultHighlighter();

    {$IFDEF WINDOWS}
    Bind_Windows_Keys();
    {$ENDIF}

    {$IFDEF LINUX_HOTKEYS}
    Bind_Linux_Keys();
    {$ENDIF}

    {$IFDEF USE_SCRIPTMANAGER}
    MenuItemFormDesigner.Visible := True;
    {$ENDIF}

    Self.OnScriptStart := @ScriptStartEvent;
    Self.OnScriptOpen := @ScriptOpenEvent;

    Tabs := TList.Create();
    AddTab();
    Manager := TIOManager.Create(SimbaSettings.Plugins.Path.Value);
    Picker := TMColorPicker.Create(Manager);
    Selector := TMWindowSelector.Create(Manager);

    {$IFDEF WINDOWS}
    if FileExists(Application.ExeName+'_old_') then
      DeleteFileUTF8(Application.ExeName+'_old_');
    {$ENDIF}

    UpdateTitle();
  finally
    EndFormUpdate();
  end;
end;

procedure TSimbaForm.FormDestroy(Sender: TObject);
var
  i : integer;
begin
  { Free the tabs }
  if (Assigned(Tabs)) then
    for i := Tabs.Count - 1 downto 0 do
      TMufasaTab(Tabs[i]).Free;

  for i := 0 to high(RecentFileItems) do
    RecentFileItems[i].Free;

  if (Assigned(Tabs)) then
    FreeAndNil(Tabs);

  { Free MML Core stuff }
  if (Assigned(Selector)) then
    FreeAndNil(Selector);
  if (Assigned(Picker)) then
    FreeAndNil(Picker);
  if (Assigned(Manager)) then
    FreeAndNil(Manager);

  if (Assigned(RecentFiles)) then
    FreeAndNil(RecentFiles);
  if (Assigned(ParamHint)) then
    FreeAndNil(ParamHint);

  {$ifdef MSWindows}
  Unbind_Windows_Keys;
  {$else}
  {$IFDEF LINUX_HOTKEYS}
  Unbind_Linux_Keys;
  {$ENDIF}
  {$endif}

  if (Assigned(SimbaSettings)) then
    SimbaSettings.Notes.Content.Value := Base64Encode(CompressString(NotesMemo.Lines.Text));

  FreeSimbaSettings(True, SimbaSettingsFile);
end;

procedure TSimbaForm.FormShortCuts(var Msg: TLMKey; var Handled: Boolean);
begin
  SetEditActions;
  Handled := ActionList.IsShortCut(Msg);
end;



procedure TSimbaForm.LabeledEditSearchEnter(Sender: TObject);
begin
  SearchStart := CurrScript.SynEdit.LogicalCaretXY;
  with CurrScript.SynEdit do
  begin
    UseIncrementalColor:= true;
    MarkupByClass[TSynEditMarkupHighlightAllCaret].TempDisable
  end;
end;

procedure TSimbaForm.LabeledEditSearchExit(Sender: TObject);
begin
  if not CheckBoxMatchCase.MouseEntered then
    RefreshTab;
end;

procedure TSimbaForm.LabeledEditSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (key = vk_f) then
  begin;
    LabeledEditSearch.SelectAll;
  end else
  if key = VK_ESCAPE then
  begin
    CloseFindPanel;
    key := 0;
  end;
end;

procedure TSimbaForm.LabeledEditSearchKeyPress(Sender: TObject; var Key: char);
begin
  if key = #13 then
  begin;
    SearchString:= LabeledEditSearch.Text;
    DoSearch([ssoFindContinue], True);
    key := #0;
//    LabeledEditSearch.SelStart:= Length(LabeledEditSearch.Text);
  end;
end;

procedure TSimbaForm.MenuEditClick(Sender: TObject);
begin
  SetEditActions;
end;

procedure TSimbaForm.MenuItemAboutClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TSimbaForm.MenuItemCloseTabsClick(Sender: TObject);
begin
  Self.CloseTabs;
end;

procedure TSimbaForm.MenuItemDebugImageClick(Sender: TObject);
begin
  MenuItemDebugImage.Checked := not DebugImgForm.Visible;
  if MenuItemDebugImage.Checked then
    DebugImgForm.Show
  else
    DebugImgForm.Hide;
end;

procedure TSimbaForm.MenuItemExportHTMLClick(Sender: TObject);
var
  SynExporterHTML : TSynExporterHTML;
begin;
  SynExporterHTML := TSynExporterHTML.Create(nil);
  SynExporterHTML.Highlighter := Highlighter;
  SynExporterHTML.ExportAsText:= True;
  with TSaveDialog.Create(nil) do
    try
      Filter:= 'HTML Files (*.html;*.htm)|*.html;*.htm|All files(*.*)|*.*';
      Options:= [ofOverwritePrompt,ofEnableSizing];
      DefaultExt:= 'html';
      if Execute then
      begin
        if CurrScript.ScriptName <> '' then
          SynExporterHTML.Title:= 'Simba - ' + CurrScript.ScriptName
        else
          SynExporterHTML.Title:= 'Simba - Untitled';
        SynExporterHTML.ExportAll(CurrScript.SynEdit.Lines);
        SynExporterHTML.SaveToFile(FileName);
      end;
    finally
      free;
      SynExporterHTML.Free;
    end;
end;

procedure TSimbaForm.MenuitemFillFunctionListClick(Sender: TObject);
begin

end;

procedure TSimbaForm.MenuItemHideClick(Sender: TObject);
begin
  if Self.Visible = false then
    MenuItemShowClick(sender)
  else
    Self.Hide;
end;

procedure TSimbaForm.MenuItemOpenIncludesFolderClick(Sender: TObject);
begin
  OpenDocument(SimbaSettings.Includes.Path.Value);
end;

procedure TSimbaForm.MenuItemOpenPluginsFolderClick(Sender: TObject);
begin
  OpenDocument(SimbaSettings.Plugins.Path.Value);
end;

procedure TSimbaForm.MenuItemOpenScriptsFolderClick(Sender: TObject);
begin
  OpenDocument(SimbaSettings.Scripts.Path.Value);
end;

procedure TSimbaForm.MenuItemReportBugClick(Sender: TObject);
begin
  OpenURL('https://github.com/MerlijnWajer/Simba/issues/new');
end;

procedure TSimbaForm.MenuItemSettingsButtonClick(Sender: TObject);
var
  res: Integer;
begin
  SimbaSettings.Save(SimbaSettingsFile);

  res := SettingsForm.ShowModal;
  if res = mrOK then
    ReloadSimbaSettings(SimbaSettingsFile);
end;

procedure TSimbaForm.MenuItemSettingsSimpleButtonClick(Sender: TObject);
begin
  SettingsSimpleForm.ShowModal;
end;

procedure TSimbaForm.MenuItemShowClick(Sender: TObject);
begin
  Self.Show;
  Self.WindowState := wsNormal;
end;

procedure TSimbaForm.MenuItemTabCloseClick(Sender: TObject);
begin
  DeleteTab(PopupTab,false);
end;

procedure TSimbaForm.MenuItemTabCloseOthersClick(Sender: TObject);
begin
  CloseTabs(PopupTab);
end;

procedure TSimbaForm.MenuItemReadOnlyTabClick(Sender: TObject);
var
  Tab: TMufasaTab;

begin
  Tab := TMufasaTab(Tabs[PopupTab]);
  if Tab.ScriptFrame.ScriptFile = '' then
  begin
    MessageDlg('Script needs to be saved before it can be put in Read Only mode',
                       mtWarning, [mbOK], 0);
    Exit;
  end;

  Tab.ScriptFrame.SetReadOnly(not Tab.ScriptFrame.GetReadOnly());
  MenuItemReadOnlyTab.Checked := not Tab.ScriptFrame.GetReadOnly();
end;

procedure TSimbaForm.MenuItemFunctionListClick(Sender: TObject);
begin
  FunctionListShown(not MenuItemFunctionList.Checked);
end;

procedure TSimbaForm.MTrayIconClick(Sender: TObject);
begin
  self.Show;
  if not SimbaSettings.Tray.AlwaysVisible.GetDefValue(True) then
    MTrayIcon.Hide;
  if Self.CanFocus then
    self.SetFocus;
end;

procedure TSimbaForm.GetSimbaNews;
begin
  try
    with TSimbaHTTPClient.Create() do
    try
      News := Get(SimbaSettings.News.URL.Value);
      if ResponseCode <> HTTP_OK then
        WriteLn('[NEWS]: Invaild response code ', ResponseCode);
    finally
      Free();
    end;
  except
    on e: Exception do
      WriteLn('[NEWS]: Exception "', e.Message, '" encountered');
  end;
end;

procedure TSimbaForm.WriteSimbaNews(Sender: TObject);
begin
  with TStringList.Create() do
  try
    Text := News; // Process line endings.

    if (Text <> '') then
      DebugMemo.Lines.Add(Text);
  finally
    Free();
  end;
end;

procedure TSimbaForm.ButtonPickClick(Sender: TObject);
var
   c, x, y: Integer;
   cobj: TColourPickerObject;
   coordinate, colour, clipboardtext: String;
begin
  if Picker.Picking then
  begin
    formWriteln('Error: Already picking a colour');
    exit;
  end;
  Picker.Pick(c, x, y);
  cobj := TColourPickerObject.Create(c, Classes.Point(x,y), '');

  { TODO: This should be no problem if the form is hidden? }
  if SimbaSettings.ColourPicker.AddToHistoryOnPick.GetDefValue(True) then
    ColourHistoryForm.AddColObj(cobj, true);

  if SimbaSettings.ColourPicker.ShowHistoryOnPick.GetDefValue(True) then
    ColourHistoryForm.Show;

  colour := inttostr(c);
  coordinate := inttostr(x) + ', ' + inttostr(y);
  formWriteLn('Picked colour: ' + colour + ' at (' + coordinate + ')');
  if (SimbaSettings.ColourPicker.AddColourToClipBoard.GetDefValue(False)) then
  begin
    clipboardtext := colour;
    if (SimbaSettings.ColourPicker.AddCoordinateToClipBoard.GetDefValue(False)) then
      clipboardtext := clipboardtext + ', ' + coordinate;
  end else
    if (SimbaSettings.ColourPicker.AddCoordinateToClipBoard.GetDefValue(False)) then
      clipboardtext := coordinate;

  if clipboardtext = '' then
    Exit;

  try
    Clipboard.AsText := clipboardtext;
  except
    on e: exception do
      mDebugLn('Exception in TSimbaForm.ButtonPickClick: ' + e.message);
  end;
end;

procedure TSimbaForm.ButtonSelectorDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Manager.SetTarget(Selector.Drag);

  formWriteLn('New window: ' + IntToStr(Selector.LastPick));
end;

procedure TSimbaForm.PageControl1Change(Sender: TObject);
begin
  RefreshTab();
  UpdateTitle();
end;

procedure TSimbaForm.ButtonTrayClick(Sender: TObject);
begin
  MTrayIcon.Show;
  self.hide;
end;

procedure TSimbaForm.PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
begin
  LastTab := PageControl1.TabIndex;

  AllowChange := True;
end;

procedure TSimbaForm.PageControl1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  PopupTab := PageControl1.IndexOfPageAt(MousePos);
  if PopupTab = -1 then
  begin
    mDebugLn('We couldn''t find which tab you clicked on, closing the popup');
    Handled := true;
  end else
  begin
    MenuItemReadOnlyTab.Checked := TMufasaTab(Tabs[PopupTab]).ScriptFrame.GetReadOnly();
  end;
end;

procedure TSimbaForm.PageControl1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  NewPos : integer;
  OldPos : integer;
begin
  if sender <> PageControl1 then
    exit;
  NewPos := PageControl1.IndexOfPageAt(Classes.Point(x,y));
  OldPos := PageControl1.TabIndex;
  if (NewPos <> OldPos) and (NewPos <> -1) then
  begin;
    Tabs.Move(OldPos,NewPos);
    PageControl1.Pages[OldPos].PageIndex := NewPos;
  end;
end;

procedure TSimbaForm.PageControl1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  Pos: Integer;
begin
  Pos := PageControl1.IndexOfPageAt(Classes.Point(x,y));
  Accept := (Pos <> PageControl1.TabIndex) and (Pos <> -1);
end;

procedure TSimbaForm.PageControl1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if(Button = mbLeft)then
  begin
    {$ifdef linux}
    PageControl1.TabIndex := PageControl1.IndexOfPageAt(Point(x,y));
    {$endif}
    PageControl1.BeginDrag(false, 10);
  end;
end;

procedure TSimbaForm.PageControl1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if(Button = mbMiddle) and (not(PageControl1.Dragging))then
    if(PageControl1.IndexOfPageAt(Classes.Point(x,y)) <> -1)then
      DeleteTab(PageControl1.IndexOfPageAt(Classes.Point(x,y)), False);
end;

procedure TSimbaForm.PopupItemFindClick(Sender: TObject);
begin
  SearchString := CurrScript.SynEdit.SelText;
  ActionFindNextExecute(ScriptPopup);
end;

function TSimbaForm.GetScriptState: TScriptState;
begin
  result := CurrScript.FScriptState;
end;

function TSimbaForm.GetShowParamHintAuto: boolean;
begin
  Result := SimbaSettings.CodeHints.ShowAutomatically.GetDefValue(True);
end;

function TSimbaForm.GetShowCodeCompletionAuto: Boolean;
begin
  Result := SimbaSettings.CodeCompletion.ShowAutomatically.GetDefValue(True);
end;

procedure TSimbaForm.SetScriptState(const State: TScriptState);
begin
  CurrScript.FScriptState:= State;
  with Self.StatusBar.panels[Panel_State] do
    case state of
      ss_Running : begin Text := 'Running'; TB_Run.Enabled:= False; TB_Pause.Enabled:= True;
                         TB_Stop.ImageIndex := Image_Stop; TB_Stop.Enabled:= True;
                         TrayPlay.Checked := True; TrayPlay.Enabled := False; TrayPause.Checked := False; TrayPause.Enabled := True;
                         TrayStop.Enabled:= True; TrayStop.Checked := False;
                   end;
      ss_Paused  : begin Text := 'Paused'; TB_Run.Enabled:= True; TB_Pause.Enabled := False;
                         TB_Stop.ImageIndex := Image_Stop; TB_Stop.Enabled:= True;
                         TrayPlay.Checked := False; TrayPlay.Enabled := True; TrayPause.Checked := True; TrayPause.Enabled := True;
                         TrayStop.Enabled:= True; TrayStop.Checked:= False;
                   end;
      ss_Stopping: begin Text := 'Stopping'; TB_Run.Enabled:= False; TB_Pause.Enabled:= False; TB_Stop.Enabled:= True;
                         TB_Stop.ImageIndex := Image_Terminate;
                         TrayPlay.Checked := False; TrayPlay.Enabled := False; TrayPause.Checked := False; TrayPause.Enabled := False;
                         TrayStop.Enabled:= True; TrayStop.Checked:= True;

                   end;
      ss_None    : begin Text := 'Done'; TB_Run.Enabled:= True; TB_Pause.Enabled:= False; TB_Stop.Enabled:= False;
                         TB_Stop.ImageIndex := Image_Stop;
                         TrayPlay.Checked := False; TrayPlay.Enabled := True; TrayPause.Checked := False; TrayPause.Enabled := False;
                         TrayStop.Enabled:= False; TrayStop.Checked:= False;
                   end;
    end;
end;


function TSimbaForm.LoadSettingDef(const Key,Def: string): string;
begin
  {$IFDEF SIMBADEBUG}
  mDebugLn('DEPRECATED LoadSettingDef call for: ' + key);
  {$ENDIF}
  result := SimbaSettings.MMLSettings.GetKeyValueDefLoad(Key,def,SimbaSettingsFile);
end;

function TSimbaForm.CreateSetting(const Key,Value: string): string;
begin
  {$IFDEF SIMBADEBUG}
  mDebugLn('DEPRECATED CreateSetting call for: ' + key);
  {$ENDIF}
  result := SimbaSettings.MMLSettings.GetKeyValueDef(Key,value);
end;

procedure TSimbaForm.SetSetting(const key,Value: string; save : boolean);
begin

  //Creates the setting if needed
  {$IFDEF SIMBADEBUG}
  mDebugLn('DEPRECATED SetSetting call for: ' + key);
  {$ENDIF}
  SimbaSettings.MMLSettings.SetKeyValue(key, value);

  if save then
  begin
    SimbaSettings.Load(SimbaSettings.MMLSettings);
    SimbaSettings.Save(SimbaSettingsFile);
    ReloadSimbaSettings(SimbaSettingsFile);
  end;
end;

function TSimbaForm.SettingExists(const key: string): boolean;
begin
  {$IFDEF SIMBADEBUG}
  mDebugLn('DEPRECATED SettingExists call for: ' + key);
  {$ENDIF}
  result := SimbaSettings.MMLSettings.KeyExists(key);
end;

procedure TSimbaForm.RegisterSettingsOnChanges;
begin
  SimbaSettings.Tray.AlwaysVisible.onChange:= @SetTrayVisiblity;
  SimbaSettings.SourceEditor.Font.onChange := @SetSourceEditorFont;
end;

procedure TSimbaForm.ScriptStartEvent(Sender: TObject; var Script: string;
  var Continue: boolean);
begin
  ScriptStartData.Sender:=Sender;
  ScriptStartData.Script:= @Script;
  ScriptStartData.Continue:= @Continue;
end;

procedure TSimbaForm.ScriptOpenEvent(Sender: TObject; var Script: string);
begin
  ScriptOpenData.Sender:=Sender;
  ScriptOpenData.Script:= @Script;
end;

procedure TSimbaForm.TT_ScriptManagerClick(Sender: TObject);
begin
  {$IFDEF USE_SCRIPTMANAGER}
  SManager.SetOptions(AppPath,SimbaSettings.ScriptManager.ServerURL.Value,SimbaSettings.ScriptManager.StoragePath.Value,SimbaSettings.ScriptManager.FileName.Value,SimbaSettings.ScriptManager.FirstRun.Value);
  if not (SManager.Visible = false) then Smanager.Show else SManager.Hide;
  {$ENDIF}
end;

procedure TSimbaForm.SetShowParamHintAuto(const AValue: boolean);
begin
  SimbaSettings.CodeHints.ShowAutomatically.Value := AValue;
end;

procedure TSimbaForm.SetShowCodeCompletionAuto(const AValue: boolean);
begin
  SimbaSettings.CodeCompletion.ShowAutomatically.Value := AValue;
end;

procedure TSimbaForm.FunctionListShown(ShowIt: boolean);
begin
  with MenuItemFunctionList, FunctionList do
  begin
    Checked := ShowIt;
    if(Checked)then
    begin
      FrameEndDock(FunctionList,FunctionList.Parent,0,0);//Set the label correctly
      if(FunctionList.Parent is TPanel)then
      begin
        SplitterFunctionList.Show;
        FunctionList.Show;
      end else
        FunctionList.Parent.Show;
    end else begin
      if(FunctionList.Parent is TPanel)then
        FunctionList.Hide
      else
        FunctionList.Parent.Hide;
      SplitterFunctionList.Hide;
    end;
  end;
end;

procedure TSimbaForm.UpdateTitle;
begin
  Application.Title:= PChar('Simba'); // XXX - Sure you want to do this for Disguise?
  if CurrScript.ScriptChanged then
  begin;
    CurrTab.TabSheet.Caption:= CurrScript.ScriptName + '*';
    Self.Caption := Format(WindowTitle,[CurrScript.ScriptName + '*']);
    ActionSaveScript.Enabled:= True;
  end else
  begin;
    ActionSaveScript.Enabled:= False;
    CurrTab.TabSheet.Caption:= CurrScript.ScriptName;
    Self.Caption := Format(WindowTitle,[CurrScript.ScriptName]);
  end;
  StatusBar.Panels[Panel_ScriptName].Text:= CurrScript.ScriptName;
  StatusBar.Panels[Panel_ScriptPath].text:= CurrScript.ScriptFile;
end;

function TSimbaForm.OpenScript: boolean;
var
  i: Integer;
  OpenInNewTab : boolean;
begin
  Result := False;
  OpenInNewTab := SimbaSettings.Tab.OpenScriptInNewTab.GetDefValue(True);
  if not OpenInNewTab then
    if CanExitOrOpen = false then
      Exit;
  with TOpenDialog.Create(nil) do
  try
    if (CurrScript.ScriptFile <> '') then
      InitialDir := ExtractFileDir(CurrScript.ScriptFile)
    else
      InitialDir := SimbaSettings.Scripts.Path.Value;
    Options := [ofAllowMultiSelect, ofExtensionDifferent, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofViewDetail];
    Filter:= 'Simba Files|*.simba;*.simb;*.cogat;*.mufa;*.txt;*.pas;|Any files|*.*';
    if Execute then
    begin
      Result := True;
      for i := 0 to Files.Count - 1 do
        if (not FileExistsUTF8(Files[i])) or (not LoadScriptFile(Files[i])) then
        begin
          Result := False;
          Break;
        end;
    end;
  finally
    Free;
  end;
  if result then
    UpdateTitle;
end;

function TSimbaForm.LoadScriptFile(filename: string; AlwaysOpenInNewTab: boolean; CheckOtherTabs : boolean
  ): boolean;
var
  OpenInNewTab : boolean;
  CheckTabsFirst : boolean;
  Tab : integer;

  script: String;

begin
  if AlwaysOpenInNewTab then
    OpenInNewTab := true
  else
    OpenInNewTab := SimbaSettings.Tab.OpenScriptInNewTab.GetDefValue(True);
  if CheckOtherTabs then
    CheckTabsFirst := True
  else
    CheckTabsFirst := SimbaSettings.Tab.CheckBeforeOpen.GetDefValue(True);
  if FileExistsUTF8(FileName) then
  begin;
    if CheckTabsFirst then
    begin;
      Tab :=  FindTab(filename);
      if tab <> -1 then
      begin
        TMufasaTab(Tabs[tab]).ScriptFrame.MakeActiveScriptFrame;
        CurrScript := TMufasaTab(Tabs[tab]).ScriptFrame;
        exit(true);
      end;
    end;
    if OpenInNewTab and (CurrScript.SynEdit.Text <> CurrScript.ScriptDefault) then //Add la tab!
      self.addtab;
    with CurrScript do
    begin
      filename := SetDirSeparators(filename);
      SynEdit.Lines.LoadFromFile(filename);

      if assigned(onScriptOpen) then
      begin
        script := SynEdit.Lines.text;
        onScriptOpen(Self, script);
        SynEdit.Lines.Text := script;
      end;

      StartText := SynEdit.Lines.text;

      ScriptName:= ExtractFileNameOnly(filename);
      mDebugLn('Script name will be: ' + ScriptName);
      ScriptFile:= FileName;
      ScriptChanged := false;
      AddRecentFile(filename);
      RefreshTab();
      Result := True;
    end;
  end;
  if Result then
    UpdateTitle;
end;

function TSimbaForm.SaveCurrentScript: boolean;
begin
  if CurrScript.GetReadOnly() then
  begin
    formWriteln('Script is in read-only/external editor mode. Not saving!');

    Exit(False);
  end;

  if not CurrScript.ScriptChanged then
  begin
    mDebugLn('SaveScript - no changes.');

    Exit(False);
  end;

  with CurrScript do
  begin
    Result := (ScriptFile <> '');

    if Result then
    begin
      try
        SynEdit.Lines.SaveToFile(ScriptFile);
      except
        mDebugLn('Cannot save the file. Try specifying a different location.');

        Exit(SaveCurrentScriptAs());
      end;
      OnSaveScript(scriptfile);
    end
    else
      Result := SaveCurrentScriptAs();
  end;
end;

function TSimbaForm.SaveCurrentScriptAs: boolean;
var
  ScriptFile : string;
begin
  Result := false;
  with TSaveDialog.Create(nil) do
  try
    if (CurrScript.ScriptFile <> '') then
      InitialDir := ExtractFileDir(CurrScript.ScriptFile)
    else
      InitialDir := SimbaSettings.Scripts.Path.Value;
    filter := 'Simba Files|*.simba;*.simb;*.cogat;*.mufa;*.txt;*.pas;|Any files|*.*';
    if Execute then
    begin;
      if ExtractFileExt(FileName) = '' then
      begin;
        ScriptFile := FileName + '.simba';
      end else
        ScriptFile := FileName;
      CurrScript.SynEdit.Lines.SaveToFile(ScriptFile);
      OnSaveScript(scriptfile);
    end;
  finally
    free;
  end;
end;

function TSimbaForm.SaveCurrentScriptAsDefault : boolean;
begin
  with CurrScript do
  begin
    try
      SynEdit.Lines.SaveToFile(SimbaSettings.SourceEditor.DefScriptPath.Value);
      mDebugLn('Script saved as default.');
      Result := True;
    except
      mDebugLn('Cannot save script as default.');
      Result := False;
    end;
  end;
end;

function TSimbaForm.CanExitOrOpen: boolean;
begin;
  Self.Enabled := False;//We HAVE to answer the popup
  Result := True;

  if (ScriptState <> ss_None) and SimbaSettings.Misc.WarnIfRunning.GetDefValue(true) then
  begin
    if ScriptState <> ss_Stopping then
    begin
      result := False;
      case MessageDlg('Script is still running', 'Do you want to stop the script?',
                        mtConfirmation, mbYesNoCancel, 0) of
                          mrYes: StopScript;
                        end;
    end else
      case MessageDlg('Script is stopping.', 'Do you want to terminate the script?',
                      mtConfirmation, mbYesNoCancel, 0) of
                        mrNo, mrCancel: Result := false;
                        mrYes: StopScript;
                      end;
  end;

  if Result and (CurrScript.StartText <> CurrScript.SynEdit.Lines.text) and
     SimbaSettings.Misc.WarnIfModified.GetDefValue(true) then
  begin
    case MessageDlg('Script has been modified.', 'Do you want to save the script?',
                mtConfirmation, mbYesNoCancel, 0) of
          mrCancel : Result := False;
          mrYes : Result := SaveCurrentScript;
    end;
  end;
  Self.Enabled := True;
  if Self.CanFocus then
    Self.SetFocus;
end;

function TSimbaForm.ClearScript: boolean;
begin
  result := false;
  if CanExitOrOpen then
  begin;
    result := true;
    CurrTab.Clear;
    RefreshTab();
  end;
end;


{ TMufasaTab }

procedure TMufasaTab.Clear;
begin
  ScriptFrame.Free;
  ScriptFrame := TScriptFrame.Create(Tabsheet);
  ScriptFrame.Parent := Tabsheet;
  ScriptFrame.Align:= alClient;
end;

constructor TMufasaTab.Create(Page: TPageControl);
begin
  inherited Create;
  PageCtrl := Page;
  Tabsheet := TTabSheet.Create(Page);
  Tabsheet.PageControl := Page;
  ScriptFrame := TScriptFrame.Create(Tabsheet);
  ScriptFrame.Parent := Tabsheet;
  ScriptFrame.Align := alClient;
end;

destructor TMufasaTab.Destroy;
begin
//  ScriptFrame.Free;
  TabSheet.Free;
  inherited Destroy;
end;

initialization
  FormatSettings.DecimalSeparator := '.';

  {$R *.lfm}

end.

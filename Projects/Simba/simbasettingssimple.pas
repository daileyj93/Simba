unit simbasettingssimple;

{$mode objfpc}{$H+}

{

Settings:
    - General

        -   Updater:
            X   Check for updates
            X   Check every X minutes
            -   URLs

        -   Interpreter
            X   Lape / PS / others
            -   Allow SysCalls

    - Environment

        X   Code Tools:
            X   Automatically show hints
            X   Automatically show completion

        X   Tab options:
            X   Open Next on Close
            X   Open Script in new Tab
            X   Check TabGeneral for open script before opening

        X   Colour Picker:
            X   Show history on pick
            X   Add to history on pick

        X   Source Editor
            X   LazColors (Boolean)
            X   Default Script (Path)
            X   CaretPastEOL (Boolean)

        X   Tray:
            X   Always Visible

        X   Function List:
            X   ShowOnStart

        -   Show Command Prompt (Windows only)

    - Advanced

        -   Paths:
            Includes
            Plugins
            Fonts
            Extensions
            Scripts

}

{$I Simba.inc}


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, types;

type
  TSettingsSimpleForm = class(TForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    CheckForUpdatesBox: TCheckBox;
    AutomaticallyUpdateBox: TCheckBox;
    CaretPastEOL: TCheckBox;
    TabAdvanced: TPage;
    TabEnvironment: TPage;
    Tabs: TNotebook;
    TabGeneral: TPage;
    UpdaterURLLabel: TLabel;
    UpdaterURLVersionLabel: TLabel;
    UpdaterURLVersion: TEdit;
    UpdaterURL: TEdit;
    HighlightLazColours: TCheckBox;
    CodeToolsCheckBoxes: TCheckGroup;
    TabSettingsCheckBoxes: TCheckGroup;
    ColourPickerCheckGroup: TCheckGroup;
    EnvOther: TCheckGroup;
    UpdateMinutesEdit: TEdit;
    DefaultScriptedit: TEdit;
    UpdaterGroup: TGroupBox;
    GroupBox2: TGroupBox;
    ImageList1: TImageList;
    UpdateCheckMinutes: TLabel;
    Label2: TLabel;
    PgControlEnvironment: TPageControl;
    PgControlAdvanced: TPageControl;
    SettingsTabsList: TListView;
    PgControlGeneral: TPageControl;
    SettingsTabsPanel: TPanel;
    PathsTreeView: TTreeView;
    tsEditor: TTabSheet;
    tsOther: TTabSheet;
    tsUpdater: TTabSheet;
    tsTabs: TTabSheet;
    tsAdvanced: TTabSheet;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure EnvOtherItemClick(Sender: TObject; Index: integer);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SettingsTabsListAdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure SettingsTabsListClick(Sender: TObject);
    procedure SettingsTabsListMouseLeave(Sender: TObject);
    procedure SettingsTabsListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SwitchSettingsTab(NewTab: Integer);
    procedure HighlightSettingsTab(NewTab: Integer);
  private
  public
  end; 

var
  SettingsSimpleForm: TSettingsSimpleForm;
  SettingsTabState: array of Integer;
  ClickSettingsTab: Boolean;

implementation

uses
  simbaunit, newsimbasettings;

{$R *.lfm}

procedure TSettingsSimpleForm.FormCreate(Sender: TObject);
begin
  SetLength(SettingsTabState, SettingsTabsList.Items.Count);
end;

procedure TSettingsSimpleForm.ButtonOKClick(Sender: TObject);
var
  N: TTreeNode;
  PerformUpdate: Boolean;

begin
  PerformUpdate := False;
  // Updater

  SimbaSettings.Updater.CheckForUpdates.Value := CheckForUpdatesBox.Checked;
  SimbaSettings.Updater.AutomaticallyUpdate.Value := AutomaticallyUpdateBox.Checked;
  SimbaSettings.Updater.CheckEveryXMinutes.Value := StrToIntDef(UpdateMinutesEdit.Text, 30);

  if (SimbaSettings.Updater.RemoteLink.Value <> UpdaterURL.Text) or
     (SimbaSettings.Updater.RemoteVersionLink.Value <> UpdaterURLVersion.Text) then
  begin
    MessageDlg('It appears you changed the Simba updater links. ' +
     'If you went from a newer version to an older version (from Unstable to Stable) ' +
     'You will have to force an update.', mtInformation, mbOKCancel, 0);
  end;
  SimbaSettings.Updater.RemoteLink.Value := UpdaterURL.Text;
  SimbaSettings.Updater.RemoteVersionLink.Value := UpdaterURLVersion.Text;

  // TabGeneral
  SimbaSettings.Tab.OpenNextOnClose.Value := TabSettingsCheckBoxes.Checked[0];
  SimbaSettings.Tab.OpenScriptInNewTab.Value := TabSettingsCheckBoxes.Checked[1];
  SimbaSettings.Tab.CheckBeforeOpen.Value := TabSettingsCheckBoxes.Checked[2];
  SimbaSettings.Tab.CurrentScriptDebug.Value := TabSettingsCheckBoxes.Checked[3];

  // Code Tools
  SimbaSettings.CodeHints.ShowAutomatically.Value := CodeToolsCheckBoxes.Checked[0];
  SimbaSettings.CodeCompletion.ShowAutomatically.Value := CodeToolsCheckBoxes.Checked[1];

  // Colour Picker
  SimbaSettings.ColourPicker.ShowHistoryOnPick.Value := ColourPickerCheckGroup.Checked[0];
  //SimbaSettings.ColourPicker.AddToHistoryOnPick.SetValue(ColourPickerCheckGroup.Checked[1]);

  // Source Editor
  SimbaSettings.SourceEditor.LazColors.Value := HighlightLazColours.Checked;
  SimbaSettings.SourceEditor.DefScriptPath.Value := DefaultScriptedit.Text;
  SimbaSettings.SourceEditor.CaretPastEOL.Value := CaretPastEOL.Checked;

  // Other
  SimbaSettings.Tray.AlwaysVisible.Value := EnvOther.Checked[0];
  SimbaSettings.CodeInsight.FunctionList.ShowOnStart.Value := EnvOther.Checked[1];
  SimbaSettings.Misc.SaveScriptOnCompile.Value := EnvOther.Checked[2];
  // Add 'Show Command prompt'

  // Paths
  // Paths
  N := PathsTreeView.Items.FindNodeWithText('Includes');
  if (N <> nil) and (N.HasChildren) then
    SimbaSettings.Includes.Path.Value := N.GetLastChild.Text;

  N := PathsTreeView.Items.FindNodeWithText('Extensions');
  if (N <> nil) and (N.HasChildren) then
    SimbaSettings.Plugins.Path.Value := N.GetLastChild.Text;

  N := PathsTreeView.Items.FindNodeWithText('Plugins');
  if (N <> nil) and (N.HasChildren) then
    SimbaSettings.Plugins.Path.Value := N.GetLastChild.Text;

  N := PathsTreeView.Items.FindNodeWithText('Fonts');
  if (N <> nil) and (N.HasChildren) then
    SimbaSettings.Fonts.Path.Value := N.GetLastChild.Text;

  N := PathsTreeView.Items.FindNodeWithText('Scripts');
  if (N <> nil) and (N.HasChildren) then
    SimbaSettings.Scripts.Path.Value := N.GetLastChild.Text;

  ModalResult := mrOK;

  if PerformUpdate then
  begin
    SimbaForm.UpdateSimbaSilent(True);
  end;
end;

procedure TSettingsSimpleForm.EnvOtherItemClick(Sender: TObject; Index: Integer);
begin
  case Index of
    0: SimbaSettings.Tray.AlwaysVisible.Value:= EnvOther.Checked[0];
    1: SimbaSettings.CodeInsight.FunctionList.ShowOnStart.Value := EnvOther.Checked[1];
    2: SimbaSettings.Misc.SaveScriptOnCompile.Value := EnvOther.Checked[2];
  end;
end;

procedure TSettingsSimpleForm.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSettingsSimpleForm.FormShow(Sender: TObject);
var N: TTreeNode;
begin
  // Updater
  CheckForUpdatesBox.Checked := SimbaSettings.Updater.CheckForUpdates.Value;
  AutomaticallyUpdateBox.Checked := SimbaSettings.Updater.AutomaticallyUpdate.Value;
  UpdateMinutesEdit.Text := IntToStr(SimbaSettings.Updater.CheckEveryXMinutes.Value);
  UpdaterURL.Text := SimbaSettings.Updater.RemoteLink.Value;
  UpdaterURLVersion.Text := SimbaSettings.Updater.RemoteVersionLink.Value;


  // TabGeneral
  TabSettingsCheckBoxes.Checked[0] := SimbaSettings.Tab.OpenNextOnClose.Value;
  TabSettingsCheckBoxes.Checked[1] := SimbaSettings.Tab.OpenScriptInNewTab.Value;
  TabSettingsCheckBoxes.Checked[2] := SimbaSettings.Tab.CheckBeforeOpen.Value;
  TabSettingsCheckBoxes.Checked[3] := SimbaSettings.Tab.CurrentScriptDebug.Value;

  // Code Tools
  CodeToolsCheckBoxes.Checked[0] := SimbaSettings.CodeHints.ShowAutomatically.Value;
  CodeToolsCheckBoxes.Checked[1] := SimbaSettings.CodeCompletion.ShowAutomatically.Value;

  // Colour Picker
  ColourPickerCheckGroup.Checked[0] := SimbaSettings.ColourPicker.ShowHistoryOnPick.Value;
  //ColourPickerCheckGroup.Checked[1] := SimbaSettings.ColourPicker.AddToHistoryOnPick.GetValue; // Wizzup: This doesn't actually get set?

  // Source Editor
  HighlightLazColours.Checked := SimbaSettings.SourceEditor.LazColors.Value;
  DefaultScriptedit.Text := SimbaSettings.SourceEditor.DefScriptPath.Value;
  CaretPastEOL.Checked := SimbaSettings.SourceEditor.CaretPastEOL.Value;
  
  // Other
  EnvOther.Checked[0] := SimbaSettings.Tray.AlwaysVisible.Value;
  EnvOther.Checked[1] := SimbaSettings.CodeInsight.FunctionList.ShowOnStart.Value;
  EnvOther.Checked[2] := SimbaSettings.Misc.SaveScriptOnCompile.Value;


  // Add 'Show Command prompt', not set in SimbaSettings?

  // Paths
  N := PathsTreeView.Items.FindNodeWithText('Includes');
  if (N <> nil) and (N.HasChildren) then
    N.GetLastChild.Text:= SimbaSettings.Includes.Path.Value;

  N := PathsTreeView.Items.FindNodeWithText('Plugins');
  if (N <> nil) and (N.HasChildren) then
    N.GetLastChild.Text:= SimbaSettings.Plugins.Path.Value;

  N := PathsTreeView.Items.FindNodeWithText('Fonts');
  if (N <> nil) and (N.HasChildren) then
    N.GetLastChild.Text:= SimbaSettings.Fonts.Path.Value;

  N := PathsTreeView.Items.FindNodeWithText('Scripts');
  if (N <> nil) and (N.HasChildren) then
    N.GetLastChild.Text:= SimbaSettings.Scripts.Path.Value;

  // Form stuff
  SettingsTabState[0] := 1;
  SettingsTabState[1] := 0;
  SettingsTabState[2] := 0;
end;

// Part of Faux TabGeneral - Controls switching of TabGeneral
procedure TSettingsSimpleForm.SwitchSettingsTab(NewTab: Integer);
var i: Integer;
begin
  for i := 0 to High(SettingsTabState) do
    if (i = NewTab) then
      SettingsTabState[i] := 1
    else
      SettingsTabState[i] := 0;

  Tabs.PageIndex := NewTab;

  SettingsTabsList.Refresh();
end;

// Part of Faux TabGeneral - Controls the highlight state
procedure TSettingsSimpleForm.HighlightSettingsTab(NewTab: Integer);
var i: Integer;
begin
  for i := 0 to High(SettingsTabState) do
  begin
    if SettingsTabState[i] = 1 then
      Continue;
    if i = NewTab then
      SettingsTabState[i] := 2
    else
    begin
      SettingsTabState[i] := 0;
    end;
  end;

  SettingsTabsList.Refresh;
end;

// Part of Faux TabGeneral - Custom draws TabGeneral
procedure TSettingsSimpleForm.SettingsTabsListAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  ItemBoundsRect, IconRect, LabelRect: TRect;
begin
  if Stage = cdPostPaint then
  begin
    ItemBoundsRect := item.DisplayRect(drBounds);

    case SettingsTabState[Item.Index] of
        0:  begin
              Sender.Canvas.Brush.Color := clWhite;
              Sender.Canvas.FillRect(ItemBoundsRect);
            end;
        1:  begin
              Sender.Canvas.Brush.Color := $00EED2C1;
              Sender.Canvas.FillRect(ItemBoundsRect);
            end;
        2:  begin
              Sender.Canvas.Brush.Color := $00F6E8E0;
              Sender.Canvas.FillRect(ItemBoundsRect);
            end;
    end;

    IconRect := Item.DisplayRect(drIcon);
    LabelRect := Item.DisplayRect(drLabel);

    ImageList1.Draw(Sender.Canvas, ItemBoundsRect.Left + (((ItemBoundsRect.Right - ItemBoundsRect.Left) div 2) - 16), IconRect.Top+3, Item.ImageIndex);
    Sender.Canvas.TextOut(LabelRect.Left+2, LabelRect.Top, Item.Caption);
  end;
end;

// Part of Faux TabGeneral - OnClick
procedure TSettingsSimpleForm.SettingsTabsListClick(Sender: TObject);
var
  x, y: Integer;
  f: TListItem;
begin
  x := ScreenToClient(Mouse.CursorPos).x;
  y := ScreenToClient(Mouse.CursorPos).y;

  f := SettingsTabsList.GetItemAt(x, y);

  if (f = nil) then
    Exit;

  SwitchSettingsTab(f.Index);
end;

// Part of Faux TabGeneral - On mouse leave
procedure TSettingsSimpleForm.SettingsTabsListMouseLeave(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to High(SettingsTabState) do
  begin
    if SettingsTabState[i] = 1 then
      Continue;
    SettingsTabState[i] := 0;
  end;

  SettingsTabsList.Repaint;
end;

// Part of Faux TabGeneral - On mouse move
procedure TSettingsSimpleForm.SettingsTabsListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  f: TListItem;
begin
  f := SettingsTabsList.GetItemAt(x, y);
  if (f = nil) then
    Exit;
  HighlightSettingsTab(f.Index);
end;

end.


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

    about form for the Mufasa Macro Library
}
unit psextension;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, virtualextension, forms, client,
  lpparser, lpcompiler, lptypes, lpvartypes, lpmessages, lpinterpreter,
  stdCtrls, MufasaTypes, MufasaBase, web,
  bitmaps, libloader, dynlibs,internets, settingssandbox, updater;


{$I Simba.inc}


type

    { TSimbaPSExtension }

  TSimbaPSExtension = class(TVirtualSimbaExtension)
  private
    FCompiler: TLapeCompiler;
    FWorking: Boolean;
    Script: TStringList;
    FClient : TClient;
    procedure StartExtension;
    function FreeScript: boolean;
    function InitScript: Boolean;
    procedure OutputMessages;
  protected
    function OnNeedFile(Sender: TObject;const OrginFileName: string; var FilePath, Output: string): Boolean;
    procedure HandleException(e: Exception);
    function Import: Boolean;
    function Compile: Boolean;
    procedure SetEnabled(bool : boolean);override;
  public
    constructor Create(FileStr: String; StartDisabled : boolean = false);
    destructor Destroy; override;
    function HookExists(const HookName: String): Boolean;override;
    function ExecuteHook(const HookName: String;var Args:TVariantArray; out OutVariant : Variant): Integer;override;
    property Working : boolean read FWorking;
  end;

implementation
uses
  colour_conv,dtmutil,LConvEncoding,
  {$ifdef mswindows}windows,  MMSystem,{$endif}//MMSystem -> Sounds
  fontloader,
  IOmanager,//TTarget_Exported
  IniFiles,//Silly INI files
  stringutil, //String st00f
  newsimbasettings, // SimbaSettings
  simba.environment,
  script_imports,

  files,
  dialogs,
  dtm, //Dtms!
  Graphics, //For Graphics types
  math, //Maths!
  mmath, //Real maths!
  mmltimer,
  strutils,
  fileutil,
  tpa, //Tpa stuff
  lclintf,
  httpsend,
  fpjson, jsonparser,
  Clipbrd,

  DCPcrypt2,
  DCPrc2, DCPrc4, DCPrc5, DCPrc6,
  DCPhaval,
  DCPmd4, DCPmd5,
  DCPripemd128, DCPripemd160,
  DCPsha1, DCPsha256, DCPsha512,
  DCPtiger

  {$IFDEF USE_SQLITE}, msqlite3{$ENDIF}

  , SimbaUnit, updateform, mmisc;  // for GetTickCount and others.//Writeln

{$MACRO ON}
{$define extdecl := register}

procedure psWriteLn(s: string);
begin
  formWriteln(s);
end;

function TSimbaPSExtension.HookExists(const HookName: String): Boolean;
begin
  Result := False;
  if FWorking then
    if FCompiler.getGlobalVar(HookName) <> nil then
      result := True;
end;

function TSimbaPSExtension.ExecuteHook(const HookName: String;var Args: TVariantArray; out OutVariant : Variant): Integer;
var
  VarStack : TByteArray;
  arg : Variant;
  b : Integer;
begin
  psWriteLn('EXECUTE HOOK');
  result := SExt_error;
  if not FWorking then
    exit;
  try
    b := 0;
    SetLength(VarStack, 1000);
    psWriteLn(hookName);
    psWriteLn('ARGS size: ' + IntToStr(SizeOf(Args)));
    psWriteLn('arg[0] size: ' + IntToStr(SizeOf(Args[0])));
    psWriteLn('ARGS length: ' + IntToStr(Length(Args)));
    for arg in Args do
    begin
      //Move(arg, VarStack[b], sizeof(arg));
      psWriteLn('arg size: ' + IntToStr(Sizeof(arg)));
      Inc(b, SizeOf(arg));
    end;
    psWriteLn('running code: ');
    PPointer(@VarStack[b])^ := @OutVariant;
    RunCode(FCompiler.Emitter.Code, FCompiler.Emitter.CodeLen, VarStack, TCodePos(FCompiler.getGlobalVar(HookName).Ptr^));
    //OutVariant := VarStack[b];
    psWriteLn(outVariant);
    result := SExt_ok;
  except
    on e : exception do
      psWriteLn(format('Error in Simba extension (%s): %s',[Self.GetName,e.message]));
  end;
end;

constructor TSimbaPSExtension.Create(FileStr: String; StartDisabled: boolean = false);
begin
  inherited create;
  FWorking := False;
  FClient := TClient.Create('',SimbaForm.Manager);
  FileName := FileStr;
  try
    Script := TStringList.Create;
    Script.LoadFromFile(FileName);
  except
    raise Exception.CreateFmt('File %s could not be read', [FileName]);
  end;
  FEnabled := false;
  FCompiler := nil;
  if not StartDisabled then
    StartExtension;
end;

function TSimbaPSExtension.InitScript: Boolean;
begin
  if not HookExists('init') then
    exit(false);
  result := true;
  try
	RunCode(FCompiler.Emitter.Code, FCompiler.Emitter.CodeLen, nil, TCodePos(FCompiler.getGlobalVar('init').Ptr^));
  except
    result := false;
  end;
end;

function TSimbaPSExtension.FreeScript: boolean;
var
  bla : variant;
  Args : TVariantArray;
begin
  if not HookExists('Free') then
    exit(false);
  result := ExecuteHook('Free',Args,bla) = SExt_ok;
end;

procedure TSimbaPSExtension.SetEnabled(bool: boolean);
var
  temp : variant;
  Args : TVariantArray;
begin
  if bool <> FEnabled then
  begin
    if bool then
    begin;
      if not assigned(FCompiler) then //We enable it for the first time, calls SetEnabled.
        StartExtension
      else
      begin
        if not FWorking then
          Exit;
        if hookexists('attach') then
          ExecuteHook('attach',Args,temp);
      end;
    end else
      if HookExists('detach') then
        ExecuteHook('detach',Args,temp);
  end;
  inherited SetEnabled(bool);
end;


procedure TMufasaBitmapCopyClientToBitmap(self : TMufasaBitmap; Resize : boolean;x,y : integer; xs, ys, xe, ye: Integer);
begin
  self.CopyClientToBitmap(SimbaForm.Manager,resize,x,y,xs,ys,xe,ye);
end;

destructor TSimbaPSExtension.Destroy;
begin
  FClient.free;
  FreeScript;
  if Assigned(FCompiler) then
    FreeAndNil(FCompiler);
  Script.Free;
  inherited;
end;

function TSimbaPSExtension.OnNeedFile(Sender: TObject;
  const OrginFileName: string; var FilePath, Output: string): Boolean;
var
  Path: string;
begin
  Path := FilePath;
  with SimbaForm do
    Result := FindFile(Path, [SimbaEnvironment.IncludePath,
                              SimbaEnvironment.ExtensionPath,
                              ExtractFileDir(Filename),
                              ExtractFileDir(OrginFileName)]);

  if (not (Result)) then
  begin
    psWriteln(FilePath + ' doesn''t exist');
    Exit;
  end;

  FilePath := Path;

  try
    with TFileStream.Create(FilePath, fmOpenRead) do
    try
      SetLength(Output, Size);
      Read(Output[1], Size);
    finally
      Free;
    end;
  except
    Result := False;
    psWriteln('TSimbaPSExtension.OnNeedFile');
  end;
end;

procedure TSimbaPSExtension.HandleException(e: Exception);
begin
  //if (Error.Callback <> nil) and (Error.Data <> nil) then
  //begin
  //  Self.Error.Data^ := Default(TErrorData);
  //
  //  if (e is lpException) then
  //  begin
  //    with (e as lpException) do
  //    begin
  //      Self.Error.Data^.Line := DocPos.Line;
  //      Self.Error.Data^.Col := DocPos.Col;
  //      Self.Error.Data^.FilePath := DocPos.FileName;
  //      Self.Error.Data^.Error := Message;
  //    end;
  //  end else
  //    Self.Error.Data^.Error := 'ERROR: ' + e.ClassName + ' :: ' + e.Message;
  //
  //  Synchronize(Error.Callback);
  //end;
end;

function TSimbaPSExtension.Import: Boolean;
var
  i: Int32;
begin
  Result := False;

  FCompiler.StartImporting();

  try
    for i := 0 to ScriptImports.Count - 1 do
      ScriptImports.Import(ScriptImports.Keys[i], FCompiler, Self);

    Result := True;
  except
    on e: Exception do
      HandleException(e);
  end;

  FCompiler.EndImporting();
end;

function TSimbaPSExtension.Compile: Boolean;
var
  T: UInt64;
begin
  Result := False;

  try
    T := GetTickCount64();

    if FCompiler.Compile() then
    begin
      FormWriteLn('Compiled successfully in ' + IntToStr(GetTickCount64() - T) + ' ms.');
      //FormWriteLn();

      Result := True;
    end;
  except
    on e: Exception do
      HandleException(e);
  end;
end;

procedure TSimbaPSExtension.StartExtension;
begin
  if assigned(FCompiler) then
    exit;//Already started..

  FCompiler := TLapeCompiler.Create(TLapeTokenizerString.Create(Self.Script.Text, FileName));
  //FCompiler.OnFindFile := @OnNeedFile;

  Import;

  formWriteln(Format('Loading extension %s', [FileName]));
  try
    FWorking := Compile;
  except
    on e : exception do
      FormWriteln(format('Error in Simba extension compilation (%s) : %s',[FileName,e.message]));
  end;

  if FWorking then
  begin
    formWriteln('Extension Enabled');

    if InitScript then
      mDebugLn('Init procedure successfully called')
    else
      mDebugLn('Init procedure didn''t execute right, or couldn''t be found');
  end else
  begin
    formWriteln('Extension Disabled - Did not compile');
    OutputMessages;
  end;

  Enabled:= FWorking;
end;

procedure TSimbaPSExtension.OutputMessages;
var
  l: Longint;
begin
  //for l := 0 to PSInstance.CompilerMessageCount - 1 do
  //  formWritelnEx(PSInstance.CompilerErrorToStr(l) + ' at line ' + inttostr(PSInstance.CompilerMessages[l].Row));
end;


end.

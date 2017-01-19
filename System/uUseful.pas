{
  Laz-Model
  Copyright (C) 2002  Eldean AB, Peter Söderman, Ville Krumlinde
  Portions (C) 2016 Peter Dyson. Initial Lazarus port

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

unit uUseful;

{$mode objfpc}{$H+}

interface


uses Classes, SysUtils, Forms, Controls, Dialogs, ComCtrls;

type
  IEldeanProgress = interface(IUnknown)
    ['{E446EEFB-DABB-4AD9-BE49-104A6F265CB4}']
    procedure Tick;
  end;

  TEldeanProgress = class(TInterfacedObject,IEldeanProgress)
   public
     constructor Create(Text : string; Max : integer); reintroduce;
     destructor Destroy; override;
     procedure Tick;
   private
     P : TProgressBar;
     F : TForm;
     AbortNext : boolean;
   end;

  TBrowseForFolderDialog = class
  private
    FTitle,FPath : string;
  public
    property Title: string read FTitle write FTitle;
    function Execute: Boolean;
    property Path: string read FPath write FPath;
  end;

  function MakeTempDir : string;

implementation

var
  RecentOpenFolderPath : string;

constructor TEldeanProgress.Create(Text: string; Max: integer);
begin
  F := TForm.Create(Application.MainForm);

  F.BorderIcons := [];
  F.BorderStyle := bsDialog;
  F.Caption := Text;
  F.ClientHeight := 22;
  F.ClientWidth := 390;
  F.Position := poScreenCenter;

  P := TProgressBar.Create(F);
  P.Parent := F;
  P.Align := alTop;
  P.Height := 22;
  P.Max := Max;
  P.Step := 1;
  P.Smooth := True;

  F.Show;
end;

destructor TEldeanProgress.Destroy;
begin
  FreeAndNil(F);
  inherited;
end;

procedure TEldeanProgress.Tick;
begin
  if AbortNext then
    Abort;
  P.StepIt;
  Application.ProcessMessages;
end;

function TBrowseForFolderDialog.Execute: Boolean;
var
  F : TSelectDirectoryDialog;
begin

  F := TSelectDirectoryDialog.Create(Application.MainForm);
  if Length(FPath) > 0 then
      F.InitialDir := FPath
    else
      F.InitialDir := RecentOpenFolderPath;
  F.Title := Title;
  if F.Execute then
    begin
      FPath := f.FileName;
      RecentOpenFolderPath := FPath;
      Result := True;
    end
  else
    Result := False;

  F.Free;
end;

var
  CleanUp : TStringList;

function MakeTempDir : string;
var
  TempPath : string;
  I : integer;
  Ok : boolean;
begin

  TempPath := GetTempDir(false);
  Ok := False;
  for I := 0 to 50 do
  begin
    Result := TempPath + 'Essmodel' + IntToStr(I);
    if not DirectoryExists(Result) then
    begin
      MkDir( Result );
      Ok := True;
      Result := Result;
      CleanUp.Add(Result);
      Break;
    end;
  end;
  if not Ok then
    raise Exception.Create('Failed to create temp directory');
end;

procedure DoCleanUp;
var
  I : integer;
  DirInfo: TSearchRec;
  Res: integer;
  S : string;
begin
  for I := 0 to CleanUp.Count-1 do
  begin
    S := CleanUp[I];
    if Pos('Essmodel',S)=0 then
      Continue;  //Safety
    Res := SysUtils.FindFirst(S + '\*.*', 0, DirInfo);
    while Res = 0 do
    begin
      SysUtils.DeleteFile(S + '\' + DirInfo.Name);
      Res := SysUtils.FindNext(DirInfo);
    end;
    SysUtils.FindClose(DirInfo);
    RemoveDir(S);
  end;
end;

initialization
  CleanUp := TStringList.Create;
finalization
  DoCleanUp;
  CleanUp.Free;
end.

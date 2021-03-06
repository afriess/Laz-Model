{
  Laz-Model
  Copyright (C) 2002  Eldean AB, Peter S�derman, Ville Krumlinde
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

unit uMainForm;

{$mode objfpc}{$H+}

interface


uses
  Classes, Math, Forms, ExtCtrls, Menus, Buttons, DefaultTranslator,
  uMainModule, uClassTreeEditForm, uConst;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    TreeEdit: TMenuItem;
    Printdiagram1: TMenuItem;
    Generatedocumentation1: TMenuItem;
    About1: TMenuItem;
    ExportXmiAction1: TMenuItem;
    Diagram1: TMenuItem;
    Copydiagramtoclipboard1: TMenuItem;
    Layoutdiagram1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    N3: TMenuItem;
    Changesettings1: TMenuItem;
    StatusPanel: TPanel;
    Unhidediagramelements1: TMenuItem;
    Help: TMenuItem;
    ReopenMenuItem: TMenuItem;
    LeftPanel: TPanel;
    DiagramPanel: TPanel;
    Splitter1: TSplitter;
    ZoomPanel: TPanel;
    TreePanel: TPanel;
    Splitter2: TSplitter;
    Panel1: TPanel;
    Saveaspicture1: TMenuItem;
    Previewdocumentation1: TMenuItem;
    OpenFolderAction1: TMenuItem;
    ExportmodeltoEMXfile1: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var aAction: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OpenFolderActionExecute(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Height := Max( Round(Screen.Height * 0.75), 480 );
  Width := Max( Round(Screen.Width * 0.75) , 640 );
  {$IFDEF DEBUG}
  ClassTreeEditForm := TClassTreeEditForm.Create(Self);
  {$ENDIF DEBUG}
  MainModule := TMainModule.Create(Nil);
end;

procedure TMainForm.OpenFolderActionExecute(Sender: TObject);
begin

end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  Caption := uConst.ProgName;
end;

procedure TMainForm.FormClose(Sender: TObject; var aAction: TCloseAction);
begin
  // Free here istead of FormDestroy, otherwise SaveChanged-dialog for a diagram will not show.
  MainModule.Free;
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

end.


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

unit uAboutForm;

{$mode objfpc}{$H+}

interface

uses
  Forms, ExtCtrls, StdCtrls,
  uConst;

type

  TAboutForm = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Image2: TImage;
    PortedLabel: TLabel;
    NameLabel: TLabel;
    RegLabel: TMemo;
    UrlLabel: TLabel;
    IconImage: TImage;
    CopyrightLabel: TLabel;
    MailLabel: TLabel;
    Label1: TLabel;
    procedure FormActivate(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

procedure TAboutForm.FormActivate(Sender: TObject);
begin
  IconImage.Picture.Icon.LoadFromResourceName(HInstance,'MAINICON');
  NameLabel.Caption := uConst.ProgName + ' ' + uConst.ProgVersion;
  CopyrightLabel.Caption := uConst.ProgCopyright;
  PortedLabel.Caption := uConst.ProgPorted;
  UrlLabel.Caption := uConst.ProgUrl;
  MailLabel.Caption := 'Contact: ' + uConst.ProgMail;
end;

end.


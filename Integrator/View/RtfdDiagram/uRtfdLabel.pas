{
  Laz-Model
  Copyright (C) 2017 Peter Dyson. Initial Lazarus port
  Portions (C) 2002  Eldean AB, Peter Söderman, Ville Krumlinde

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
unit uRtfdLabel;

{$mode objfpc}{$H+}

interface
uses
  Classes, LCLIntf, LCLType, Controls, ExtCtrls, Graphics,
  uModel, uModelEntity, uListeners, uConfig;

const
  ClassShadowWidth = 3;
  cDefaultWidth = 185;
  cDefaultHeight = 41;
  cDefaultLeft = 4;
  cDefaultRight = cDefaultWidth-cDefaultLeft-ClassShadowWidth;
  cDefaultlblHeight = 15;
  cIconW = 10;
  cMargin = 4;

var

  TopColor : array[boolean] of TColor = ($EAF4F8, clWhite);

type

  TRtfdODLabel = class(TComponent, IModelEntityListener)
  private
    FOwner: TCustomControl;
    FCaption: TCaption;
    FCanvas: TCanvas;
    FAlignment: TAlignment;
    FTransparent: Boolean;
    fFont: TFont;
    function GetAlignment: TAlignment;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetTransparent(const Value: Boolean);
    procedure SetWantedBounds;
  protected
    FBox: TRect;
    Entity: TModelEntity;
    procedure Paint; virtual;
    procedure SetText(const Value: TCaption);
    function GetText: TCaption;
  public
    constructor Create(AOwner: TComponent; AEntity: TModelEntity; Tp: integer); reintroduce; virtual;
    destructor Destroy; override;
    procedure Change(Sender: TModelEntity); virtual;
    procedure Paint(width: integer); virtual;
    procedure AddChild(Sender: TModelEntity; NewChild: TModelEntity); virtual;
    procedure Remove(Sender: TModelEntity); virtual;
    procedure EntityChange(Sender: TModelEntity); virtual;
    function WidthNeeded : integer; virtual;
    function Height:  integer;
    property ModelEntity: TModelEntity read Entity;
    property Alignment: TAlignment read GetAlignment write SetAlignment default taLeftJustify;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property Canvas: TCanvas read FCanvas write FCanvas;
    property Parent: TCustomControl read FOwner;
    property Font: TFont read fFont write fFont;
    property Caption: TCaption read FCaption write FCaption;
  end;

implementation

function TRtfdODLabel.GetAlignment: TAlignment;
begin
  Result := FAlignment;
end;

procedure TRtfdODLabel.SetAlignment(const Value: TAlignment);
begin
  if Value <> FAlignment then
    begin
    FAlignment := Value;
    FOwner.Invalidate;
  end;
end;

procedure TRtfdODLabel.SetTransparent(const Value: Boolean);
begin
  if Value <> FTransparent then
    begin
    FTransparent := Value;
    FOwner.Invalidate;
  end;
end;

procedure TRtfdODLabel.SetWantedBounds;
var
  Al: integer;
  Rect: TRect;
  DC: HDC;
  txt: string;
begin

  Rect := FBox;
  txt := FCaption + '   ';
  case FAlignment of
    taLeftJustify: Al := DT_LEFT;
    taRightJustify: Al := DT_RIGHT;
    taCenter: Al := DT_CENTER;
  end;

  Al := Al or DT_CALCRECT  or DT_NOPREFIX;

  DC := GetDC(0);
  SelectObject(DC, fFont.Handle);
  DrawText(DC, PChar(txt), Length(txt), Rect, Al);
  ReleaseDC(0,DC);

  FBox.Right := Rect.Right;

end;

procedure TRtfdODLabel.Paint(width: integer);
begin
  FBox.Right := width;
  Paint;
end;

procedure TRtfdODLabel.Paint;
var
  Al: Integer;
  oldFont: TFont;
begin

    oldFont := fCanvas.Font;
    FCanvas.Font := fFont;
    if FTransparent then
      begin
        FCanvas.Brush .Style := bsClear;
        FCanvas.Brush.Color := TopColor[ Config.IsLimitedColors ];
      end
    else
      begin
        FCanvas.Brush.Style := bsClear;
        FCanvas.Brush.Color :=  clWhite;
      end;

    FCanvas.Pen.color := clBlack;

    case FAlignment of
      taLeftJustify: Al := DT_LEFT;
      taRightJustify: Al := DT_RIGHT;
      taCenter: Al := DT_CENTER;
    end;

    DrawText(FCanvas.Handle,PChar(fCaption),Length(fCaption),FBox,Al);

    fCanvas.Font := oldFont;
    FCanvas.Brush.Color :=  clWhite;

end;

procedure TRtfdODLabel.SetText(const Value: TCaption);
begin
  if Value <> FCaption then
    begin
    FCaption := Value;
    FOwner.Invalidate;
  end;

end;

function TRtfdODLabel.GetText: TCaption;
begin
  Result := FCaption;
end;

constructor TRtfdODLabel.Create(AOwner: TComponent; AEntity: TModelEntity;
  Tp: integer);
begin
  inherited Create(AOwner);
  fOwner := AOwner as TCustomControl;
  Self.Entity := AEntity;
  FTransparent := False;
  fFont:= TFont.Create;
  FBox.Top := Tp;
  FBox.Left := cDefaultLeft;
  FBox.Right := cDefaultRight - ClassShadowWidth;
  FBox.Bottom  := fBox.Top + cDefaultlblHeight;
  FCanvas := fOwner.Canvas;
end;

destructor TRtfdODLabel.Destroy;
begin
  fFont.Free;
  inherited;
end;

procedure TRtfdODLabel.Change(Sender: TModelEntity);
begin
  //stub fOwner.Invalidate;
end;

procedure TRtfdODLabel.AddChild(Sender: TModelEntity; NewChild: TModelEntity);
begin
  // stub
end;

procedure TRtfdODLabel.Remove(Sender: TModelEntity);
begin
   // stub
end;

procedure TRtfdODLabel.EntityChange(Sender: TModelEntity);
begin
  // fOwner.Invalidate;
end;

function TRtfdODLabel.WidthNeeded: integer;
begin
   SetWantedBounds;
   Result := FBox.Right;
end;

function TRtfdODLabel.Height: integer;
begin
  Result := FBox.Bottom - FBox.Top;
end;

end.


{
  Laz-Model
  Copyright (C) 2016 Peter Dyson. Initial Lazarus port

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
unit uLayoutConnector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, gvector,
  uLayoutConcepts, uError;

type

TAnchorPoint = record
  Pos: TPoint;
  Used: boolean;
  Dir: TRelationEnd;
  OutPath: TOrthoDir;
end;

TAnchorPoints = Array[apTopLeft..apLeftUpper] of TAnchorPoint;

{
  Essentially this class is a mathematical Orthagonal Vector (aka ray).
  It is just a point and a direction.
}
TLine = Class
protected
  FStartPoint: TPoint;
  FDir: TOrthoDir;
  function getX: integer;
  function getY: integer;
  function DebugString: string; virtual;
  procedure setX(AValue: integer);
  procedure setY(AValue: integer);
  procedure Shorten(AValue: integer);
  function GetDirectedLength(ALine: TLine): integer; overload;
  function GetDirectedLength(APoint: TPoint): integer; overload;
public
  constructor Create(StartPoint: TPoint; ADir: TOrthoDir); virtual;

  property x: integer read getX write setX;
  property y: integer read getY write setY;
  property Pos: TPoint read FStartPoint write FStartPoint;
  property Dir: TOrthoDir read FDir write FDir;
end;
PTLine = ^TLine;
{
  for future rounded corner style connections.
  Dir is startpoint tanget for a TArc.
}
TArc = Class(TLine)
private
  FRect: TRect;
  FRadius: integer;
  FAngDir: TAngularRotation;
  FSAngle: integer;
  FEAngle: integer;
  procedure InitialiseArc(EDir: TOrthoDir);
protected
  function DebugString: string; override;
public
  constructor Create(StartPoint: TPoint; SDir: TOrthoDir; Radius: integer; EDir: TOrthoDir);
  procedure Draw(ACanvas: TCanvas);
end;

TPathLines = specialize TVector<TLine>;

{
  A path of some description which joins two points.
  Being a vector this can be passed to alogritms to be modified before draw.
  Adding or removing TLines or its descendents.
}
TPath = class(TPathLines)
private
  FEndPoint: TLine;
  FRounded: boolean;
  FStyle: TPathStyle;
  function GetStartPoint: TLine;
public
  constructor Create(StartPoint, EndPoint: TLine; Style: TPathStyle);
  destructor Destroy; override;
  procedure ClearPath;
  procedure RoundCorners(Radius: Integer);
  procedure Draw(Canvas: TCanvas); virtual;
  property StartPoint: TLine read GetStartPoint;
  property EndPoint: TLine read FEndPoint write FEndPoint;
  property Style: TPathStyle read FStyle write FStyle;
end;

TDecoration = class(TObject)
  private
    FOrigin: TPoint;

  public
    constructor Create(AOrigin: TPoint); virtual;
    procedure Draw(ACanvas: TCanvas); virtual; abstract;

end;

TStringDecoration = class(TDecoration)
  FCaption: string;
   public
    constructor Create(Caption: string; Origin: TPoint);
    procedure Draw(ACanvas: TCanvas); override;
end;

TGraphicDecoration = class(TDecoration)
end;

TOrthoArrowDecoration = class(TGraphicDecoration)
private
  FDir: TOrthoDir;
public
  procedure Draw(ACanvas: TCanvas; AStyle: TArrowStyle);
  constructor Create(AOrigin: TPoint; ADir: TOrthoDir);
end;



TDecs = specialize TVector<TDecoration>;

TDecorations = class(TDecs)
public
   procedure Draw(ACanvas: TCanvas; AStyle: TDecorationDisplayStyle);
end;

TDecoratedConnection = class(TPath)
private
  FIsDirty: boolean;
  FOwner: TControl;
  FOwnerEnd: TControl;
  FTargetEnd: TControl;
  FWantMid: boolean;
  FOwnerDecoration: TGraphicDecoration;
  FTargetDecoration: TGraphicDecoration;
  // temp until associations working
  Arrow : TOrthoArrowDecoration;
  FArrowStyle: TArrowStyle;
  procedure SetOwnerDecoration(AValue: TGraphicDecoration);
  procedure SetTargetDecoration(AValue: TGraphicDecoration);

public
  Decorations: TDecorations;
  constructor Create(AOwner: TControl; AStartPoint, AEndPoint: TLine; AStyle: TPathStyle);
  destructor destroy; override;
  procedure DrawDecorated(ACanvas: TCanvas; AStyle: TDecorationDisplayStyle = ddsEndpointOnly);
  procedure Refresh;
  procedure SetEndControls(AOwnerEnd, ATargetEnd: TControl);
  property IsDirty: boolean read FIsDirty write FIsDirty;
  property WantMid: boolean read FWantMid write FWantMid;
  property OwnerEnd: TControl read FOwnerEnd;
  property TargetEnd: TControl read FTargetEnd;
  property OwnerDecoration: TGraphicDecoration write SetOwnerDecoration;
  property TargetDecoration :TGraphicDecoration write SetTargetDecoration;
  property ArrowStyle: TArrowStyle read FArrowStyle write FArrowStyle default asEmptyOpen;
end;


implementation

uses essConnectPanel;

{ TDecoration }

constructor TDecoration.Create(AOrigin: TPoint);
begin
  inherited Create;
  FOrigin := AOrigin;
end;


{ TDecorations }

procedure TDecorations.Draw(ACanvas: TCanvas; AStyle: TDecorationDisplayStyle);
var
   it: TVectorEnumerator;
begin

   if AStyle > ddsNone then
   begin
     it := Self.GetEnumerator;
     while Assigned(it.Current) do
     begin

        TDecoration(it.Current).Draw(ACanvas);
     end;
     it.Free;
   end;
end;

{ TArrowDecoration }

constructor TOrthoArrowDecoration.Create(AOrigin: TPoint; ADir: TOrthoDir);
begin
  inherited Create(AOrigin);
  FDir := ADir;
end;

procedure TOrthoArrowDecoration.Draw(ACanvas: TCanvas; AStyle: TArrowStyle);
var
   p1,p2: TPoint;
begin
  with ACanvas do
  begin
     if IsHorizontal[FDir] then
       begin
         p1.x := FOrigin.x + cArrowLength * oDir[FDir];
         p1.y := FOrigin.y + cArrowWidth;

         p2.x := FOrigin.x + cArrowLength * oDir[FDir] ;
         p2.y := FOrigin.y - cArrowWidth ;
       end
     else
       begin
         p1.x := FOrigin.x + cArrowWidth ;
         p1.y := FOrigin.y + cArrowLength * oDir[FDir];

         p2.x := FOrigin.x - cArrowWidth ;
         p2.y := FOrigin.y + cArrowLength * oDir[FDir];
       end;

    Pen.Width := 1;
    Pen.Style := psSolid;

   if AStyle = asEmptyClosed then
     Polygon([p2,FOrigin, p1])
   else
     PolyLine([p2,FOrigin,p1]);
  end;
end;

{ TDecoratedConnection }

procedure TDecoratedConnection.SetOwnerDecoration(AValue: TGraphicDecoration);
begin
   FOwnerDecoration := AValue;
end;

procedure TDecoratedConnection.SetTargetDecoration(AValue: TGraphicDecoration);
begin
   FTargetDecoration := AValue;
end;

constructor TDecoratedConnection.Create(AOwner: TControl; AStartPoint,
  AEndPoint: TLine; AStyle: TPathStyle);
begin
  inherited Create(AStartPoint, AEndPoint, AStyle);
  FOwner := AOwner;
  //temp
  Arrow := TOrthoArrowDecoration.Create(Origin,odLeft);
end;

destructor TDecoratedConnection.destroy;
begin
  //temp
  Arrow.Free;
  inherited destroy;
end;

procedure TDecoratedConnection.DrawDecorated(ACanvas: TCanvas;
  AStyle: TDecorationDisplayStyle);
begin
  // Call
  Self.Draw(ACanvas);

  // temp
  Arrow.FOrigin := EndPoint.Pos;
  Arrow.FDir := EndPoint.FDir ;
  Arrow.Draw(ACanvas, FArrowStyle);

//  Decorations.Draw(ACanvas, AStyle);
end;

procedure TDecoratedConnection.Refresh;
begin
  with  TessConnectPanel(FOwner).PathLayout do
  begin
    RefreshPath(self);
    CalculatePath(self);
  end;
end;

procedure TDecoratedConnection.SetEndControls(AOwnerEnd, ATargetEnd: TControl);
begin
  FOwnerEnd := AOwnerEnd;
  FTargetEnd := ATargetEnd;
end;

{ TStringDecoration }

constructor TStringDecoration.Create(Caption: string; Origin: TPoint);
begin
    inherited Create(Origin);
    FCaption := Caption;
end;

procedure TStringDecoration.Draw(ACanvas: TCanvas);
begin
  with ACanvas do
  begin

  end;
end;

{ TPath }

function TPath.GetStartPoint: TLine;
begin
  Result := TLine(Mutable[0]^);
end;


constructor TPath.Create(StartPoint, EndPoint: TLine; Style: TPathStyle);
begin
   inherited Create;
   FStyle := Style;
   FEndPoint := EndPoint;
   FRounded := False;
   Self.PushBack(StartPoint);
end;

destructor TPath.Destroy;
begin
  while not self.IsEmpty do
  begin
    TLine(Back).Free;
    self.PopBack;
  end;
  FreeAndNil(FEndPoint);
  inherited Destroy;
end;

procedure TPath.ClearPath;
begin
  while Size > 1 do
  begin
   TLine(Back).Free;
   PopBack;
  end;
  FRounded := False;
end;

procedure TPath.RoundCorners(Radius: Integer);
var
   itp,it,itn: Tline;
   len: integer;
   fRad: integer;  // Forced Radius
   dRad: integer;  // Drawn Radius
   arc: TArc;
   i: integer;
   p1: TPoint;
   Done: boolean;

   function WantRad: integer;
   begin
      if fRad > 0 then
        Result := fRad
      else
        Result := Radius;
   end;

begin
   if (not FRounded) and (Self.Size > 1) then
   begin
     i := 1;
     itp := Self.Items[i-1];
     it := Self.Items[i];
     {$IFDEF DEBUG_PATH_ROUNDING}
     ErrorHandler.Trace ('New Path Round');
     ErrorHandler.Trace ('0p: ' + itp.DebugString);
     ErrorHandler.Trace ('0: ' + it.DebugString);
     {$ENDIF}
     fRad := -1;
     // is first line long enough?
     len := itp.GetDirectedLength(it);
     if len < Radius then
        fRad := len div 2;

     done := false;

     while not Done do
     begin
       if (i + 1) < Self.Size then
         itn :=  Self.Items[i+1]
       else
         begin
           itn := Self.EndPoint;
           done := true;
         end;
       // Get Line length for it -> itn
       len := it.GetDirectedLength(itn);
       {$IFDEF DEBUG_PATH_ROUNDING}
       ErrorHandler.Trace('Pass: ' + IntToStr(i));
       ErrorHandler.Trace ('1p: ' + itp.DebugString);
       ErrorHandler.Trace ('1: ' + it.DebugString);
       ErrorHandler.Trace ('1n: ' + itn.DebugString);
       ErrorHandler.Trace('Len: ' + IntToStr(len));
       {$ENDIF}

       // Calculate actual radius plus forced next radius
       if len > WantRad + Radius then
         begin
           dRad := WantRad;
           fRad := -1;
         end
       else
         if len > fRad * 2 then
           begin
             dRad := len div 2;
             fRad := len div 2;
           end
         else
           begin
             dRad := fRad;
             fRad := fRad;
           end;

       // get start point for the arc
       p1 := OrthoOffsetPoint(it.Pos, OrthoOpposite[itp.Dir],dRad);
       // shorten the next line
       it.Shorten(dRad);
       // Insert the arc
       if (dRad > 2) then
         begin
           Arc := TArc.Create(p1, itp.Dir, dRad, it.Dir);
           Self.Insert(i,arc);
           inc(i);
         end
       else
         fRad :=1;
       {$IFDEF DEBUG_PATH_ROUNDING}
       ErrorHandler.Trace ('Arc: ' + Arc.DebugString);
       ErrorHandler.Trace ('Bp: ' + itp.DebugString);
       ErrorHandler.Trace ('B: ' + it.DebugString);
       ErrorHandler.Trace ('Bn: ' + itn.DebugString);
       {$ENDIF}
       inc(i);
       itp := Self.Items[i-1];
       it := Self.Items[i];
     end;
     FRounded := True;
   end;
 end;

{
 Default to simple drawing of lines with the supplied pathstyle as a default.
 For other path resolvers or even here update for rounded. Ortho resolvers
 can cope with just this simple algo.
}
procedure TPath.Draw(Canvas: TCanvas);
var
   it: TVectorEnumerator;
begin
  with Canvas do
  begin
    Pen.Style := CanvasLineStyle[FStyle];
    Pen.Width := CanvasPenWidth[FStyle];
    it := Self.GetEnumerator;
    // guaranteed to have at least one point from class creation
    MoveTo(TLine(it.Current).FStartPoint);
    while it.MoveNext do
    begin
      if (it.Current is TArc) then
        begin
          LineTo(TLine(it.Current).FStartPoint);
          TArc(it.Current).Draw(Canvas);
          it.MoveNext;
          MoveTo(TLine(it.Current).FStartPoint);
        end
      else
        LineTo(TLine(it.Current).FStartPoint);
    end;
    LineTo(FEndPoint.FStartPoint);
  end;
  it.Free;
end;

{ TArc }

procedure TArc.InitialiseArc(EDir: TOrthoDir);
begin
  case FDir of
    odLeft:
      if EDir = odUp then
        begin
           FSAngle := 270 * 16;
           FEAngle := -90 * 16;
           FAngDir := adClockwise;
           FRect.Left := FStartPoint.x - FRadius;
           FRect.Right := FStartPoint.x + FRadius;
           FRect.Bottom := FStartPoint.y;
           FRect.Top := FStartPoint.y - FRadius - FRadius;

        end
      else
        begin
          FSAngle := 90 * 16;
          FEAngle := 90 * 16;
          FAngDir := adAntiClockwise;
          FRect.Left := FStartPoint.x - FRadius;
          FRect.Right := FStartPoint.x + FRadius;
          FRect.Bottom := FStartPoint.y + FRadius + FRadius;
          FRect.Top := FStartPoint.y;
        end;
    odRight:
      if EDir = odUp then
        begin
          FSAngle := 270 * 16;
          FEAngle := 90 * 16 ;
           FAngDir := adAntiClockwise;
           FRect.Left := FStartPoint.x - FRadius;
           FRect.Right := FStartPoint.x + FRadius;
           FRect.Bottom := FStartPoint.y;
           FRect.Top := FStartPoint.y - FRadius - FRadius;
        end
      else
        begin
          FSAngle := 90 * 16;
          FEAngle := -90 * 16 ;
          FAngDir := adClockwise;
          FRect.Left := FStartPoint.x - FRadius;
          FRect.Right := FStartPoint.x + FRadius;
          FRect.Bottom := FStartPoint.y + FRadius + FRadius;
          FRect.Top := FStartPoint.y;
        end;
    odUp:
      if EDir = odLeft then
        begin
          FSAngle := 0;
          FEAngle := 90 * 16;
           FAngDir := adAntiClockwise;
           FRect.Left := FStartPoint.x - FRadius - FRadius;
           FRect.Right := FStartPoint.x;
           FRect.Bottom := FStartPoint.y + FRadius;
           FRect.Top := FStartPoint.y - FRadius;
        end
      else
        begin
          FSAngle := 180 * 16;
          FEAngle := -90 * 16;
          FAngDir := adClockwise;
          FRect.Left := FStartPoint.x;
          FRect.Right := FStartPoint.x + FRadius + FRadius;
          FRect.Bottom := FStartPoint.y + FRadius;
          FRect.Top := FStartPoint.y - FRadius;
        end;
    odDown:
      if EDir = odLeft then
        begin
           FSAngle := 0 * 16;
           FEAngle := -90 * 16;
           FAngDir := adClockwise;
           FRect.Left := FStartPoint.x - FRadius - FRadius;
           FRect.Right := FStartPoint.x;
           FRect.Bottom := FStartPoint.y + FRadius;
           FRect.Top := FStartPoint.y - FRadius;
        end
      else
        begin
          FSAngle := 180 * 16;
          FEAngle := 90 * 16;
          FAngDir := adAntiClockwise;
          FRect.Left := FStartPoint.x;
          FRect.Right := FStartPoint.x + FRadius + FRadius;
          FRect.Bottom := FStartPoint.y + FRadius;
          FRect.Top := FStartPoint.y - FRadius;
        end;
  end;

end;

function TArc.DebugString: string;
begin
  Result := Format('x: %d, y: %d, dir: %d, rad: %d',[x,y,integer(FDir), FRadius]);
end;

constructor TArc.Create(StartPoint: TPoint; SDir: TOrthoDir; Radius: integer;
  EDir: TOrthoDir);
begin
   inherited Create(StartPoint, SDir);
   FRadius := Radius;
   InitialiseArc(EDir);
end;

procedure TArc.Draw(ACanvas: TCanvas);
begin
  ACanvas.Arc(FRect.Left,FRect.Top,FRect.Right,FRect.Bottom, FSAngle, FEAngle);
end;

{ TLine }

function TLine.getX: integer;
begin
  Result := FStartPoint.x;
end;

function TLine.getY: integer;
begin
  Result := FStartPoint.y;
end;

function TLine.DebugString: string;
begin
  Result := Format('x: %d, y: %d, dir: %d',[x,y,integer(FDir)]);
end;

procedure TLine.setX(AValue: integer);
begin
   FStartPoint.x := AValue;
end;

procedure TLine.setY(AValue: integer);
begin
  FStartPoint.y := AValue;
end;

procedure TLine.Shorten(AValue: integer);
begin
  case FDir of
    odLeft: FStartPoint.x := FStartPoint.x - AValue;
    odRight: FStartPoint.x := FStartPoint.x + AValue;
    odUp: FStartPoint.y := FStartPoint.y - AValue;
    odDown: FStartPoint.y := FStartPoint.y + AValue;
  end;
end;

function TLine.GetDirectedLength(ALine: TLine): integer;
begin
  Result := GetDirectedLength(ALine.Pos);
end;

function TLine.GetDirectedLength(APoint: TPoint): integer;
begin
  if IsHorizontal[Self.Dir] then
    Result := Abs(FStartPoint.x - APoint.x)
  else
    Result := Abs(FStartPoint.y - APoint.y);
end;

constructor TLine.Create(StartPoint: TPoint; ADir: TOrthoDir);
begin
  inherited create;
  FStartPoint := StartPoint;
  FDir := ADir;
end;




end.


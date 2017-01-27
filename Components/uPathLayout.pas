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
unit uPathLayout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, gvector,
  uLayoutConcepts, uLayoutConnector, essConnectPanel;

const
  cPathMargin = 30;



type
// substitutable algos which may be called by their allocation in layout
TNudgeParent = procedure (Item, Parent: TObject; PreferedEnd: TRelationEnd ) of object;


 {
  Original // TODO move old logic into new TVectorPathLayout
 }
  TVectorPathLayout = class(TBasePathLayout)
    public
      function GetConnection(const AOwner: TControl; const rOwner: TControl; const rTarget: TControl;
                   WantMid: boolean): TDecoratedConnection;  override;
      procedure ArrangeAnchors; override;
      procedure CalculatePath(con: TDecoratedConnection); override;
      procedure RefreshPath(con: TDecoratedConnection); override;
      procedure DrawConnection(con: TDecoratedConnection; ACanvas: TCanvas); override;
  end;

 TOrthoPathLayout = class(TBasePathLayout)
 public
   function GetConnection(const AOwner: TControl; const rOwner: TControl; const rTarget: TControl;
                   WantMid: boolean): TDecoratedConnection;  override;
   procedure ArrangeAnchors; override;
   procedure CalculatePath(con: TDecoratedConnection); override;
   procedure RefreshPath(con: TDecoratedConnection); override;
   procedure DrawConnection(con: TDecoratedConnection; ACanvas: TCanvas); override;
 end;

 TRoundedPathLayout = class(TBasePathLayout)
 public
   function GetConnection(const AOwner: TControl; const rOwner: TControl; const rTarget: TControl;
                   WantMid: boolean): TDecoratedConnection; override;
   procedure ArrangeAnchors; override;
   procedure CalculatePath(con: TDecoratedConnection); override;
   procedure RefreshPath(con: TDecoratedConnection); override;
   procedure DrawConnection(con: TDecoratedConnection; ACanvas: TCanvas); override;
 end;

implementation

procedure SetAnchorPoints(const Rect: TRect; var Pts: TAnchorPoints);
var
  halfH, quatH, halfW, quatW: integer;
  rwid, rhigh: integer;
begin

   rwid := Rect.Right - Rect.Left;
   rhigh := Rect.Bottom - Rect.Top;

   halfW := (rwid) div 2;
   quatW := (rwid) div 4;

   halfH := (rhigh) div 2;
   quatH := (rhigh) div 4;

   Pts[apTopLeft].Pos.x := halfw - quatW;
   Pts[apTopLeft].Pos.y := 0;

   Pts[apTopMid].Pos.x := halfw;
   Pts[apTopMid].Pos.y := 0;

   Pts[apTopRight].Pos.x := halfw + quatW;
   Pts[apTopRight].Pos.y := 0;

   Pts[apRightUpper].Pos.x := rwid;
   Pts[apRightUpper].Pos.y := halfh - quatH;

   Pts[apRightMid].Pos.x := rwid;
   Pts[apRightMid].Pos.y := halfh;

   Pts[apRightLower].Pos.x := rwid;
   Pts[apRightLower].Pos.y := halfh + quatH;

   Pts[apBottomRight].Pos.x := halfw + quatW;
   Pts[apBottomRight].Pos.y := rhigh;

   Pts[apBottomMid].Pos.x := halfw;
   Pts[apBottomMid].Pos.y := rhigh;

   Pts[apBottomLeft].Pos.x := halfw - quatW;
   Pts[apBottomLeft].Pos.y := rhigh;

   Pts[apLeftLower].Pos.x := 0;
   Pts[apLeftLower].Pos.y := halfh + quatH;

   Pts[apLeftMid].Pos.x := 0;
   Pts[apLeftMid].Pos.y := halfh;

   Pts[apLeftUpper].Pos.x := 0;
   Pts[apLeftUpper].Pos.y := halfh - quatH;

end;

function GetRectPoint(rect: TRect; Pos: TAnchorPosition): TPoint;
var
  halfH, quatH, halfW, quatW: integer;
  rwid, rhigh: integer;
begin

   rwid := Rect.Right - Rect.Left;
   rhigh := Rect.Bottom - Rect.Top;

   halfW := (rwid) div 2;
   quatW := (rwid) div 4;

   halfH := (rhigh) div 2;
   quatH := (rhigh) div 4;

   case Pos of
   apTopLeft:
     begin
        Result.x := halfw - quatW + rect.Left;
        Result.y := rect.top;
     end;
   apTopMid:
     begin
       Result.x := halfw + rect.Left;
       Result.y := rect.top;
     end;
   apTopRight:
     begin
       Result.x := halfw + quatW + rect.Left;
       Result.y := rect.top;
     end;
   apRightUpper:
     begin
       Result.x := rect.right;
       result.y := halfh - quatH + rect.Top;
     end;
   apRightMid:
     begin
       Result.x := rect.right;
       result.y := halfh + rect.Top;
     end;
   apRightLower:
     begin
       Result.x := rect.right;
       result.y := halfh + quatH + rect.Top;
     end;
   apBottomRight:
     begin
       Result.x := halfw + quatW + rect.Left;
       Result.y := rect.Bottom;
     end;
   apBottomMid:
     begin
       Result.x := halfw + rect.Left;
       Result.y := rect.Bottom;
     end;
   apBottomLeft:
     begin
       Result.x := halfw - quatW + rect.Left;
       Result.y := rect.Bottom;
     end;
   apLeftLower:
     begin
       Result.x := rect.Left;
       result.y := halfh + quatH + rect.Top;
     end;
   apLeftMid:
     begin
       Result.x := rect.Left;
       result.y := halfh + rect.Top;
     end;
   apLeftUpper:
     begin
       Result.x := rect.Left;
       result.y := halfh - quatH + rect.Top;
     end;
   end;

end;

function CreateConnection(const AOwner: TControl; const rOwner: TControl; const rTarget: TControl;
  AWantMid: boolean): TDecoratedConnection;
var
  quad: TOctantPosition;
  anchors: TLinkAnchors;
  po,pt: TPoint;
  Path: TPath;
  p,p1: TPoint;
  ls,le: TLine;
  dir: TOrthoDir;
 begin
    // owner midpoint
    po.x := (rOwner.BoundsRect.Right + rOwner.BoundsRect.Left) div 2;
    po.y := (rOwner.BoundsRect.Bottom + rOwner.BoundsRect.Top) div 2;
    // target midpoint
    pt.x := (rTarget.BoundsRect.Right + rTarget.BoundsRect.Left) div 2;
    pt.y := (rTarget.BoundsRect.Bottom + rTarget.BoundsRect.Top) div 2;

     quad := GetQuadrant(rOwner.BoundsRect,rTarget.BoundsRect, po, pt, cPathMargin);
     anchors := OrthoConnectTo(AWantMid,quad, po, pt);
     p := GetRectPoint(rOwner.BoundsRect, anchors.Owner);
     p1 := GetRectPoint(rTarget.BoundsRect, anchors.Target);
     dir := AnchorOrtho[anchors.Owner];
     ls := TLine.Create(p, dir);
     dir := AnchorOrtho[anchors.Target];
     le := TLine.Create(p1, dir);
     Result := TDecoratedConnection.Create (AOwner, ls, le, psThin);
     Result.WantMid := AWantMid;
     Result.SetEndControls(rOwner,rTarget);

end;

function CreateVectorConnection(const AOwner: TControl; const rOwner: TControl; const rTarget: TControl;
  AWantMid: boolean): TDecoratedConnection;
var
    ls,le: TLine;
begin
   ls := TLine.Create(Origin, odLeft);
   le := TLine.Create(Origin, odLeft);
   Result := TDecoratedConnection.Create (AOwner, ls, le, psThin);
   Result.SetEndControls(rOwner,rTarget);
end;

// TODO make some of these 'plugable' into a PathLayout??
// ATM this is OrthoSpecific
procedure UpdateConnection(con: TDecoratedConnection);
var
  quad: TOctantPosition;
  anchors: TLinkAnchors;
  po,pt: TPoint;
  Path: TPath;
  p,p1: TPoint;
  ls,le: TLine;
  dir: TOrthoDir;
begin
   // owner midpoint
   with con.OwnerEnd do
   begin
     po.x := (BoundsRect.Right + BoundsRect.Left) div 2;
     po.y := (BoundsRect.Bottom + BoundsRect.Top) div 2;
   end;
   // target midpoint
   with con.TargetEnd do
   begin
     pt.x := (BoundsRect.Right + BoundsRect.Left) div 2;
     pt.y := (BoundsRect.Bottom + BoundsRect.Top) div 2;
   end;

    quad := GetQuadrant(con.OwnerEnd.BoundsRect,con.TargetEnd.BoundsRect, po, pt, cPathMargin);
    anchors := OrthoConnectTo(con.WantMid ,quad, po, pt);
    p := GetRectPoint(con.OwnerEnd.BoundsRect, anchors.Owner);
    con.StartPoint.x := p.x;
    con.StartPoint.y := p.y;
    con.StartPoint.Dir := AnchorOrtho[anchors.Owner];

    p1 := GetRectPoint(con.TargetEnd.BoundsRect, anchors.Target);
    con.EndPoint.x := p1.x;
    con.EndPoint.y := p1.y;
    con.EndPoint.Dir := AnchorOrtho[anchors.Target];

end;

procedure UpdateVectorConnection(con: TDecoratedConnection);
var
  p1,p2: TPoint;
begin
   CalcShortest(con.OwnerEnd.BoundsRect, con.TargetEnd.BoundsRect,p1,p2);
   con.StartPoint.Pos := p1;
   con.EndPoint.Pos := p2;
end;

function IsOrthoInline(Line1, Line2: Tline): boolean;
begin
   if OrthoAxisForDir[Line1.Dir] = oaHorizontal then
     Result := (Line1.Pos.y = Line2.Pos.y)
   else
     Result := (Line1.Pos.x = Line2.Pos.x);
end;

{
  Given a connection with two directional endpoints
  Generate a simple corner if the endpoints are OrthoAxisOpposite
    i.e. one Horizontal and one vertical
  If both points are in the same axis then generate a 'kink' line if
  the points are not inline in that axis.
}
procedure ConnectEndPointsSimpleOrtho(conn: TDecoratedConnection);
var
  ToEndLine, MidLine: TLine;
  mid: integer;
begin
   MidLine := nil;
   With conn do
   begin
     ToEndLine := TLine.Create(Origin, OrthoOpposite[EndPoint.Dir]);
     if StartPoint.Dir = OrthoOpposite[EndPoint.Dir] then
       begin
       if IsOrthoInline(StartPoint, EndPoint) then
         FreeAndNil(ToEndLine)
       else
         begin
           MidLine := TLine.Create(Origin, odLeft);
           if IsVertical[StartPoint.dir]then
             begin
                MidLine.Dir := GetHorizontalDir(StartPoint.x, EndPoint.x);
                mid := (StartPoint.y + EndPoint.y) div 2;
                MidLine.x := StartPoint.x;
                MidLine.y := mid;
                ToEndLine.x := EndPoint.x;
                ToEndLine.y := mid;
             end
           else
             begin
               MidLine.Dir := GetVerticalDir(StartPoint.y, EndPoint.y);
               mid := (StartPoint.x + EndPoint.x) div 2;
               MidLine.x := mid;
               MidLine.y := StartPoint.y;
               ToEndLine.x := mid;
               ToEndLine.y := EndPoint.y;
             end;
           end;
       end
     else
       //just join the dot
       begin
         if OrthoAxisForDir[StartPoint.Dir] = oaHorizontal then
           begin
               ToEndLine.x := EndPoint.x;
               ToEndLine.y := StartPoint.y;
           end
         else
           begin
             ToEndLine.x := StartPoint.x;
             ToEndLine.y := EndPoint.y;
           end;
       end;
   end;
   // add the lines to the connection
   if Assigned(Midline) then
       conn.PushBack(MidLine);
   if Assigned(ToEndLine) then
     conn.PushBack(ToEndLine);

end;

const cRoundRadius = 25;


{ TOrthoPathLayout }

function TOrthoPathLayout.GetConnection(const AOwner: TControl;
  const rOwner: TControl; const rTarget: TControl; WantMid: boolean
  ): TDecoratedConnection;
begin
   Result := CreateConnection(AOwner, rOwner, rTarget,WantMid);
   ConnectEndPointsSimpleOrtho(Result);
end;

procedure TOrthoPathLayout.ArrangeAnchors;
begin

end;

procedure TOrthoPathLayout.CalculatePath(con: TDecoratedConnection);
begin
end;

procedure TOrthoPathLayout.RefreshPath(con: TDecoratedConnection);
begin
   con.ClearPath;
   UpdateConnection(con);
   ConnectEndPointsSimpleOrtho(con);

end;

procedure TOrthoPathLayout.DrawConnection(con: TDecoratedConnection;
  ACanvas: TCanvas);
begin
    con.DrawDecorated(ACanvas);
end;


{ TRoundedPathLayout }

function TRoundedPathLayout.GetConnection(const AOwner: TControl;
  const rOwner: TControl; const rTarget: TControl; WantMid: boolean
  ): TDecoratedConnection;
begin
   Result := CreateConnection(AOwner, rOwner, rTarget,WantMid);
   ConnectEndPointsSimpleOrtho(Result);
end;

procedure TRoundedPathLayout.ArrangeAnchors;
begin

end;

procedure TRoundedPathLayout.CalculatePath(con: TDecoratedConnection);
begin

end;

procedure TRoundedPathLayout.RefreshPath(con: TDecoratedConnection);
begin
   con.ClearPath;
   UpdateConnection(con);
   ConnectEndPointsSimpleOrtho(con);
   Con.RoundCorners(cRoundRadius);
end;

procedure TRoundedPathLayout.DrawConnection(con: TDecoratedConnection;
  ACanvas: TCanvas);
begin
   con.DrawDecorated(ACanvas);
end;


{ TVectorPathLayout }

function TVectorPathLayout.GetConnection(const AOwner: TControl;
  const rOwner: TControl; const rTarget: TControl; WantMid: boolean
  ): TDecoratedConnection;
begin
   Result := CreateVectorConnection(AOwner, rOwner, rTarget,WantMid);
end;

procedure TVectorPathLayout.ArrangeAnchors;
begin

end;

procedure TVectorPathLayout.CalculatePath(con: TDecoratedConnection);
begin

end;

procedure TVectorPathLayout.RefreshPath(con: TDecoratedConnection);
begin
  UpdateVectorConnection(con);
end;

procedure TVectorPathLayout.DrawConnection(con: TDecoratedConnection;
  ACanvas: TCanvas);
begin
    con.ClearPath; // required for style switch
    TPath(con).Draw(ACanvas);  // only draw the path
    VectorDrawArrow(ACanvas, con.StartPoint.pos, con.EndPoint.Pos, con.ArrowStyle);
end;





end.

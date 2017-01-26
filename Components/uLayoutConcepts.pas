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
unit uLayoutConcepts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Math;

type

TOrthoAxis  = (oaVertical, oaHorizontal);

TOrthoDir = (odLeft, odRight, odUp, odDown);

TVerticalAlign = (vaSame, vaLower, vaHigher);

THorizontalAlign = (haSame, haLeft, haRight);

TOctantPosition = (opNorth, opNorthEast, opEast, opSouthEast,
                   opSouth, opSouthWest, opWest, opNorthWest);

TRelationEnd = (reOwner, reTarget);

TAnchorPosition = ( apTopLeft, apTopMid, apTopRight,
                    apRightUpper, apRightMid, apRightLower,
                    apBottomRight, apBottomMid, apBottomLeft,
                    apLeftLower, apLeftMid, apLeftUpper,
                    apMemberLeft, apMemberRight
                  );

TLinkAnchors = record
  Owner: TAnchorPosition;
  Target: TAnchorPosition;
end;




TStringDecorationLinePos = (sdlpStart, sdlpMid, sdlpEnd);

TStringDecorationPreferedPos = (sdppPrefer1, sdppPrefer2);

TStringPathDecoration = record
  Value: string;
  LinePos: TStringDecorationLinePos;
  Prefer: TStringDecorationPreferedPos;
end;


// Hints for Decoration placement.
TDecorationPreferedPosition = (dppWantStart, dppWantEnd, dppWantMid);

TDecorationPreferedOrientaion = (dpoDontCare, dpoPreferHorizontal, dpoPreferVertical);

// Doe we display LinesOnly, Lines + EndPoints(eg arrows) or Everything.
TDecorationDisplayStyle = (ddsNone, ddsEndpointOnly, ddsAll);

// at the moment these mirror old code until Associations are done
// These WILL change.
TPathStyle = (psThin, psNormal, psThinDash);

TArrowStyle = (asEmptyOpen,asEmptyClosed);



TPathLayoutStyle =(plsOrtho, plsVector, plsRoundedOrtho);


const
// Concepts which can be represented as lookups

AnchorPositionOpposite: Array[apTopLeft..apMemberRight] of TAnchorPosition =
                (apBottomRight,  apBottomMid, apBottomRight,
                 apLeftLower, apLeftMid, apLeftUpper,
                 apTopLeft, apTopMid, apTopRight,
                 apRightUpper, apRightMid, apRightLower,
                 apMemberRight, apMemberLeft
                );

AnchorOrtho: Array[apTopLeft..apMemberRight] of TOrthoDir =
                  (odUp, odUp, odUp,
                   odRight, odRight, odRight,
                   odDown, odDown, odDown,
                   odLeft, odLeft, odLeft,
                   odLeft, odRight
                  );

OrthoOpposite: Array[odLeft..odDown] of TOrthoDir =
                     (odRight, odLeft, odDown, odUp);

OrthoAxisForDir: Array[odLeft..odDown] of TOrthoAxis =
                     (oaHorizontal,oaHorizontal,oaVertical,oaVertical);


IsVertical: Array [odLeft..odDown] of boolean =
                     (False, False, True, True);

IsHorizontal: Array [odLeft..odDown] of boolean =
                     (True, True, False, False);

{
 Flip direction for Horizontal and vertical
 Reduces case by 2 Less code when lots of points
 without using Matrix transforms.
}
oDir: Array[odLeft..odDown] of integer =
                     (-1, 1, -1, 1);


axisDir: Array[oaVertical..oaHorizontal] of integer = (1,-1);

Origin: TPoint = (x:0;y:0;);

{
 Other Consts.
}

CanvasLineStyle: Array[psThin..psThinDash] of TPenStyle =
              (psSolid,psSolid,psDash);

CanvasPenWidth: Array[psThin..psThinDash] of integer = (1,2,1);

// TODO Arrow Size
cArrowLength: integer = 10;
cArrowWidth:  integer = 6;


// move any Alogrithms which utilise the above here if it makes sense to
// share.
function OrthoConnectTo(UseMid: boolean; OtherOct: TOctantPosition;
                     const po: TPoint; const pt: Tpoint): TLinkAnchors;

function GetQuadrant(const rOwner: TRect; const rTarget: TRect;
                     const po: TPoint; const pt: Tpoint; Margin: integer): TOctantPosition;

function GetHorizontalDir(FromX, ToX: integer): TOrthoDir; inline;
function GetVerticalDir(FromY, ToY: integer): TOrthoDir; inline;

procedure VectorDrawArrow(Canvas:TCanvas; pfrom, pto: TPoint; ArrowStyle : TArrowStyle);
function PointToAngle(R : TRect; P : TPoint) : double;
function AngleToPoint(R : TRect; Angle : double) : TPoint;
procedure CalcShortest(FromRect,ToRect : TRect; var P1,P2 : TPoint);

implementation

{
  given a target region, a preference for MidPoint for Inheritance,
  centerpoints for the rects.
  Calculate the section to connect to both owner and target.
  Looks complicated but really easy to debug visually.
}
function OrthoConnectTo(UseMid: boolean; OtherOct: TOctantPosition;
                     const po: TPoint; const pt: Tpoint): TLinkAnchors;
begin

   case OtherOct of
   opNorth:
     If UseMid then
       begin
         Result.Owner := apTopMid;
         Result.Target := apBottomMid;
       end
     else
       begin
         if po.x < pt.x then
           begin
             Result.Owner := apTopRight;
             Result.Target := apBottomLeft;
           end
         else
           begin
             Result.Owner := apTopLeft;
             Result.Target := apBottomRight;
           end;
       end;
   opNorthEast:
     If UseMid then
       begin
         Result.Owner := apTopMid;
         Result.Target := apBottomMid;
       end
     else
       begin
         Result.Owner := apRightUpper;
         Result.Target := apBottomLeft;
       end;
   opEast:
     If UseMid then
       begin
         Result.Owner := apRightMid;
         Result.Target := apLeftMid;
       end
     else
       begin
         if po.y > pt.y then
           begin
             Result.Owner := apRightUpper;
             Result.Target := apLeftLower;
           end
         else
           begin
             Result.Owner := apRightLower;
             Result.Target := apLeftUpper;
           end;
       end;
   opSouthEast:
     If UseMid then
       begin
         Result.Owner := apRightMid;
         Result.Target := apLeftMid;
       end
     else
       begin
         Result.Owner := apRightLower;
         Result.Target := apTopLeft;
       end;
   opSouth: // should not originate links from south mid as this is a reserved in:
     begin
      if po.x < pt.x then
       begin
         Result.Owner := apBottomRight;
         Result.Target := apTopLeft;
       end
      else
       begin
         Result.Owner := apBottomLeft;
         Result.Target := apTopRight;
       end;
     end;
   opSouthWest:
     If UseMid then
       begin
         Result.Owner := apLeftMid;
         Result.Target := apRightMid;
       end
     else
       begin
         Result.Owner := apLeftLower;
         Result.Target := apTopRight;
       end;
   opWest:
     If UseMid then
       begin
         Result.Owner := apLeftMid;
         Result.Target := apRightMid;
       end
     else
       begin
         if po.y > pt.y then
           begin
             Result.Owner := apLeftUpper;
             Result.Target := apRightLower;
           end
         else
           begin
             Result.Owner := apLeftLower;
             Result.Target := apRightUpper;
           end;
       end;
   opNorthWest:
     If UseMid then
       begin
         Result.Owner := apTopMid;
         Result.Target := apBottomMid;
       end
     else
       begin
         Result.Owner := apLeftUpper;
         Result.Target := apBottomRight;
       end;
   end;
end;

function GetQuadrant(const rOwner: TRect; const rTarget: TRect;
                     const po: TPoint; const pt: Tpoint; Margin: integer): TOctantPosition;
begin

  if rOwner.Top > (rTarget.Bottom  + Margin) then   // Nx
    begin
      if rOwner.Left > (rTarget.Right + Margin) then
        Result := opNorthWest
      else if rOwner.Right + Margin < rTarget.Left then
        Result := opNorthEast
      else
        Result := opNorth;
    end
  else if (rOwner.Bottom  + Margin) < rTarget.Top then  // Sx
    begin
      if rOwner.Left > rTarget.Right + Margin then
        Result := opSouthWest
      else if (rOwner.Right + Margin) < rTarget.Left then
        Result := opSouthEast
      else
        Result := opSouth;
      end
  else                             // EW
    if po.x > pt.x then    // use rect center points for this ensures all areas are classified.
      Result := opWest
    else
      Result := opEast;

end;


function GetHorizontalDir(FromX, ToX: integer): TOrthoDir;
begin
  If FromX < ToX then
    Result := odRight
  else
    Result := odLeft;
end;

function GetVerticalDir(FromY, ToY: integer): TOrthoDir;
begin
  If FromY < ToY then
    Result := odDown
  else
    Result := odUp;
end;

function PointToAngle(R : TRect; P : TPoint) : double;
var
  Px,Py : integer;
begin
  Px := p.x - ( r.left + (r.Right-r.left) div 2 );
  Py := p.y - ( r.top + (R.Bottom-R.Top) div 2 );
  Result := Arctan2(py* (r.Right-r.left) , px* (R.Bottom-R.Top) );
end;

function AngleToPoint(R : TRect; Angle : double) : TPoint;
var
  Si,Co,E : double;
  X,Y : integer;

  function Range(Min,Max,Value : integer) : integer;
  begin
    if (value < min) then
      value := min;
    if (value > max) then
      value := max;
    Result := Value;
  end;

begin
  Si := Sin(Angle);
  Co := Cos(Angle);
  E := 0.001;
  X:=0;
  Y:=0;
  if Abs(Si)>E then
  begin
    X := Round((1.0 + Co/Abs(si)) / 2.0 * (r.Right-r.left));
    X := Range(0, (r.Right-r.left), x);
  end
  else if Co>=0.0 then
    X := (r.Right-r.left);

  if Abs(Co)>e then
  begin
    Y := Round((1.0 + Si/Abs(co))/2.0 * (R.Bottom-R.Top));
    Y := range(0, (R.Bottom-R.Top), y);
  end else if Si>=0 then
    Y := (R.Bottom-R.Top);
  Result := Point(R.Left + X,R.Top + Y);
end;

procedure CalcShortest(FromRect,ToRect : TRect; var P1,P2 : TPoint);
var
  Temp : TPoint;
begin
  Temp := Point( FromRect.Left + (FromRect.Right-FromRect.Left)div 2, FromRect.Top + (FromRect.Bottom-FromRect.Top)div 2);
  P2 := AngleToPoint( ToRect , PointToAngle(ToRect,Temp) );

  Temp := Point( ToRect.Left + (ToRect.Right-ToRect.Left)div 2, ToRect.Top + (ToRect.Bottom-ToRect.Top)div 2);
  P1 := AngleToPoint( FromRect , PointToAngle(FromRect,Temp) );
end;

procedure VectorDrawArrow(Canvas:TCanvas; pfrom, pto: TPoint; ArrowStyle : TArrowStyle);
  var
    HeadLength: Integer;
    x1,x2: Integer;
    y1,y2: Integer;
    xbase: Integer;
    xLineDelta: Integer;
    xLineUnitDelta: Double;
    xNormalDelta: Integer;
    xNormalUnitDelta: Double;
    ybase: Integer;
    yLineDelta: Integer;
    yLineUnitDelta: Double;
    yNormalDelta: Integer;
    yNormalUnitDelta: Double;
    oldPW: Integer;
    oldPS: TPenStyle;
    Tmp1 : double;
begin
    x1 := pfrom.x;  // first point
    y1 := pfrom.y;

    x2 := pto.x;  // second point with arrow
    y2 := pto.y;

    Canvas.MoveTo(x1,y1);
    Canvas.LineTo(x2,y2);

    xLineDelta := x2 - x1;
    yLineDelta := y2 - y1;

    if (xLineDelta=0) and (yLineDelta=0) then exit; // Line is 0 length
    if (abs(xLineDelta)>20000) or (abs(yLineDelta)>20000) then exit; // Line is too long

    Tmp1 := SQRT( SQR(xLineDelta) + SQR(yLineDelta) );
    if Tmp1=0 then
      xLineUnitDelta := 0
    else
      xLineUnitDelta := xLineDelta / Tmp1;

    Tmp1 := SQRt( SQR(xLineDelta) + SQR(yLineDelta) );
    if Tmp1=0 then
      yLineUnitDelta := 0
    else
      yLineUnitDelta := yLineDelta / Tmp1;

    // (xBase,yBase) is where arrow line is perpendicular to base of triangle.
    HeadLength := 10; // pixels
    xBase := x2 - Round(cArrowLength * xLineUnitDelta);
    yBase := y2 - Round(cArrowWidth * yLineUnitDelta);

    xNormalDelta :=  yLineDelta;
    yNormalDelta := -xLineDelta;
    xNormalUnitDelta := xNormalDelta / Sqrt( Sqr(xNormalDelta) + Sqr(yNormalDelta) );
    yNormalUnitDelta := yNormalDelta / Sqrt( Sqr(xNormalDelta) + Sqr(yNormalDelta) );

    oldPW := Canvas.Pen.Width;
    oldPS := Canvas.Pen.Style;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;

    // Draw the arrow tip
    case ArrowStyle of
      asEmptyClosed :
        Canvas.Polygon([Point(x2,y2),
          Point(xBase + ROUND(cArrowLength*xNormalUnitDelta),
                yBase + ROUND(cArrowWidth*yNormalUnitDelta)),
          Point(xBase - ROUND(cArrowLength*xNormalUnitDelta),
                yBase - ROUND(cArrowWidth*yNormalUnitDelta)) ]);
      asEmptyOpen :
        Canvas.Polyline([Point(xBase + ROUND(cArrowLength*xNormalUnitDelta),
               yBase + ROUND(cArrowWidth*yNormalUnitDelta)),
               Point(x2,y2),
         Point(xBase - ROUND(cArrowLength*xNormalUnitDelta),
               yBase - ROUND(cArrowWidth*yNormalUnitDelta)) ]);
    end;

    Canvas.Pen.Width := oldPW;
    Canvas.Pen.Style := oldPS;
end;

end.


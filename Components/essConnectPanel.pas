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

unit essConnectPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, Math,
  LCLIntf, LCLType, Messages, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  uViewIntegrator, uRtfdComponents, uLayoutConcepts, uLayoutConnector;

type
  // Available linestyles
  TessConnectionStyle = (csThin, csNormal, csThinDash);
  //Different kinds of arrowheads
  TessConnectionArrowStyle = TArrowStyle;

  {
    Specifies a connection between two managed objects.
  }
  TConnection = class
  public
    FFrom, FTo: TControl;
    FConnectStyle: TessConnectionStyle;
    ArrowStyle : TessConnectionArrowStyle;
  end;

  TBasePathLayout = class;
  {
    Wrapper around a control managed by essConnectPanel
  }
  TManagedObject = class
  private
    FSelected: Boolean;
    procedure SetSelected(const Value: Boolean);
  private
    FControl: TControl;
    // Old eventhandlers
    FOnMouseDown :TMouseEvent;
    FOnMouseMove :TMouseMoveEvent;
    FOnMouseUp :TMouseEvent;
    FOnClick :TNotifyEvent;
    FOnDblClick :TNotifyEvent;
    property Selected: Boolean read FSelected write SetSelected;
  public
    destructor Destroy; override;
  end;

  {
    Component that manages a list of contained controls that can be connected with
    somekind of line and allows the user to move it around and gives the containd
    control grabhandles when selected.

    Further it manages the layout of the contained controls.
  }

  { TessConnectPanel }

  TessConnectPanel = class(TCustomPanel)
  private
    FIsModified, FIsMoving, FIsRectSelecting, FSelectedOnly: Boolean;
    FMemMousePos: TPoint;
    FSelectRect: TRect;
    FBackBitmap: TBitmap;
    TempHidden : TObjectList;
    ClientDragging: Boolean;
    FNeedCheckSize: Boolean;
    FPathLayout: TBasePathLayout;
    FPathStyle :TPathLayoutStyle;
    procedure SetPathStyle(AValue:TPathLayoutStyle);
    procedure SetSelectedOnly(const Value : boolean);
  protected
    FManagedObjects: TList;
    FConnections: TObjectList;

    procedure CreateParams(var Params: TCreateParams); override;

    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;

    function FindManagedControl( AControl: TControl ): TManagedObject;
    procedure SelectObjectsInRect(SelRect: TRect);

    procedure OnManagedObjectMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnManagedObjectMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OnManagedObjectMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnManagedObjectClick(Sender: TObject);
    procedure OnManagedObjectDblClick(Sender: TObject);

    procedure ChildMouseDrag(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure Paint; override;
  public
    OnContentChanged : TNotifyEvent;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Add a control to the managed list
    function AddManagedObject(AObject: TControl): TControl;
    // Return the first of the selected controls if any.
    function GetFirstSelected : TControl;

    // Returns a objectlist containing the selected controls.
    // The list should be freed by the caller.
    function GetSelectedControls : TObjectList;


    // Returns a list containing all the managed controls.
    // The list should be freed by the caller.
    function GetManagedObjects: TList;


    // Returns a list with all interobject connections.
    // The list should be freed by the caller.
    function GetConnections : TList;

    // Add a connection from Src to Dst with the supplied style
    function ConnectObjects(Src, Dst: TControl;
      AStyle:TessConnectionStyle = csNormal;
      Arrow : TessConnectionArrowStyle = asEmptyClosed): Boolean;

    // Free all managed objects and the managed controls.
    procedure ClearManagedObjects;

    // Unselect all selected objects
    procedure ClearSelection;

    procedure SetFocus; override;

    procedure RecalcSize;

    property PathLayout: TBasePathLayout read FPathLayout;

    property IsModified: Boolean read FIsModified write FIsModified;

    // Bitmap to be used as background
    property BackBitmap : TBitmap read FBackBitmap write FBackBitmap;

    //Only draw selected
    property SelectedOnly : boolean read FSelectedOnly write SetSelectedOnly;

    property PathStyle: TPathLayoutStyle read FPathStyle write SetPathStyle;
  published
    property Align;
    property Alignment;
    property Anchors;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property FullRepaint;
    property ParentBiDiMode;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color default clWhite;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;


  TBasePathLayout = class
  private
    FOwner: TObject;
  public
    constructor Create(AOwner: TessConnectPanel);
    // intermediate which keeps program working.
    function GetConnection(const AOwner: TControl; const rOwner: TControl; const rTarget: TControl;
                   WantMid: boolean): TDecoratedConnection; virtual; abstract;
    // Will be final version
//     function GetBaseAnchors(A: TAssociation): TDecoratedConnection; overload; virtual; abstract;
    // stage 2 rearrange anchors according to space available
    procedure ArrangeAnchors; virtual; abstract;
    // stage 3 generate paths between finalised points.
    procedure CalculatePath(con: TDecoratedConnection); virtual; abstract;
    procedure RefreshPath(con: TDecoratedConnection); virtual; abstract;
    procedure DrawConnection(con: TDecoratedConnection; ACanvas: TCanvas); virtual; abstract;
end;

procedure Register;

implementation

uses uPathLayout;

type
  TCrackControl = class(TControl) end;

procedure Register;
begin
  RegisterComponents('Eldean', [TessConnectPanel]);
end;


constructor TBasePathLayout.Create(AOwner: TessConnectPanel);
begin
  FOwner := AOwner;
end;

{ TessConnectPanel }
function TessConnectPanel.AddManagedObject(AObject: TControl): TControl;
var
  crkObj : TCrackControl;
  newObj: TManagedObject;
begin
  Result := nil;
  if (AObject.Left + AObject.Width) > Width then Width := Max(Width,AObject.Left + AObject.Width + 50);
  if (AObject.Top + AObject.Height) > Height then Height := Max(Height,AObject.Top + AObject.Height + 50);

  AObject.Parent := Self;
  AObject.Visible := True;
  if FindManagedControl(AObject) = nil then
  begin
    newObj := TManagedObject.Create;
    newObj.FControl := AObject;
    FManagedObjects.Add(newObj);

    crkObj := TCrackControl(AObject);
    newObj.FOnMouseDown := crkObj.OnMouseDown;
    newObj.FOnMouseMove := crkObj.OnMouseMove;
    newObj.FOnMouseUp := crkObj.OnMouseUp;
    newObj.FOnClick := crkObj.OnClick;
    newObj.FOnDblClick := crkObj.OnDblClick;

    crkObj.OnMouseDown := @OnManagedObjectMouseDown;
    crkObj.OnMouseMove := @OnManagedObjectMouseMove;
    crkObj.OnMouseUp := @OnManagedObjectMouseUp;
    crkObj.OnClick := @OnManagedObjectClick;
    crkObj.OnDblClick := @OnManagedObjectDblClick;
    Result := AObject;
  end;
end;

procedure TessConnectPanel.ClearManagedObjects;
var
  i: Integer;
begin
  FConnections.Clear;
  for i:=0 to FManagedObjects.Count -1 do
    TManagedObject(FManagedObjects[i]).Free;
  FManagedObjects.Clear;
  SetBounds(0,0,0,0);
  FIsModified := False;
end;

procedure TessConnectPanel.ClearSelection;
var
  i: Integer;
begin
  for i:=0 to FManagedObjects.Count -1 do
    TManagedObject(FManagedObjects[i]).Selected := False;
end;

procedure TessConnectPanel.Click;
begin
  inherited;
  ClearSelection;
end;

function TessConnectPanel.ConnectObjects(Src, Dst: TControl;
  AStyle: TessConnectionStyle; Arrow : TessConnectionArrowStyle): Boolean;
var
  conn: TConnection;
  con: TDecoratedConnection;
begin

   con := FPathLayout.GetConnection(self, src, dst, (arrow = asEmptyClosed));
   con.IsDirty := True;
   con.Style := TPathStyle(ord(AStyle));
   con.ArrowStyle := TArrowStyle(ord( Arrow));
   FConnections.Add(con);

end;

constructor TessConnectPanel.Create(AOwner: TComponent);
begin
  inherited;
  FManagedObjects := TList.Create;
  FConnections := TObjectList.Create(True);
  Color := clWhite;
  TempHidden := TObjectList.Create(False);
  UseDockManager := True;
  FPathLayout := TOrthoPathLayout.Create(Self);
end;

procedure TessConnectPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
//  Params.Style := Params.Style and (not WS_CLIPCHILDREN);
end;



destructor TessConnectPanel.Destroy;
begin
  FreeAndNil(TempHidden);
  if Assigned(FManagedObjects) then
    FreeAndNil(FManagedObjects);
  if Assigned(FConnections) then
    FreeAndNil(FConnections);
  if Assigned(FPathLayout) then
    FreeAndNil(FPathLayout);
  inherited;
end;

function TessConnectPanel.FindManagedControl( AControl: TControl): TManagedObject;
var
  i: Integer;
  curr: TManagedObject;
begin
  Result := nil;
  for i:=0 to FManagedObjects.Count -1 do
  begin
    curr := TManagedObject(FManagedObjects[i]);
    if curr.FControl = AControl then
    begin
      Result := curr;
      exit;
    end;
  end;
end;

function TessConnectPanel.GetConnections: TList;
var
  i: Integer;
begin
  Result := TList.Create;
  for i := 0 to FConnections.Count-1 do
    Result.Add(FConnections[I]);
end;

function TessConnectPanel.GetManagedObjects: TList;
var
  i: Integer;
begin
  Result := TList.Create;
  for i := 0 to FManagedObjects.Count-1 do
    Result.Add(TManagedObject(FManagedObjects[i]).FControl);
end;

function TessConnectPanel.GetFirstSelected: TControl;
var
  Tmp : TObjectList;
begin
  Result := nil;
  Tmp := GetSelectedControls;
  if Tmp.Count>0 then
    Result := Tmp[0] as TControl;
  Tmp.Free;
end;

function TessConnectPanel.GetSelectedControls: TObjectList;
var
  I : Integer;
begin
  Result := TObjectList.Create(False);
  for I := 0 to FManagedObjects.Count-1 do
    if TManagedObject(FManagedObjects[I]).FSelected then
      Result.Add( TManagedObject(FManagedObjects[I]).FControl );
end;


procedure TessConnectPanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  inherited;
  pt.x:=x;
  pt.y:=y;

  FMemMousePos.x := pt.x;
  FMemMousePos.y := pt.y;

  if (Button = mbLeft) then
  begin
    FIsRectSelecting := True;
    FSelectRect.TopLeft := FMemMousePos;
    FSelectRect.BottomRight := FMemMousePos;

    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := clSilver;
    Canvas.Pen.Mode := pmXor;
    Canvas.Pen.Width := 0;
    Canvas.Rectangle(FSelectRect);
  end;
end;

procedure TessConnectPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  inherited;

    pt.x:=x;
    pt.y:=y;
    if FIsRectSelecting then
    begin
      FMemMousePos := pt;
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Color := clSilver;
      Canvas.Pen.Mode := pmXor;
      Canvas.Pen.Width := 0;
      Canvas.Rectangle(FSelectRect);
      FSelectRect.BottomRight := FMemMousePos;
      Canvas.Rectangle(FSelectRect);
    end;
end;

procedure TessConnectPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if FIsRectSelecting then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Mode := pmXor;
    Canvas.Pen.Width := 0;
    Canvas.Rectangle(FSelectRect);
    FIsRectSelecting := False;
    // Do Select everything inside the rect.
    SelectObjectsInRect(FSelectRect);
  end;
end;

procedure TessConnectPanel.OnManagedObjectClick(Sender: TObject);
begin

end;

procedure TessConnectPanel.OnManagedObjectDblClick(Sender: TObject);
begin
  SetCurrentEntity(TRTFDBOX(Sender).Entity)
end;

procedure TessConnectPanel.ChildMouseDrag(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  pt,pt1: TPoint;
  r: TRect;
  mcont: TManagedObject;
  p2: TPoint;
  i,dx,dy, mdx, mdy: Integer;
  curr: TCrackControl;
  MovedRect : TRect;

  procedure InMakeVisible(C : TRect);
  begin
    mdx := TScrollBox(Parent).HorzScrollBar.Position;
    mdy := TScrollBox(Parent).VertScrollBar.Position;

    if (dx>0) and (C.BottomRight.X >= TScrollBox(Parent).HorzScrollBar.Position + Parent.Width) then
      TScrollBox(Parent).HorzScrollBar.Position := C.BottomRight.X - Parent.Width;

    if (dy>0) and (C.BottomRight.Y >= TScrollBox(Parent).VertScrollBar.Position + Parent.Height) then
      TScrollBox(Parent).VertScrollBar.Position := C.BottomRight.Y - Parent.Height;

    if (dx<0) and (C.Left <= TScrollBox(Parent).HorzScrollBar.Position) then
      TScrollBox(Parent).HorzScrollBar.Position := C.Left;

    if (dy<0) and (C.Top <= TScrollBox(Parent).VertScrollBar.Position) then
      TScrollBox(Parent).VertScrollBar.Position := C.Top;

    mdy := mdy - TScrollBox(Parent).VertScrollBar.Position;
    mdx := mdx - TScrollBox(Parent).HorzScrollBar.Position;

    if (mdx <> 0) or (mdy <> 0) then
    begin
      p2 := Mouse.CursorPos;
      p2.X := p2.X + mdx;
      p2.Y := p2.Y + mdy;
      Mouse.CursorPos := p2;
    end;
  end;
begin

  pt1 := Mouse.CursorPos;

  pt.x := pt1.x;
  pt.Y := pt1.y;
  dx := pt.x - FMemMousePos.x;
  dy := pt.y - FMemMousePos.y;

  IntersectRect(r{%H-},Parent.ClientRect,BoundsRect);
  r.TopLeft := Parent.ClientToScreen(r.TopLeft);
  r.BottomRight := Parent.ClientToScreen(r.BottomRight);

  if (Abs(Abs(dx)+Abs(dy)) > 5) or (FIsMoving) then
  begin
    FMemMousePos := pt;
    FIsMoving := True;
    fNeedCheckSize := True;
    MovedRect:=Rect(MaxInt,0,0,0);
    for i:=0 to FManagedObjects.Count -1 do
    begin
      if TManagedObject(FManagedObjects[i]).Selected then
      begin
        mcont := TManagedObject(FManagedObjects[i]);
        curr := TCrackControl(mcont.FControl);
        if curr.Left+dx >= 0 then
          curr.Left := curr.Left + dx;
        if curr.Top+dy >= 0 then
          curr.Top := curr.Top + dy;
        if (curr.Left + curr.Width + 50) > Width then
          Width := (curr.Left + curr.Width + 50);
        if (curr.Top + curr.Height + 50) > Height then
          Height := (curr.Top + curr.Height + 50);
        if MovedRect.Left=MaxInt then
          MovedRect := curr.BoundsRect
        else
          UnionRect(MovedRect,curr.BoundsRect,MovedRect);
        curr.Invalidate;
        IsModified := True;
      end;
      invalidate;
    end;

    if MovedRect.Left <> MaxInt then
      InMakeVisible(MovedRect);


  end;
end;

procedure TessConnectPanel.OnManagedObjectMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
  inst: TManagedObject;
begin
  if not(ssCtrl in shift) then
        ClearSelection;
  inst := FindManagedControl(Sender as TControl);
  inst.SetSelected(true);

  ClientDragging := True;

  pt := Mouse.CursorPos;
  FMemMousePos.x := pt.x;
  FMemMousePos.y := pt.y;

end;

procedure TessConnectPanel.OnManagedObjectMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);

begin
  if ClientDragging then
    ChildMouseDrag(Sender,Shift,x,y);

end;

procedure TessConnectPanel.OnManagedObjectMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
    inst: TManagedObject;
begin
  ClientDragging := False;
  if fNeedCheckSize then
  begin
     ReCalcSize;
     fNeedCheckSize := False;
  end;
  inst := FindManagedControl(Sender as TControl);
   if not(ssCtrl in shift) then
        ClearSelection;

   inst.SetSelected(true);

end;

procedure TessConnectPanel.Paint;
const
  HANDLESIZE: Integer = 5;
var
  Rect, r2: TRect;
  TopColor, BottomColor: TColor;
  i: Integer;
  con: TDecoratedConnection;


  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

  function CenterOf(const r: TRect): TPoint;
  begin
    Result.x := (r.Left + r.Right) div 2;
    Result.y := (r.Top + r.Bottom) div 2;
  end;

  procedure MakeRectangle(var r: TRect; x1,y1,x2,y2: Integer);
  begin
  r.Left := x1; r.Right := x2;
  r.Top := y1; r.Bottom := y2;
  end;
begin
  Canvas.Pen.Mode := pmCopy;
  Rect := GetClientRect;
  if BevelOuter <> bvNone then
  begin
    AdjustColors(BevelOuter);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  Frame3D(Canvas, Rect, Color, Color, BorderWidth);
  if BevelInner <> bvNone then
  begin
    AdjustColors(BevelInner);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;

  if Assigned(FBackBitmap) then
    Canvas.Brush.Bitmap := FBackBitmap
  else
    Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect);

  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := 3;

  for i:=0 to FConnections.Count -1 do
  begin
     con := (FConnections[i] as TDecoratedConnection);
     if con.IsDirty then con.Refresh;
     FPathLayout.DrawConnection(con, Canvas);


{    conn := (FConnections[i] as TConnection);
    if (not Conn.FFrom.Visible) or (not Conn.FTo.Visible) then
      Continue;
    case conn.FConnectStyle of
      csThin:
        begin
          Canvas.Pen.Width := 1;
          Canvas.Pen.Style := psSolid;
        end;
      csNormal:
        begin
          Canvas.Pen.Width := 3;
          Canvas.Pen.Style := psSolid;
        end;
      csThinDash:
        begin
          Canvas.Pen.Width := 1;
          Canvas.Pen.Style := psDash;
        end;
    end;

    if  Conn.ArrowStyle=asEmptyOpen then
      CalcConnectionEnds(conn.FFrom.BoundsRect,conn.FTo.BoundsRect,Conn.ArrowStyle=asEmptyClosed, p{%H-},p1{%H-})
    else
      CalcShortest(conn.FFrom.BoundsRect,conn.FTo.BoundsRect,p{%H-},p1{%H-});

    if FindManagedControl(conn.FFrom).Selected and (not FSelectedOnly) then
      Canvas.Pen.Color := clGreen
    else
      Canvas.Pen.Color := clBlack;

    Canvas.Brush.Color := clWhite;
    DrawArrow(Canvas,p,p1,Conn.ArrowStyle);}
  end;

  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Bitmap := nil;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clBlack;

  //Grab-handles
  if not ClientDragging then
  if not FSelectedOnly then for i:=0 to FManagedObjects.Count -1 do
  begin
    if TManagedObject(FManagedObjects[i]).Selected and (TManagedObject(FManagedObjects[i]).FControl.Visible) then
    begin
      Rect := TManagedObject(FManagedObjects[i]).FControl.BoundsRect;
      MakeRectangle(r2{%H-}, Rect.Left -HANDLESIZE, Rect.Top -HANDLESIZE, Rect.Left+HANDLESIZE, Rect.Top+HANDLESIZE);
      Canvas.FillRect(r2);
      MakeRectangle(r2, Rect.Right -HANDLESIZE, Rect.Top -HANDLESIZE, Rect.Right+HANDLESIZE, Rect.Top+HANDLESIZE);
      Canvas.FillRect(r2);
      MakeRectangle(r2, Rect.Left -HANDLESIZE, Rect.Bottom -HANDLESIZE, Rect.Left+HANDLESIZE, Rect.Bottom+HANDLESIZE);
      Canvas.FillRect(r2);
      MakeRectangle(r2, Rect.Right -HANDLESIZE, Rect.Bottom -HANDLESIZE, Rect.Right+HANDLESIZE, Rect.Bottom+HANDLESIZE);
      Canvas.FillRect(r2);
    end;
  end;
end;

procedure TessConnectPanel.RecalcSize;
var
  i, xmax, ymax: Integer;
begin
  xmax := 300;
  ymax := 150;
  for i:=0 to ControlCount -1 do
  begin
    if (Controls[i].Align <> alNone) or (not Controls[i].Visible) then
      Continue;
    xmax := Max(xmax,Controls[i].Left + Controls[i].Width + 50);
    ymax := Max(ymax,Controls[i].Top + Controls[i].Height + 50);
  end;
  SetBounds(Left,Top,xmax,ymax);
  if Assigned(OnContentChanged) then
    OnContentChanged(nil);
end;

procedure TessConnectPanel.SelectObjectsInRect(SelRect: TRect);
var
  i: Integer;
  r1,r2: TRect;
begin
  r1 := SelRect;
  if (SelRect.Top > SelRect.Bottom) then
  begin
    SelRect.Top := r1.Bottom;
    SelRect.Bottom := r1.Top;
  end;
  if (SelRect.Left > SelRect.Right) then
  begin
    SelRect.Left := r1.Right;
    SelRect.Right := r1.Left;
  end;

  for i:=0 to FManagedObjects.Count -1 do
  begin
    r1 := TCrackControl(TManagedObject(FManagedObjects[i]).FControl).BoundsRect;
    IntersectRect(r2{%H-},SelRect,r1);
    if EqualRect(r1,r2) and TManagedObject(FManagedObjects[i]).FControl.Visible then
      TManagedObject(FManagedObjects[i]).Selected := True;
  end;
end;

procedure TessConnectPanel.SetFocus;
var
  F : TCustomForm;
  X,Y : integer;
begin
  F := GetParentForm(Self);

  // Try to see if we can call inherited, otherwise there is a risc of getting
  // 'Cannot focus' exception when starting from delphi-tools.
  if CanFocus and (Assigned(F) and F.Active) then
  begin
    // To avoid having the scrollbox resetting its positions after a setfocus call.
    X := (Parent as TScrollBox).HorzScrollBar.Position;
    Y := (Parent as TScrollBox).VertScrollBar.Position;
    inherited;
    (Parent as TScrollBox).HorzScrollBar.Position := X;
    (Parent as TScrollBox).VertScrollBar.Position := Y;
  end;

//  if GetCaptureControl <> Self then SetCaptureControl(Self);
end;

procedure TessConnectPanel.WMEraseBkgnd(var Message: TWmEraseBkgnd);
var
  can : Tcanvas;
begin
  can := tcanvas.create;
  try
    can.handle := message.DC;
    if Assigned(FBackBitmap) then
      Can.Brush.Bitmap := FBackBitmap
    else
      Can.Brush.Color := Color;
    Can.FillRect(ClientRect);
  finally
    can.free;
  end;
  Message.Result := 1;
end;

procedure TessConnectPanel.SetPathStyle(AValue: TPathLayoutStyle);
begin
  if AValue <> FPathStyle then
  begin
    FPathStyle := AValue;
    if Assigned(Self.PathLayout) then
      FPathLayout.Free;
    case AValue of
      plsOrtho: FPathLayout := TOrthoPathLayout.Create(Self);
      plsVector: FPathLayout := TVectorPathLayout.Create(Self);
      plsRoundedOrtho: FPathLayout := TRoundedPathLayout.Create(Self);
    end;
    Invalidate;
  end
end;

procedure TessConnectPanel.SetSelectedOnly(const Value : boolean);
var
  I : integer;
begin
  if FSelectedOnly <> Value then
  begin
    FSelectedOnly := Value;

    if FSelectedOnly then
    begin
      TempHidden.Clear;
      for i:=0 to FManagedObjects.Count -1 do
        if (not TManagedObject(FManagedObjects[i]).Selected) and TManagedObject(FManagedObjects[i]).FControl.Visible then
        begin
          TManagedObject(FManagedObjects[i]).FControl.Visible := False;
          TempHidden.Add( TObject(FManagedObjects[i]));
        end;
    end
    else
    begin
      for I := 0 to TempHidden.Count-1 do
        TManagedObject(TempHidden[I]).FControl.Visible := True;
      TempHidden.Clear;
    end;

  end;
end;


{ TManagedObject }

destructor TManagedObject.Destroy;
begin
  if Assigned(FControl) then FreeAndNil(FControl);
  inherited;
end;

procedure TManagedObject.SetSelected(const Value: Boolean);
begin
  FControl.Parent.Invalidate;
  FSelected := Value;
  if FSelected then
    FControl.BringToFront;
end;

end.

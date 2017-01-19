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
unit uClassTreeEditIntegrator;

{$mode objfpc}{$H+}

interface

uses
  Controls,
  uViewIntegrator, uModel, uFeedback, uClassTreeEditForm;

type

  { TClassTreeEditIntegrator }

  TClassTreeEditIntegrator = class(TViewIntegrator)
  private
    MyForm: TClassTreeEditForm;
  public
    constructor Create(om: TObjectModel; AParent: TWinControl; AFeedback : IEldeanFeedback = nil); override;
    destructor Destroy; override;
    procedure CurrentEntityChanged; override;
  end;

implementation


{ TClassTreeEditIntegrator }

constructor TClassTreeEditIntegrator.Create(om: TObjectModel;
  AParent: TWinControl; AFeedback: IEldeanFeedback);
begin
  inherited Create(om, AParent, AFeedback);
  MyForm := ClassTreeEditForm;
  MyForm.Model := om;
end;

destructor TClassTreeEditIntegrator.Destroy;
begin
  inherited Destroy;
end;

procedure TClassTreeEditIntegrator.CurrentEntityChanged;
begin
  inherited CurrentEntityChanged;
  if  MyForm.Visible then
    MyForm.ModelObject := CurrentEntity;
end;

end.


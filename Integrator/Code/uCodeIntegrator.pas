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

unit uCodeIntegrator;

{$mode objfpc}{$H+}

interface

uses
  uIntegrator, uCodeProvider;

type

  TCodeIntegrator = class(TTwowayIntegrator)
  private
    FCodeProvider: TCodeProvider;
    procedure SetCodeProvider(const Value: TCodeProvider);
  public
    property CodeProvider: TCodeProvider read FCodeProvider write SetCodeProvider;
  end;

implementation

procedure TCodeIntegrator.SetCodeProvider(const Value: TCodeProvider);
begin
  if Assigned(FCodeProvider) then
  begin
    FCodeProvider.Free;
    FCodeProvider := nil;
  end;

  FCodeProvider := Value;
end;


end.


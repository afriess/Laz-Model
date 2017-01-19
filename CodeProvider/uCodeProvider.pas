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

unit uCodeProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uFeedback, uConst;

type

  TCodeChangeType = (cctAdd, cctRemove, cctChange);

  TCodeChangeEvent = procedure(ChangeType: TCodeChangeType; Namn: string) of object;

  TCodeProvider = class
  private
    FOnCodeChange: TCodeChangeEvent;
    FActive: Boolean;
    FSearchPath: TStringList;
    FWatched: TStringList;
    procedure SetActive(const Value: Boolean);
  protected
    Feedback : IEldeanFeedback;
    LoadedCount : integer;
    procedure HookChanges; virtual; abstract;
    procedure UnhookChanges; virtual; abstract;
    procedure AddChangeWatch(AName: string);
  public
    constructor Create(AFeedback : IEldeanFeedback = nil);
    destructor Destroy; override;

    function LocateFile(const AName: string): string; virtual; abstract;
    function LoadStream(const AName: string): TStream; virtual; abstract;
    procedure SaveStream(const AName: string; AStream: TStream); virtual; abstract;

    procedure AddSearchPath(APath: string);

    property Active: Boolean read FActive write SetActive;
    property SearchPath: TStringList read FSearchPath;
    property OnCodeChange: TCodeChangeEvent read FOnCodeChange write FOnCodeChange;
  end;

implementation

procedure TCodeProvider.AddChangeWatch(AName: string);
begin
  FWatched.Add(AName);
  if Active then HookChanges; // Attach 'again' to recieve changes to this file.
end;

procedure TCodeProvider.AddSearchPath(APath: string);
begin
  if APath<>'' then
  begin
    if APath[Length(APath)] <> PathDelim then
      APath := APath + PathDelim;
    if FSearchPath.IndexOf(APath) < 0 then
      FSearchPath.Add(APath);
  end;
end;

constructor TCodeProvider.Create(AFeedback : IEldeanFeedback = nil);
begin
  inherited Create;
  FSearchPath := TStringList.Create;
  FWatched := TStringList.Create;
  if Feedback=nil then
    Self.Feedback := NilFeedback
  else
    Self.Feedback := AFeedback;
end;

destructor TCodeProvider.Destroy;
begin
  Feedback.Message( IntToStr(LoadedCount) + rsFilesRead_ending_lc);
  FreeAndNil(FSearchPath);
  FreeAndNil(FWatched);
  inherited;
end;

procedure TCodeProvider.SetActive(const Value: Boolean);
begin
  if (not Active) and Value then
  begin
      // Activate source change hook
    HookChanges;
    FActive := Value;
  end
  else if Active and (not Value) then
  begin
      // Deactivate source change hook
    UnhookChanges;
    FActive := Value;
  end;
end;

end.

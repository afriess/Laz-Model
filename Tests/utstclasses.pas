{
 MODEL   STATUS Working.
 DOC     STATUS Not Implemented.
 CODEGEN STATUS Not Implemented.
}
unit utstClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TTestClass = class (TObject)
  private
    FValue: string;
  protected
    // virtual methods
    procedure AddItem(AValue: string); virtual; abstract;
    procedure AddItem2(const AValue: string); dynamic;
    procedure Undo(Maybe: boolean = True);
    procedure Redo(Maybe: boolean = False);
  public
    // simple constructor destuctor
    constructor create; override;
    destructor destroy; override;
  published
    property Value: string read FValue write FValue;
  end;

  TTestClass1 = class (TTestClass)
    protected
      procedure AddItem(AValue: string); reintroduce;
      procedure AddItem2(const AValue: string); override;
  end;

implementation

end.


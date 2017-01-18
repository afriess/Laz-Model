unit utstUnitDefinesDefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
{$IFDEF WINDOWS}
  Types,
  LCLType
{$ENDIF}
{$IFDEF UNIX}
  Types,
  LCLType,
  dynlibs
{$ENDIF}
{$IFDEF DARWIN}
  MacOsAll
{$ENDIF}
 ;

type

  TTestClass = class
    private
      FAttr: string;
  end;


implementation

end.


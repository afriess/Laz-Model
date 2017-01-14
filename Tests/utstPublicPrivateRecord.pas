{
 MODEL   STATUS Not Working.
 DOC     STATUS Not Implemented.
 CODEGEN STATUS Not Implemented.
}
unit utstPublicPrivateRecord;
// scanner seems to ignore this  so done on the ''command line''
{$mode Delphi}

interface

{ requires following patch

fpc\3.0.0\source\packages\fcl-passrc\src\pparser.pp
***************
*** 3861,3867 ****
              begin
              If not (po_delphi in Scanner.Options) then
                ParseExc(SErrRecordVisibilityNotAllowed);
!             if not (v in [visPrivate,visPublic,visStrictPrivate]) then
                ParseExc(SParserInvalidRecordVisibility);
              NextToken;
              Continue;
--- 3861,3867 ----
              begin
              If not (po_delphi in Scanner.Options) then
                ParseExc(SErrRecordVisibilityNotAllowed);
!             if not (v in [visPrivate,visPublic,visStrictPrivate,visProtected]) then
                ParseExc(SParserInvalidRecordVisibility);
              NextToken;
              Continue;

}

uses
  Classes, SysUtils;

type

  TTest1 = record
    a : integer;
    function Test(aRecurse: Boolean): Integer;
  end;

  TTest2 = record
  private
    A,b : integer;
  public
    procedure setA(AValue : integer);
    property SafeA : Integer Read A Write SetA;
  end;

  TTest3 = packed record
  private
    fA,fb : byte;
    procedure setA(AValue : Integer);
    function geta : integer;
  public
    property A : Integer Read GetA Write SetA;
  end;

  TTest4 = record
   private
     a : Integer;
   protected
     function getp : integer;
   public
     b : string;
     procedure setp (aValue : integer);
     property p : integer read Getp Write SetP;
   public
   case x : integer of
     1 : (Q : string);
     2 : (S : String);
   end;


implementation

end.


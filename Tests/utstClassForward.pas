{
 MODEL   STATUS Not Working.
 DOC     STATUS Not Implemented.
 CODEGEN STATUS Not Implemented.
}
unit utstClassForward;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TForward = class;

  TOther = class(TObject)
    private
     FForward : TForward;
    public
     property AForward: TForward read FForward write FForward;
  end;

  TForward = class(TObject)
   public
    FString : string;
  end;
  
  TCanSeeEverthing = class(TObject)
   public 
    FForward : TForward;
    FOther : TOther;
  end;  
   
   


implementation

end.


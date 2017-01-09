unit utstrecordtypes;

{
  UML 2.5 10.2.3.1 these should  be represented as structured datatypes
  with attributes. ? How to handle RPoint below.


}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  Point = Record
          X,Y,Z : Real;
          end;

  RPoint = Record
          Case Boolean of
          False : (X,Y,Z : Real);
          True : (R,theta,phi : Real);
          end;

  BetterRPoint = Record
          Case UsePolar : Boolean of
          False : (X,Y,Z : Real);
          True : (R,theta,phi : Real);
          end;

  MyRec = Record
        X : Longint;
        Case byte of
          2 : (Y : Longint;
               case byte of
               3 : (Z : Longint);
               );
        end;


 //The following two types should be identical apart from name
 {$PackRecords 1}
   Trec2A = Record
     A : Byte;
     B : Word;
   end;
 {$PackRecords default}


 Trec2B = Packed Record
    A : Byte;
    B : Word;
 end;

implementation

end.

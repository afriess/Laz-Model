{
 MODEL   STATUS Not Working.
 DOC     STATUS Not Implemented.
 CODEGEN STATUS Not Implemented.
}
unit utstBoundedInteger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
    { Throws error in passrc.
      this is a copy of TGraphicsColor = -$7FFFFFFF-1..$7FFFFFFF; in GraphType,
      so is valid and cross platform.
    }

   {
     implied integer, Model as bounded integer. Integer has no size in UML
   }
   // Like TColor, but avoiding dependency on Graphics.
   TChartColor = -$7FFFFFFF-1..$7FFFFFFF;


implementation

end.


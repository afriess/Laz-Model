{
 MODEL   STATUS Not Working.
 DOC     STATUS Not Implemented.
 CODEGEN STATUS Not Implemented.
}
unit utstEscapedKeywordInClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TGroupType = (gtProject, gtPackage, gtOther);

  TGroupItem = class
  public
    { passrc baulks at &Type }
    &Type: TGroupType;
    Title: string;
    Package: TIDEPackage;
    GroupTabLabel: TGroupTabLabelClass;
    Files: TStringList;

    constructor Create(AType: TGroupType; const ATitle: string; APackage: TIDEPackage);
    destructor Destroy; override;
  end;


implementation

end.


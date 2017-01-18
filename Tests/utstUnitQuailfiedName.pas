{
 MODEL   STATUS Not Working.
 DOC     STATUS Not Implemented.
 CODEGEN STATUS Not Implemented.
}
unit utstUnitQuailfiedName;

{$mode objfpc}{$H+}

{
  This test class poses a few questions.

  Do we keep &| store &| show the qualified name?

  The qualified name may be required for correct compilation. e.g.
  In Laz-Model we have a class called TOperation.
  There is also the following procedure (which may be removed as it is part
  of the old mouse handling) which uses
   TOperation = (opInsert, opRemove);  from the fpc RTL.
  procedure TRtfdBox.Notification(AComponent: TComponent; Operation: Classes.TOperation);

  Here one is a Class and one is a DataType. The DataType HAS to be qualified
  for proper compilation.

  However there is also the usage in uTreeViewIntegrator where TClass is
  fully qualified throughout the unit, without any ryhme or reason. ( although
  it is probably just a case of having had to qualify in much earlier versions
  of the codebase.)

  Probably the best idea is to add IsQualified to TClassifiers in the model,
  but keep as simple full name if thrown in the unknown bucket without the
  IsQualified set

  So In the above examples uModel.TClass would be represented as a
   classifer with name TClass, with is Qualified, while  Classes.TOperation
   would be thrown in the unknown datatype bucket as a classifier with name
   Classes.TOperation

   If at some later date we get round to parsing the Classes unit, then at this
   point it will become shown as TOperation::IsQualified=True.
}
interface

uses
  Classes, SysUtils;

type

  TQNClass1 = class(TObject)
    public
      name: string;
  end;


  TQNClass2 = class(TObject)
    private
      FClass1:  utstUnitQualifiedName.TQNClass1;
      FOTher: Classes.TOperation;
    public
  end;


implementation

end.


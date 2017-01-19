{
  Laz-Model
  Copyright (C) 2017 Peter Dyson. Initial Lazarus port
  Portions (C) 2002  Eldean AB, Peter Söderman, Ville Krumlinde

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
unit uFPDocGen;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, laz2_dom, laz2_XMLRead, laz2_XMLWrite,
  uUseful, uModelEntity, uModel, uDocGen, uIterators, uConfig;

type

  TFPDocGen = class(TDocGen)
    private
      FConfigFile: TXMLDocument;
      FPackageFile: TXMLDocument;
      FUnitsNode: TDOMNode;
      FDescriptionsNode: TDOMNode;
      FModuleNode: TDOMNode;
      FPackageName : string;
      FDirs: TStringList;
      procedure GetConfigFile(SubDir: String);
      procedure SaveConfigFile(SubDir: String);
      procedure AddSkeletonConfig;
      procedure AddSkeletonPackage(pname: string);
      procedure GetValidSubDirs;
      procedure CheckConfigForUnit(P: TUnitPackage; Dir: string);
      function HasPackageElement(entName: string): boolean;
      function GetPackageElement(entName: string): TDOMNode;
      function UnitInConfig(fname: string): boolean;
      procedure ProcessPackage(P: TUnitPackage; Dir: string);
      procedure PackageDone(P: TUnitPackage; Dir: string);
      procedure ProcessDataTypes(P: TUnitPackage);
      function SALinkExists(lnkName: string; seeNode: TDOMNode): boolean;
      procedure GetPackageFile(fname: string);
      procedure SavePackageFile(fname: string);
      procedure AddEnumLiterals(ent: TEnumeration);
      function AddEntity(EntName: string): TDOMNode;
      procedure AddParentLinks(Ent: TModelEntity; seeNode: TDOMNode);
    protected
      procedure DocStart; override;
      procedure TraverseModel; override;
      procedure WriteClassDetail(C : TClass); override;
      procedure DocFinished; override;
      procedure WriteInterfaceDetail(I: TInterface); override;
    public
      destructor Destroy; override;
  end;


implementation

procedure TFPDocGen.GetConfigFile(SubDir: String);
var
  nod : TDOMNode;
  fname: string;
begin
  try
    // subdirectories should be 2 letter ISO contry code.
    if Length(SubDir) = 2 then
       fname := DestPath + SubDir + PathDelim + 'FPDoc.xml'
     else
       fname := DestPath + 'FPDoc.xml';

    ReadXMLFile(FConfigFile, fname);
  except
    If not Assigned (FConfigFile) then
       FConfigFile := TXMLDocument.Create;
    AddSkeletonConfig;
  end;

  //find the units node
  nod := FConfigFile.DocumentElement.FindNode('packages');
  FUnitsNode := nod.FirstChild.FirstChild;
  FDescriptionsNode := FUnitsNode.NextSibling;

end;

procedure TFPDocGen.GetPackageFile(fname: string);
var
  pname: string;
begin
   try
     ReadXMLFile(FPackageFile, fname);
   except
     If not Assigned (FPackageFile) then
       FPackageFile := TXMLDocument.Create;
     pname := ExtractFileName(fname);
     pname := LeftStr(pname, Length(pname)-4);
     AddSkeletonPackage(pname);
   end;

  //find the module node       fpdoc      package    module
  FModuleNode := FPackageFile.FirstChild.FirstChild.FirstChild;

end;

procedure TFPDocGen.SaveConfigFile(SubDir: String);
var
  fname: string;
begin
  if Length(SubDir) = 2 then
     fname := DestPath + SubDir + PathDelim + 'FPDoc.xml'
   else
     fname := DestPath + 'FPDoc.xml';
  WriteXMLFile(FConfigFile, fname);
  if Assigned(FConfigFile) then
     FreeAndNil(FConfigFile);
end;

procedure TFPDocGen.AddSkeletonConfig;
var
  RootNode, packagesnod, packagenod, optionsnod, nod: TDOMNode;
begin

  RootNode := FConfigFile.CreateElement('docproject');
  FConfigFile.AppendChild(RootNode);

  optionsnod := FConfigFile.CreateElement('options');
  RootNode.AppendChild(optionsnod);

  nod := FConfigFile.CreateElement('option');
  TDOMElement(nod).SetAttribute('ostarget', 'Linux');
  optionsnod.AppendChild(nod);

  nod := FConfigFile.CreateElement('option');
  TDOMElement(nod).SetAttribute('cputarget', 'x86_64');
  optionsnod.AppendChild(nod);

   nod := FConfigFile.CreateElement('option');
  TDOMElement(nod).SetAttribute('show-private', 'false');
  optionsnod.AppendChild(nod);

  nod := FConfigFile.CreateElement('option');
  TDOMElement(nod).SetAttribute('stop-on-parser-error', 'false');
  optionsnod.AppendChild(nod);

  packagesnod := FConfigFile.CreateElement('packages');
  RootNode.AppendChild(packagesnod);

  packagenod := FConfigFile.CreateElement('package');
  TDOMElement(packagenod).SetAttribute('name', FPackageName);
  TDOMElement(packagenod).SetAttribute('output', FPackageName);
  packagesnod.AppendChild(packagenod);

  nod := FConfigFile.CreateElement('units');
  packagenod.AppendChild(nod);

  nod := FConfigFile.CreateElement('descriptions');
  packagenod.AppendChild(nod);

end;

procedure TFPDocGen.AddSkeletonPackage(pname: string);
var
  RootNode: TDOMNode;
  PackNode: TDOMNode;
  ModuleNode: TDOMNode;
  DescNode: TDOMNode;
begin
   RootNode := FPackageFile.CreateElement('fpdoc-descriptions');
   FPackageFile.AppendChild(RootNode);

   PackNode := FPackageFile.CreateElement('package');
   TDOMElement(PackNode).SetAttribute('name', FPackageName );
   RootNode.AppendChild(PackNode);

   ModuleNode := FPackageFile.CreateElement('module');
   TDOMElement(ModuleNode).SetAttribute('name', pname );
   PackNode.AppendChild(ModuleNode);

   DescNode := FPackageFile.CreateElement('descr');
   ModuleNode.AppendChild(DescNode);

end;

procedure TFPDocGen.GetValidSubDirs;
var
  Info : TSearchRec;
begin
  FDirs := TStringList.Create;
  If FindFirst (DestPath + '*',faDirectory,Info)=0 then
  repeat
    if (Length(Info.Name) = 2) and (Info.Name <> '..') then
      FDirs.Add(Info.Name);
  until FindNext(info) <> 0;;
end;

procedure TFPDocGen.CheckConfigForUnit(P: TUnitPackage; Dir: string);
var
  fname: string;
  nod: TDOMNode;
begin
  fname := ExtractRelativepath(Dir, P.Sourcefilename);
  if not UnitInConfig(fname) then
  begin
    nod := FConfigFile.CreateElement('unit');
    TDOMElement(nod).SetAttribute('file', fname);
    FUnitsNode.AppendChild(nod);
    nod := FConfigFile.CreateElement('description');
    fname := ExtractFileName(ChangeFileExt(fname, '.xml'));
    TDOMElement(nod).SetAttribute('file', fname);
    FDescriptionsNode.AppendChild(nod);
  end;
end;

function TFPDocGen.HasPackageElement(entName: string): boolean;
begin
  Result := Assigned(GetPackageElement(entName));
end;

function TFPDocGen.GetPackageElement(entName: string): TDOMNode;
var
  nod: TDOMNode;
  attr: TDOMNode;
begin
  Result := nil;
  nod := FModuleNode.FirstChild;
  while nod <> nil do
  begin
     attr := nod.Attributes.GetNamedItem('name');
     if Assigned(attr) then
       if TDOMAttr(attr).Value = entName then
       begin
          Result := nod;
          Break;
       end;
     nod := nod.NextSibling;
  end;

end;

function TFPDocGen.UnitInConfig(fname: string): boolean;
var
  nod: TDOMNode;
  attr: TDOMNode;
begin
  Result := False;
  nod := FUnitsNode.FirstChild;
  while nod <> nil do
  begin
     attr := nod.Attributes.GetNamedItem('file');
     if TDOMAttr(attr).Value = fname then
     begin
        Result := True;
        Break;
     end;
     nod := nod.NextSibling;
  end;

end;

procedure TFPDocGen.ProcessPackage(P: TUnitPackage; Dir: string);
begin
    GetPackageFile(Dir + P.Name + '.xml');
    ProcessDataTypes(P);
end;

procedure TFPDocGen.ProcessDataTypes(P: TUnitPackage);
var
  Mi : TModelIterator;
  ent: TClassifier;
begin
  Mi := TModelIterator.Create(P.GetClassifiers,TDataTypeFilter.Create, ioAlpha);
  if Mi.Count > 0 then
    while Mi.HasNext do
    begin
      ent := Mi.Next as TClassifier;
      if (ent is uModel.TDataType) then
      begin
        if not HasPackageElement(ent.Name) then
        begin
          AddEntity(ent.Name);
          if (ent is TEnumeration) then
             AddEnumLiterals(ent as TEnumeration);
        end;
      end;
    end;
end;

function TFPDocGen.SALinkExists(lnkName: string; seeNode: TDOMNode): boolean;
var
  nod: TDOMNode;
  attr: TDOMNode;
begin
  Result := False;
  nod := seeNode.FirstChild;
  while nod <> nil do
  begin
     attr := nod.Attributes.GetNamedItem('id');
     if Assigned(attr) then
       if TDOMAttr(attr).Value = lnkName then
       begin
          Result := True;
          Break;
       end;
     nod := nod.NextSibling;
  end;

end;

procedure TFPDocGen.SavePackageFile(fname: string);
begin
  WriteXMLFile(FPackageFile, fname);
  if Assigned(FPackageFile) then
     FreeAndNil(FPackageFile);
end;

procedure TFPDocGen.AddEnumLiterals(ent: TEnumeration);
var
  Pi: TModelIterator;
  tel: TEnumLiteral;
begin
   Pi := TModelIterator.Create(ent.GetFeatures);
   if Pi.Count > 0 then
   while Pi.HasNext do
   begin
     tel := TEnumLiteral(Pi.Next);
     if not  HasPackageElement(ent.name + '.' + tel.Name) then
       AddEntity(ent.name + '.' + tel.Name);
   end;
end;

function TFPDocGen.AddEntity(EntName: string): TDOMNode;
var
  entNode: TDOMNode;
  shortNode: TDOMNode;
  descrNode: TDomNode;
  seealsoNode: TDOMNode;
begin
  entNode := FPackageFile.CreateElement('element');
  TDOMElement(entNode).SetAttribute('name', EntName);
  FModuleNode.AppendChild(entNode);
  shortNode := FPackageFile.CreateElement('short');
  entNode.AppendChild(shortNode);
  descrNode := FPackageFile.CreateElement('descr');
  entNode.AppendChild(descrNode);
  seealsoNode := FPackageFile.CreateElement('seealso');
  entNode.AppendChild(seealsoNode);
  Result := entNode;
end;

procedure TFPDocGen.AddParentLinks(Ent: TModelEntity; seeNode: TDOMNode);
var
  par: TModelEntity;
  lnkName: string;
  linkNode: TDOMNode;
begin
  case Ent.ClassName of
     'TClass':
       begin
         par := TClass(Ent).Ancestor;
       end;
     'TInterface':
       begin
         par := TInterface(Ent).Ancestor;
       end;
  end;

  if (Assigned(par)) and (par.Owner<>Model.UnknownPackage) then
  begin
    lnkName := par.Owner.Name + '.' + par.Name;
    if not SALinkExists(lnkName, seeNode) then
    begin
      linkNode := FPackageFile.CreateElement('link');
      TDOMElement(linkNode).SetAttribute('id', lnkName);
      TDOMElement(linkNode).TextContent := par.Name + ' {Parent of}';
      seeNode.AppendChild(linkNode);
      AddParentLinks(par,seeNode);
    end;
  end;

end;

procedure TFPDocGen.PackageDone(P: TUnitPackage; Dir: string);
begin
  SavePackageFile(Dir + P.Name + '.xml');
end;

procedure TFPDocGen.DocStart;
begin

  FPackageName := Config.ProjectName;

  // find 2 letter subirs of DestPath and add them to FDirs
  GetValidSubDirs;

end;

procedure TFPDocGen.TraverseModel;
var
  P : TUnitPackage;
  Pro : IEldeanProgress;
  PCount : integer;
  curPath : string;
  i: integer;
  Mi: TModelIterator;
begin
  Packages.Reset;

  //Init progressbar
  PCount := 0;
  while Packages.HasNext do
  begin
    Inc(PCount);
    Packages.Next;
  end;
  PCount := PCount * FDirs.Count;

  Pro := TEldeanProgress.Create('Generating documentation...',PCount);

  for i := 0 to FDirs.Count - 1 do
  begin
    GetConfigFile(FDirs[i]);
    Packages.Reset;
    while Packages.HasNext do
    begin
      P := Packages.Next as TUnitPackage;
      curPath := DestPath + FDirs[i] + PathDelim;

      CheckConfigForUnit(P, curPath);

      // Package and DataType details
      ProcessPackage(P, curPath);

      //Interface details
      Mi := TModelIterator.Create(P.GetClassifiers,TInterface,Low(TVisibility),ioAlpha);
      while Mi.HasNext do
        WriteInterfaceDetail(Mi.Next as TInterface);

      //Class details
      Mi := TModelIterator.Create(P.GetClassifiers,TClass,Low(TVisibility),ioAlpha);
      while Mi.HasNext do
        WriteClassDetail(Mi.Next as TClass);

      PackageDone(P, curPath);

      Pro.Tick;
    end;

    SaveConfigFile(FDirs[i]);
  end;
end;

procedure TFPDocGen.WriteClassDetail(C: TClass);
var
  ent: TModelEntity;
  Mi: TModelIterator;
  clsNode: TDOMNode;
  seeNode: TDOMNode;
begin
  clsNode := getPackageElement(C.Name);
  if not Assigned(clsNode) then
     clsNode := AddEntity(C.Name);
  seeNode := clsNode.FindNode('seealso');
  if Assigned(seeNode) then
    AddParentLinks(C as TModelEntity, seeNode);

  Mi := TModelIterator.Create(C.GetAttributes, ioAlpha);
  if Mi.Count > 0 then
    while Mi.HasNext do
    begin
      ent := Mi.Next;
      if not HasPackageElement(C.Name + '.' + ent.Name) then
           AddEntity(C.Name + '.' + ent.Name);
    end;

  Mi := TModelIterator.Create(C.GetOperations, ioAlpha);
  if Mi.Count > 0 then
    while Mi.HasNext do
    begin
      ent := Mi.Next;
      if not HasPackageElement(C.Name + '.' + ent.Name) then
           AddEntity(C.Name + '.' + ent.Name);
    end;

end;

procedure TFPDocGen.DocFinished;
begin
   // stub
end;

procedure TFPDocGen.WriteInterfaceDetail(I: TInterface);
var
  intfNode: TDOMNode;
  seeNode: TDOMNode;
  ent: TModelEntity;
  Mi: TModelIterator;
begin
  intfNode := getPackageElement(I.Name);
  if not Assigned(intfNode) then
     intfNode := AddEntity(I.Name);
  seeNode := intfNode.FindNode('seealso');
  if Assigned(seeNode) then
    AddParentLinks(I as TModelEntity, seeNode);

  Mi := TModelIterator.Create(I.GetAttributes, ioAlpha);
  if Mi.Count > 0 then
    while Mi.HasNext do
    begin
      ent := Mi.Next;
      if not HasPackageElement(I.Name + '.' + ent.Name) then
           AddEntity(I.Name + '.' + ent.Name);
    end;

  Mi := TModelIterator.Create(I.GetOperations, ioAlpha);
  if Mi.Count > 0 then
    while Mi.HasNext do
    begin
      ent := Mi.Next;
      if not HasPackageElement(I.Name + '.' + ent.Name) then
           AddEntity(I.Name + '.' + ent.Name);
    end;

end;

destructor TFPDocGen.Destroy;
begin
  FreeAndNil(FDirs);
  inherited Destroy;
end;

end.


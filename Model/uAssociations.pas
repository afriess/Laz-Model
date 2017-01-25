unit uAssociations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uModelEntity, uDocumentation;

type
  // this may or may not go
  TAssociationType =(
    atOwnedMember,
    atMemberReference,                 // pointer members declared ^Thing
    atDirectInheritance,
    atInterfaceInheritance,
    atSpecializeInheritance,
  );

  // this could be a record but as a class a nil value can be assigned when it is member of a class to indicate absence.
  // May be expanded for model checking purposes.
  // BNF
  // <multiplicity> ::= <multiplicity-range> [ [ ‘{‘ <order-designator> [‘,’ <uniqueness-designator> ] ‘}’ ] |
  // [ ‘{‘ <uniqueness-designator> [‘,’ <order-designator> ] ‘}’ ] ]
  // <multiplicity-range> ::= [ <lower> ‘..’ ] <upper>
  //  <lower> ::= <value-specification>
  //  <upper> ::= <value-specification>
  // <order-designator> ::= ‘ordered’ | ‘unordered’
  // <uniqueness-designator> ::= ‘unique’ | ‘nonunique’

  TMultiplicty = class
    lower : string;          // specs say this should be non-negative integer but pascal allows ordinal strings,( EnumLiterals, Constants) which does actually fulfill the constraint lower_is_integer 7.8.8.8 although pascal differentiates between integer and ordinal.
    upper : string;          // Unlimited Natural defined as positive integers + *, as for lower, ordinal strings are allowed.
    isOrdered: boolean;
    isUnique: boolean;
  end;

  {
   UML abstract Relationship in Figure 7.1 Root.
   True abstract relationship would be an attribute of a base UML Primitive DataType  i.e. ThisItem: String;
   There is no requirement to create a directed relationship for Primitive DataTypes.
   However most other nodeling tools seem to have Language base packages, presumably so there can
   be a relationship for all members Primitive or not, and allow for Language Specific primitives.
   This would seem like a reasonable idea. Arrays could be a Template DataType where cardinallity is
   in the Association End and the Array Type would be the binding association. Similar notation could
   be used for Set of Enum. And allow for Array of Array of DataType as unbound template chain.

   Conclusion: This class remains abstract and can only be included in DirectedRelationships.
               Create DirectedRelationShips for all members.
               We required  DataType Templates.
  }
  TAssociationEnd = class
    Cardinality: TMultipicity;
    Name: string;             // this is the name that the other end uses as its internal identifier. nil string for absence
    Entity: TModelEntity;     // the entity this end of the DirectedAssociation is linked to.
    Navigable: boolean;       // default is navigable from Owner -> Target.
  end;




  TBinding = class
    BoundName: string;                // <T>  <T1,T1>???
    BindTo: TModelEntity;
  end;


  {
    Enumerate, classify or Subclass UML/XMI Associations??

    Common                             36 Association Types
    Values                             19 Association Types
    Classification                     47 Association Types
    Classifiers                        16 Association Types
    Structuredd Classifiers            35 Association Types
    Packages                           14 Association Types
    Common Behaviour                   12 Association Types
    State Machines                     32 Association Types
    Activities                         34 Association Types
    Actions                           109 Association Types
    Interactions                       38 Association Types
    Use Cases                           8 Association Types
    Deployments                        10 Association Types
    Information Flows                   8 Association Types
    --------------------             ----
                                      418

    All the above are required for correct xmi generation.

    A class diagram can use everything up to Common Behaviour in the above list.
    Below that are other diagram types which may or may not use the above.

    Possible solution generate enumeration via XSLT from UML.xmi. Saves a lot
    of work and would enable easier migration to later versions.

    UML.xmi also contains field isDerived for a uml:Association.Need to investigate more.


    Will fix up ancestors when we attach to ModelEntity subclasses and have to
    worry about lifetimes, dangling pointers etc. Probably should have its own listeners heirarchy
    so it can remove references to itself.
  }
  TDirectedAssociation = class(TModelEntity)
    StereoType: string;            // works for now.
    OwnedEnd: TAssociationEnd;     // source in 7.2.3.3
    TargetEnd: TAssociationEnd;
    Constraint: TDocumentation;    // works for now. ModelEntity should really have this?
  end;

  TBindAssociation = class(TDirectedAssociation)
    Bindings:
  end;


implementation

end.


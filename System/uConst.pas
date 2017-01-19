{
  Laz-Model
  Copyright (C) 2002  Eldean AB, Peter Söderman, Ville Krumlinde
  Portions (C) 2016 Peter Dyson. Initial Lazarus port

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

unit uConst;

{$mode objfpc}{$H+}

interface

resourcestring
  // try this naming convention for starters
  // rs[a-z*]_ic  Initial Caps       = rsInitialCaps_ic
  // rs[a-z"]_ac  ALL CAPS           = rsAllCaps_ac
  // rs[a-z*]_lc  all lower case     = rsAllLowerCase_lc
  // this is mainly for developers to understand what type of string is being used.
  // this may not make sense in non european languages but should convey the
  // basic intent of the displayed string.
  // Placing all resource string here may help reuse and non duplication.

  {Tree View Integrator}
  rsPackages_ic = 'Packages';
  rsAllClasses_ic = 'All Classes';
  rsDependencies_lc = 'dependencies';
  rsDatatypes_lc = 'datatypes';
  rsInterfaces_lc = 'interfaces';
  rsSubclasses_lc = 'subclasses';
  rsImplementors_lc = 'implementors';
  rsAncestor_ic = 'Ancestor';

const
  ProgName = 'Laz-Model';
  ProgVersion = '2.2';

  ProgCopyright = 'Copyright (C) Eldean AB Sweden';
  ProgPorted = 'Ported for Lazarus by Peter Dyson';

  ProgUrl  = 'https://github.com/dicepd/Laz-Model'; // 'http://www.essmodel.com';
  ProgMail = 'mail address?'; // 'essmodel@eldean.se';


implementation

end.


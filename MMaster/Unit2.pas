{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TOptionsControl = class(TForm)
    OKBut: TButton;
    CancelBut: TButton;
    EditPlayerPath: TEdit;
    Label1: TLabel;
    EditFFPath: TEdit;
    Label2: TLabel;
  private
  public
  end;

var
  OptionsControl: TOptionsControl;

implementation

{$R *.DFM}

end.
 
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

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Mask, Dialogs;

type
  TEnterNewData = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    ElNumOfPoints: TEdit;
    Label3: TLabel;
    ElStep: TEdit;
    Label4: TLabel;
    ElSymbol: TEdit;
    ElFullName: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EnterNewData: TEnterNewData;

implementation

{$R *.DFM}

end.

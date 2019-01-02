{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit Unit3;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Dialogs;

type
  TInputWavelengthDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    WavelengthValueEdit: TEdit;
    Label1: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  InputWavelengthDlg: TInputWavelengthDlg;

implementation

{$R *.DFM}

procedure TInputWavelengthDlg.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var TempDouble : Double;
    SaveDecimalSeparator : Char;
begin
 if ModalResult = mrOk then
  begin
   SaveDecimalSeparator := DecimalSeparator;
   DecimalSeparator := '.';
   try
    TempDouble := StrToFloat(WavelengthValueEdit.Text);
   except
    MessageDlg('Invalid wavelength value...',mtError,[mbOk],0);
    ActiveControl := WavelengthValueEdit;
    CanClose := False;
    DecimalSeparator := SaveDecimalSeparator;
    Exit;
   end;
   DecimalSeparator := SaveDecimalSeparator;
   CanClose := True;
  end;{if ModalResult = mrOk then...}
end;

procedure TInputWavelengthDlg.FormActivate(Sender: TObject);
begin
 ActiveControl := WavelengthValueEdit;
end;

end.

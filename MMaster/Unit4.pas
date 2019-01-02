{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit Unit4;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls, Dialogs;

type
  TInputValueDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    InputValue: TEdit;
    Label1: TLabel;
    ApplyToAllCheck: TCheckBox;
    InputSiteIdentifier: TEdit;
    Label3: TLabel;
    EntirelyAtSite: TCheckBox;
    Bevel2: TBevel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ApplyToAllCheckClick(Sender: TObject);
  public
  end;

var
  InputValueDlg: TInputValueDlg;

implementation

{$R *.DFM}

procedure TInputValueDlg.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
Var TempDouble : Double;
begin
 if ModalResult = mrOk then
  try
   TempDouble := StrToFloat(InputValue.Text);
  except
   ActiveControl := InputValue;
   MessageDlg('This value must be numeric...',mtError,[mbOk],0);
   CanClose := False;
   Exit;
  end;
 CanClose := True;
end;

procedure TInputValueDlg.ApplyToAllCheckClick(Sender: TObject);
begin
 if ApplyToAllCheck.Checked then
  begin
   InputSiteIdentifier.Text := '';
   InputSiteIdentifier.Enabled := False;
   InputSiteIdentifier.Color := clBtnFace;
  end
 else
  begin
   InputSiteIdentifier.Text := '';
   InputSiteIdentifier.Enabled := True;
   InputSiteIdentifier.Color := clWindow;
  end;
end;

end.
 
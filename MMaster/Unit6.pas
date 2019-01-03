


{------------------------------------------------------------------------------}
{       Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)                  }
{------------------------------------------------------------------------------}
unit Unit6;

{$MODE Delphi}

interface

uses SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Dialogs, LResources;

type
  TSetMomentValueDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    InputSiteIdentifier: TEdit;
    Label1: TLabel;
    InputMomentValue: TEdit;
    Label2: TLabel;
    ApplyToAllCheck: TCheckBox;
    Bevel2: TBevel;
    procedure ApplyToAllCheckClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  public
  end;

var
  SetMomentValueDlg: TSetMomentValueDlg;

implementation

procedure TSetMomentValueDlg.ApplyToAllCheckClick(Sender: TObject);
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

procedure TSetMomentValueDlg.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
Var TempDouble : Double;
begin
 if ModalResult = mrOk then
  try
   TempDouble := StrToFloat(InputMomentValue.Text);
  except
   ActiveControl := InputMomentValue;
   MessageDlg('This value must be numeric...',mtError,[mbOk],0);
   CanClose := False;
   Exit;
  end;
 CanClose := True;
end;

initialization
{$I Unit6.lrs}
end.
 

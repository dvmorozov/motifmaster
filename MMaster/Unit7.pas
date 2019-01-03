


{------------------------------------------------------------------------------}
{       Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)                  }
{------------------------------------------------------------------------------}
unit Unit7;

{$MODE Delphi}

interface

uses SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, LResources;

type
  TSetMeanMomentValueDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    ApplyToAllCheck: TCheckBox;
    InputSiteIdentifier: TEdit;
    Label1: TLabel;
    Bevel2: TBevel;
    procedure ApplyToAllCheckClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SetMeanMomentValueDlg: TSetMeanMomentValueDlg;

implementation

procedure TSetMeanMomentValueDlg.ApplyToAllCheckClick(Sender: TObject);
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

initialization
{$I Unit7.lrs}
end.
 

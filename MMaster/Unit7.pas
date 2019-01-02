{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit Unit7;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls;

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

{$R *.DFM}

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

end.
 
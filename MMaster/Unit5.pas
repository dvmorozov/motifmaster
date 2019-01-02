{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit Unit5;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, DataClasses, Dialogs, StrConst, Tools;

type
  TEditPropVectorDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    EditZCoord: TEdit;
    Label16: TLabel;
    Label15: TLabel;
    EditYCoord: TEdit;
    EditXCoord: TEdit;
    Label14: TLabel;
    ButtonHelp: TButton;
    procedure OKBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  protected
    FEditedPropVector: TWaveVector;
    procedure SetEditedPropVector(const APropVector: TWaveVector);
  public
    property EditedPropVector: TWaveVector write SetEditedPropVector;
  end;

var
  EditPropVectorDlg: TEditPropVectorDlg;

implementation

{$R *.DFM}

procedure TEditPropVectorDlg.OKBtnClick(Sender: TObject);
begin
  if EditXCoord.Text = '' then begin
    ActiveControl := EditXCoord;
    MessageDlg(LoadStr(SERMustNotBeEmpty), mtError, [mbOk], 0);
    Exit;
  end;
  if EditYCoord.Text = '' then begin
    ActiveControl := EditYCoord;
    MessageDlg(LoadStr(SERMustNotBeEmpty), mtError, [mbOk], 0);
    Exit;
  end;
  if EditZCoord.Text = '' then begin
    ActiveControl := EditZCoord;
    MessageDlg(LoadStr(SERMustNotBeEmpty), mtError, [mbOk], 0);
    Exit;
  end;

  FEditedPropVector[1] := StrToFloatDef(EditXCoord.Text, 0);
  FEditedPropVector[2] := StrToFloatDef(EditYCoord.Text, 0);
  FEditedPropVector[3] := StrToFloatDef(EditZCoord.Text, 0);
end;

procedure TEditPropVectorDlg.SetEditedPropVector(const APropVector: TWaveVector);
begin
  FEditedPropVector := APropVector;
  EditXCoord.Text := FloatToStrF(FEditedPropVector[1], ffGeneral, 6, 3);
  EditYCoord.Text := FloatToStrF(FEditedPropVector[2], ffGeneral, 6, 3);
  EditZCoord.Text := FloatToStrF(FEditedPropVector[3], ffGeneral, 6, 3);
end;

procedure TEditPropVectorDlg.FormActivate(Sender: TObject);
begin
  ActiveControl := EditXCoord;
end;

end.
 
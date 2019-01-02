{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit Unit9;

interface

uses
    Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
    Buttons, ExtCtrls, DataClasses, Dialogs, StrConst, NumericGrid;

type
    TEditSiteDlg = class(TForm)
        OKBtn: TButton;
        CancelBtn: TButton;
        Bevel1: TBevel;
        Label17: TLabel;
        EditSiteName: TEdit;
        EditElement: TEdit;
        Label1: TLabel;
        EditNuclearScatAmpl: TEdit;
        Label2: TLabel;
        ButtonHelp: TButton;

        procedure OKBtnClick(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure EditNuclearScatAmplKeyPress(Sender: TObject; var Key: Char);

    protected
        FEditedSite: TSite;
        procedure SetEditedSite(const AEditedSite: TSite);

    public
        property EditedSite: TSite write SetEditedSite;
    end;

var
    EditSiteDlg: TEditSiteDlg;

implementation

{$R *.DFM}

procedure TEditSiteDlg.OKBtnClick(Sender: TObject);
begin
    if EditSiteName.Text = '' then
    begin
        ActiveControl := EditSiteName;
        MessageDlg(LoadStr(SERMustNotBeEmpty), mtError, [mbOk], 0);
        Exit;
    end;

    if EditElement.Text = '' then
    begin
        ActiveControl := EditElement;
        MessageDlg(LoadStr(SERMustNotBeEmpty), mtError, [mbOk], 0);
        Exit;
    end;

    try
        StrToFloat(EditNuclearScatAmpl.Text);
    except
        ActiveControl := EditNuclearScatAmpl;
        MessageDlg(LoadStr(SERInvalidInput), mtError, [mbOk], 0);
        Exit;
    end;

    with FEditedSite do
    begin
        SiteName := EditSiteName.Text;
        Element := EditElement.Text;
        NuclearScatAmpl := StrToFloat(EditNuclearScatAmpl.Text);
        ModalResult := mrOk;
    end;
end;

procedure TEditSiteDlg.SetEditedSite(const AEditedSite: TSite);
begin
    FEditedSite := AEditedSite;

    with FEditedSite do
    begin
        EditSiteName.Text := SiteName;
        EditElement.Text := Element;
        EditNuclearScatAmpl.Text := FloatToStr(NuclearScatAmpl);
    end;
end;

procedure TEditSiteDlg.FormActivate(Sender: TObject);
begin
    ActiveControl := EditSiteName;
end;

procedure TEditSiteDlg.EditNuclearScatAmplKeyPress(Sender: TObject;
    var Key: Char);
begin
    if not (Key in REAL_SET) then Key := #0;
end;

end.
 
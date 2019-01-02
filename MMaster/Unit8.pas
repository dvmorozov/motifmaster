{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit Unit8;

interface

uses
    Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
    Buttons, ExtCtrls, DataClasses, Tools;

type
  TSiteCalcOptDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    GroupBox1: TGroupBox;
    StaticSiteName: TStaticText;
    GroupBox2: TGroupBox;
    CheckMagnetic: TCheckBox;
    CheckTakingPart: TCheckBox;
    Button1: TButton;
    GroupBox3: TGroupBox;
    RadioByOne: TRadioButton;
    RadioByAll: TRadioButton;
    RadioDontUse: TRadioButton;
    GroupVariationMode: TGroupBox;
    RadioVaryAll: TRadioButton;
    RadioSaveEqual: TRadioButton;
    RadioVaryModules: TRadioButton;
    procedure OKBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RadioVaryAllClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);

  protected
    FEditedSite: TSite;
    FEditedSiteCopy: TSite;

    UpdatingAllowed: Boolean;

    procedure SetEditedSite(const AEditedSite: TSite);

    procedure SetPropertiesIntoForm;
    procedure GetPropertiesFromForm;

  public
    property EditedSite: TSite write SetEditedSite;
  end;

var
  SiteCalcOptDlg: TSiteCalcOptDlg;

implementation

{$R *.DFM}

procedure TSiteCalcOptDlg.SetPropertiesIntoForm;
var TempByte: Byte;
begin
    UpdatingAllowed := False;
    with FEditedSiteCopy do
    begin
        CheckMagnetic.Checked := not NotMagnetic;
        CheckTakingPart.Checked := not Disabled;
        StaticSiteName.Caption := SiteName;

        TempByte := AllowedVariationModes;
        if (TempByte and EVMT_ALL_COMPONENTS) <> 0 then
            RadioVaryAll.Enabled := True else RadioVaryAll.Enabled := False;
        if (TempByte and EVMT_EQUAL_MODULES) <> 0 then
            RadioSaveEqual.Enabled := True else RadioSaveEqual.Enabled := False;
        if (TempByte and EVMT_ONLY_MODULES) <> 0 then
            RadioVaryModules.Enabled := True else RadioVaryModules.Enabled := False;

        if TempByte = 0 then begin
            RadioVaryAll.Checked := False;
            RadioSaveEqual.Checked := False;
            RadioVaryModules.Checked := False;
        end else
            case VariationMode of
                VM_ALL_COMPONENTS : RadioVaryAll.Checked := True;
                VM_EQUAL_MODULES : RadioSaveEqual.Checked := True;
                VM_ONLY_MODULES : RadioVaryModules.Checked := True;
            end;

        TempByte := AllowedReprModes;
        if (TempByte and ERMT_BY_ONE) <> 0 then
            RadioByOne.Enabled := True else RadioByOne.Enabled := False;
        if (TempByte and ERMT_BY_ALL) <> 0 then
            RadioByAll.Enabled := True else RadioByAll.Enabled := False;
        if (TempByte and ERMT_DONT_USE) <> 0 then
            RadioDontUse.Enabled := True else RadioDontUse.Enabled := False;

        if TempByte = 0 then begin
            RadioByOne.Checked := False;
            RadioByAll.Checked := False;
            RadioDontUse.Checked := False;
        end else
            case ReprMode of
                RM_BY_ONE : RadioByOne.Checked := True;
                RM_BY_ALL : RadioByAll.Checked := True;
                RM_DONT_USE : RadioDontUse.Checked := True;
            end;
    end;
    UpdatingAllowed := True;
end;

procedure TSiteCalcOptDlg.SetEditedSite(const AEditedSite: TSite);
begin
    FEditedSite := AEditedSite;
    UtilizeObject(FEditedSiteCopy);
    FEditedSiteCopy := TSite(AEditedSite.GetCopy);
    SetPropertiesIntoForm;
end;

procedure TSiteCalcOptDlg.OKBtnClick(Sender: TObject);
begin
    if UpdatingAllowed then begin
        GetPropertiesFromForm;
        FEditedSiteCopy.CopyParameters(FEditedSite);
    end
end;

procedure TSiteCalcOptDlg.GetPropertiesFromForm;
begin
    with FEditedSiteCopy do
    begin
        NotMagnetic := not CheckMagnetic.Checked;
        Disabled := not CheckTakingPart.Checked;

        if RadioByOne.Checked then ReprMode := RM_BY_ONE else
            if RadioByAll.Checked then ReprMode := RM_BY_ALL else
                if RadioDontUse.Checked then ReprMode := RM_DONT_USE;

        if RadioVaryAll.Checked then VariationMode := VM_ALL_COMPONENTS else
            if RadioSaveEqual.Checked then VariationMode := VM_EQUAL_MODULES else
                if RadioVaryModules.Checked then VariationMode := VM_ONLY_MODULES;
    end;
end;

procedure TSiteCalcOptDlg.FormDestroy(Sender: TObject);
begin
    UtilizeObject(FEditedSiteCopy);
end;

procedure TSiteCalcOptDlg.RadioVaryAllClick(Sender: TObject);
begin
    if UpdatingAllowed then begin
        GetPropertiesFromForm;
        SetPropertiesIntoForm;
    end;
end;

procedure TSiteCalcOptDlg.FormShow(Sender: TObject);
begin
    ActiveControl := OkBtn;
end;

procedure TSiteCalcOptDlg.Button1Click(Sender: TObject);
begin
    Application.HelpJump('hlp_SitesCalcOptDlg');
end;

end.
 
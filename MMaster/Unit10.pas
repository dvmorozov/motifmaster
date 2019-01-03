//      двойной косой чертой комментируются замечания, сохраняемые во
//      всех версиях исходника; фигурными скобками комментируются замечания,
//      сохраняемые только в версии исходника для бесплатного распространения
{------------------------------------------------------------------------------}
{       Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)                  }
{------------------------------------------------------------------------------}
unit Unit10;

{$MODE Delphi}

interface

uses
    Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
    Buttons, ExtCtrls, CheckLst, DataClasses, SimpMath, Dialogs,
    StrConst, Tools, MaskEdit, LResources, HelpIntfs;

type
  TPropVectCalcOptDlg = class(TForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    Bevel1: TBevel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox4: TGroupBox;
    StaticSiteIdentifier: TStaticText;
    StaticParameters: TStaticText;
    Label1: TLabel;
    Label2: TLabel;
    CheckMagnetic: TCheckBox;
    RadioX: TRadioButton;
    RadioY: TRadioButton;
    RadioZ: TRadioButton;
    ButtonHelp: TButton;
    GroupBox3: TGroupBox;
    RadioOther: TRadioButton;
    EditAxisDirection: TMaskEdit;
    Label3: TLabel;
    ComboStructureType: TComboBox;
    CheckStructural: TCheckBox;
    RadioOneArm: TRadioButton;
    RadioTwoArms: TRadioButton;
    Label4: TLabel;
    ButtonApply: TButton;
    procedure ButtonOKClick(Sender: TObject);
    procedure RadioOtherClick(Sender: TObject);
    procedure RadioXClick(Sender: TObject);
    procedure ComboStructureTypeChange(Sender: TObject);
    procedure ButtonHelpClick(Sender: TObject);
    procedure CheckMagneticClick(Sender: TObject);
    procedure RadioOneArmClick(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  protected
    FEditedPropVector: TWaveVector;
    FEditedPropVectorCopy: TWaveVector;
    (*копия !!! редактируемого вектора; используется для временного
    хранения сделанных изменений*)

    UpdatingAllowed: Boolean;

    procedure SetEditedPropVector(const APropVector: TWaveVector);
    (*заполнение формы параметрами редактируемого вектора*)
    procedure SetSite(const ASite: TSite);

    procedure SetRotationAxis;          (*ось вращ.: вектор -> форма*)
    function GetRotationAxis: Boolean;  (*ось вращ.: форма -> вектор*)

    procedure SetTransformType;
    function GetTransformType: Boolean;

    procedure SetStarType;
    function GetStarType: Boolean;

    procedure FillComboStructureType;
    procedure SetPropertiesIntoForm;    (*заполняет форму в соответствии со
                                        свойствами копии*)

  public
    destructor Destroy; override;

    property EditedPropVector: TWaveVector write SetEditedPropVector;
    property Site: TSite write SetSite;
  end;

var
  PropVectCalcOptDlg: TPropVectCalcOptDlg;

implementation

procedure TPropVectCalcOptDlg.SetEditedPropVector(const APropVector: TWaveVector);
var SavedCopyingMode: LongInt;
begin
    FEditedPropVector := APropVector;
    UtilizeObject(FEditedPropVectorCopy);

    SavedCopyingMode := APropVector.GetSelfCopyingMode;
    APropVector.SetSelfCopyingMode(SSCOPY_WITHOUT_REPR);
    FEditedPropVectorCopy := TWaveVector(APropVector.GetCopy);
    APropVector.SetSelfCopyingMode(SavedCopyingMode);

    with FEditedPropVectorCopy do StaticParameters.Caption := AsString;

    SetPropertiesIntoForm;
end;

procedure TPropVectCalcOptDlg.SetSite(const ASite: TSite);
begin
     StaticSiteIdentifier.Caption := ASite.SiteName;
end;

procedure TPropVectCalcOptDlg.SetPropertiesIntoForm;
var TempByte: Byte;
begin
     (*запрещает обратное изменение параметров копии в ответ на
     первоначальное изменение элементов управления формы*)
     UpdatingAllowed := False;
     with FEditedPropVectorCopy do begin
          CheckMagnetic.Checked := IsPropVectMagnetic;
          CheckStructural.Checked := IsPropVectStructural;

          TempByte := AllowedPropVectorTypes;
          if TempByte and PVT_STRUCTURAL <> 0 then
             CheckStructural.Enabled := True
          else CheckStructural.Enabled := False;

          if TempByte and PVT_MAGNETIC <> 0 then
             CheckMagnetic.Enabled := True
          else CheckMagnetic.Enabled := False;

          SetRotationAxis;
          SetTransformType;
          SetStarType;
     end;
     UpdatingAllowed := True;
end;

procedure TPropVectCalcOptDlg.ButtonOKClick(Sender: TObject);
begin
     FEditedPropVectorCopy.SetSelfCopyingMode(SSCOPY_WITHOUT_REPR);
     FEditedPropVectorCopy.CopyParameters(FEditedPropVector);

     ModalResult := mrOk;
end;

procedure TPropVectCalcOptDlg.SetRotationAxis;
var TempByte: Byte;
begin
     with FEditedPropVectorCopy do begin
          TempByte := AllowedRotAxis;
          if TempByte and ALLOWED_AXIS_X <> 0 then RadioX.Enabled := True
          else RadioX.Enabled := False;

          if TempByte and ALLOWED_AXIS_Y <> 0 then RadioY.Enabled := True
          else RadioY.Enabled := False;

          if TempByte and ALLOWED_AXIS_Z <> 0 then RadioZ.Enabled := True
          else RadioZ.Enabled := False;

          if TempByte and ALLOWED_AXIS_ANY <> 0 then RadioOther.Enabled := True
          else RadioOther.Enabled := False;

          case RotAxisType of
               RA_NONE :
               begin
                    RadioX.Checked := False;
                    RadioY.Checked := False;
                    RadioZ.Checked := False;
                    RadioOther.Checked := False;
               end;
               RA_X : RadioX.Checked := True;
               RA_Y : RadioY.Checked := True;
               RA_Z : RadioZ.Checked := True;
               RA_OTHER : RadioOther.Checked := True;
          end;

          case RotAxisType of
               RA_X, RA_Y, RA_Z, RA_NONE :
               begin
                    EditAxisDirection.Enabled := False;
                    ButtonApply.Enabled := False;
                    EditAxisDirection.Color := clBtnFace;
                    EditAxisDirection.Text := '';
                    EditAxisDirection.EditMask := '';
               end;
               RA_OTHER :
               begin
                    EditAxisDirection.Enabled := True;
                    ButtonApply.Enabled := True;
                    EditAxisDirection.Color := clWindow;
                    EditAxisDirection.EditMask := DoubleVector3EditMask;
                    with FEditedPropVectorCopy do
                         EditAxisDirection.Text := DoubleVector3AsString(RotAxis, True, 5, 4);
                    ActiveControl := EditAxisDirection;
               end;
          end;
     end;
end;

function TPropVectCalcOptDlg.GetRotationAxis: Boolean;
var TempRotAxis: TDoubleVector3;
begin
     Result := True;
     with FEditedPropVectorCopy do begin
          if RadioX.Checked then begin
             TempRotAxis[1] := 1; TempRotAxis[2] := 0; TempRotAxis[3] := 0;
             RotAxis := TempRotAxis;
             Exit
          end;

          if RadioY.Checked then begin
             TempRotAxis[1] := 0; TempRotAxis[2] := 1; TempRotAxis[3] := 0;
             RotAxis := TempRotAxis;
             Exit
          end;

          if RadioZ.Checked then begin
             TempRotAxis[1] := 0; TempRotAxis[2] := 0; TempRotAxis[3] := 1;
             RotAxis := TempRotAxis;
             Exit
          end;

          if RadioOther.Checked then begin
             try
                RotAxis := StringAsDoubleVector3(EditAxisDirection.Text);
             except
                MessageDlg(LoadStr(SERInvalidInput), mtError, [mbOk], 0);
                ActiveControl := EditAxisDirection;
                Result := False;
             end;
          end;
     end;
end;

procedure TPropVectCalcOptDlg.FillComboStructureType;
var i: LongInt;
begin
     with FEditedPropVectorCopy, ComboStructureType do begin
          Items.Clear;
          for i := 0 to AllowedTransTypesNumber - 1 do
              case AllowedTransTypes[i] of
                   TT_SS : Items.Add('Simple / FM Spiral');
                   TT_LSW : Items.Add('Long. / Trans. Spin Wave');
                   TT_CS : Items.Add('Complex Spiral');
                   TT_ES : Items.Add('Elliptic Spiral');
              else Items.Add('Unknown Type');
          end;
     end;
end;

procedure TPropVectCalcOptDlg.SetTransformType;
begin
     FillComboStructureType;
     with FEditedPropVectorCopy, ComboStructureType do
          ItemIndex := TransTypeIndex;
end;

function TPropVectCalcOptDlg.GetTransformType: Boolean;
begin
     Result := True;
      with FEditedPropVectorCopy, ComboStructureType do
           TransTypeIndex := ItemIndex;
end;

procedure TPropVectCalcOptDlg.SetStarType;
var TempByte: Byte;
begin
     with FEditedPropVectorCopy do begin
          TempByte := StarType;
          RadioOneArm.Checked := False;
          RadioTwoArms.Checked := False;

          TempByte := AllowedStarTypes;
          if TempByte and ST_ONE_ARM <> 0 then RadioOneArm.Enabled := True
          else RadioOneArm.Enabled := False;

          if TempByte and ST_TWO_ARMS <> 0 then RadioTwoArms.Enabled := True
          else RadioTwoArms.Enabled := False;

          TempByte := StarType;
          if TempByte = ST_ONE_ARM then RadioOneArm.Checked := True
          else if TempByte = ST_TWO_ARMS then RadioTwoArms.Checked := True;
     end;
end;

function TPropVectCalcOptDlg.GetStarType: Boolean;
begin
     Result := True;
     with FEditedPropVectorCopy do begin
          if RadioOneArm.Checked then StarType := ST_ONE_ARM
          else StarType := ST_TWO_ARMS;
     end;
end;

procedure TPropVectCalcOptDlg.RadioOtherClick(Sender: TObject);
begin
     if UpdatingAllowed then begin
        EditAxisDirection.Enabled := True;
        ButtonApply.Enabled := True;
        EditAxisDirection.Color := clWindow;
        EditAxisDirection.EditMask := DoubleVector3EditMask;
        with FEditedPropVectorCopy do
             EditAxisDirection.Text := DoubleVector3AsString(RotAxis, True, 5, 4);
        ActiveControl := EditAxisDirection;
     end;
end;

procedure TPropVectCalcOptDlg.RadioXClick(Sender: TObject);
begin
     if UpdatingAllowed then begin
        GetRotationAxis;
        SetPropertiesIntoForm;
     end;
end;

procedure TPropVectCalcOptDlg.ComboStructureTypeChange(Sender: TObject);
begin
     if UpdatingAllowed then begin
        GetTransformType;
        SetPropertiesIntoForm;
     end;
end;

procedure TPropVectCalcOptDlg.ButtonHelpClick(Sender: TObject);
begin
    ShowHelpOrErrorForKeyword('','HTML/prop_vect_calc_opt.html');
end;

procedure TPropVectCalcOptDlg.CheckMagneticClick(Sender: TObject);
begin
     if UpdatingAllowed then begin
        with FEditedPropVectorCopy do begin
             if CheckMagnetic.Checked then
                PropVectorType := PropVectorType or PVT_MAGNETIC
             else PropVectorType := PropVectorType and not PVT_MAGNETIC;

             if CheckStructural.Checked then
                PropVectorType := PropVectorType or PVT_STRUCTURAL
             else PropVectorType := PropVectorType and not PVT_STRUCTURAL;
        end;

        SetPropertiesIntoForm;
     end;
end;

procedure TPropVectCalcOptDlg.RadioOneArmClick(Sender: TObject);
begin
     if UpdatingAllowed then begin
        GetStarType;
        SetPropertiesIntoForm;
     end;
end;

procedure TPropVectCalcOptDlg.ButtonApplyClick(Sender: TObject);
begin
     if UpdatingAllowed then
        if GetRotationAxis then SetPropertiesIntoForm;
end;

destructor TPropVectCalcOptDlg.Destroy;
begin
    UtilizeObject(FEditedPropVectorCopy);
    inherited;
end;

procedure TPropVectCalcOptDlg.FormShow(Sender: TObject);
begin
     ActiveControl := ButtonOK;
     (*это сделано, чтобы избежать вызова обработчика OnClick
     активного элемента при отображении формы; при нажатии кнопки [X]
     активный элемент сохраняется, и его обработчик снова вызывается,
     когда форма отображается и UpdatingAllowed уже = True, хотя
     никакого действия пользователя еще не было*)
end;

initialization
{$I Unit10.lrs}
end.
 

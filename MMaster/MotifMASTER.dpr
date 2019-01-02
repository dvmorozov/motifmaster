//      двойной косой чертой комментируются замечания, сохраняемые во
//      всех версиях исходника; фигурными скобками комментируются замечания,
//      сохраняемые только в версии исходника для бесплатного распространения
{------------------------------------------------------------------------------}
{       Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)                  }
{------------------------------------------------------------------------------}
program MotifMASTER;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {OptionsControl},
  Unit4 in 'Unit4.pas' {InputValueDlg},
  Unit6 in 'Unit6.pas' {SetMomentValueDlg},
  Unit7 in 'Unit7.pas' {SetMeanMomentValueDlg},
  Main in 'Main.pas' {HiddenForm},
  ComponentList in '..\Library\Common\ComponentList.pas',
  DownhillSimplexAlgorithm in '..\Library\Algorithms\DownhillSimplexAlgorithm.pas',
  TableComp in '..\Library\Common\TableComp.pas',
  Tools in '..\Library\Tools\Tools.pas',
  CalcModule in '..\Library\MMaster\CalcModule.pas',
  Plotter2 in '..\Library\MMaster\Plotter2.pas',
  DataClasses in '..\Library\MMaster\DataClasses.pas',
  SourceFile in '..\Library\MMaster\SourceFile.pas',
  StrConst in '..\Library\MMaster\StrConst.pas',
  SyntMessages in '..\Library\MMaster\SyntMessages.pas',
  Math3d in '..\Library\Math\Math3d.pas',
  SimpMath in '..\Library\Math\SimpMath.pas',
  FFDataFile in '..\Library\FFeditor\FFDataFile.pas',
  AlgorithmContainer in '..\Library\Algorithms\AlgorithmContainer.pas',
  Runner in '..\Library\Common\Runner.pas',
  Unit9 in 'Unit9.pas' {EditSiteDlg},
  Unit5 in 'Unit5.pas' {EditPropVectorDlg},
  Unit8 in 'Unit8.pas' {SiteCalcOptDlg},
  Unit10 in 'Unit10.pas' {PropVectCalcOptDlg},
  DownhillSimplexContainer in '..\Library\Algorithms\DownhillSimplexContainer.pas',
  CombEnumerator in '..\Library\Algorithms\CombEnumerator.pas',
  Algorithm in '..\Library\Algorithms\Algorithm.pas',
  Decisions in '..\Library\Algorithms\Decisions.pas',
  FocusPanel in '..\Library\NewVCL\FocusPanel.pas',
  Unit11 in 'Unit11.pas' {StructViewPropDlg},
  SelfCopied in '..\Library\Common\SelfCopied.pas',
  ObjSavingStringList in '..\Library\Common\ObjSavingStringList.pas',
  Plotter in '..\Library\MMaster\Plotter.pas',
  NumericGrid in '..\Library\NewVCL\NumericGrid.pas',
  Unit12 in 'Unit12.pas' {AboutBox},
  CBRCComponent in '..\Library\Common\CBRCComponent.pas',
  SelfSaved in '..\Library\Common\SelfSaved.pas',
  ClassInheritIDs in '..\Library\MMaster\ClassInheritIDs.pas';

{$R *.res}

begin
  Application.CreateForm(THiddenForm, HiddenForm);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TOptionsControl, OptionsControl);
  Application.CreateForm(TInputValueDlg, InputValueDlg);
  Application.CreateForm(TSetMomentValueDlg, SetMomentValueDlg);
  Application.CreateForm(TSetMeanMomentValueDlg, SetMeanMomentValueDlg);
  Application.CreateForm(TEditSiteDlg, EditSiteDlg);
  Application.CreateForm(TEditPropVectorDlg, EditPropVectorDlg);
  Application.CreateForm(TSiteCalcOptDlg, SiteCalcOptDlg);
  Application.CreateForm(TPropVectCalcOptDlg, PropVectCalcOptDlg);
  Application.CreateForm(TStructViewPropDlg, StructViewPropDlg);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.ShowMainForm := False;
  Application.Run;
end.

program MotifMASTER;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, Algorithm, AlgorithmContainer, CalcModule,
  CBRCComponent, ClassInheritIDs, CombEnumerator, ComponentList, DataClasses,
  Decisions, DownhillSimplexAlgorithm, DownhillSimplexContainer, FFDataFile,
  Math3d, NumericGrid, ObjSavingStringList, Plotter2, Plotter,
  Runner, SelfCopied, SelfSaved, SimpMath, SourceFile, StrConst,
  TableComp, Tools, Unit12, Unit1, Unit2, Unit4, Unit5, Unit6, Unit7, Unit8,
  Unit9, Unit10, Unit11, Dclusr40, MyExceptions;

{$R MotifMASTER.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TEditPropVectorDlg, EditPropVectorDlg);
  Application.CreateForm(TEditSiteDlg, EditSiteDlg);
  Application.CreateForm(TInputValueDlg, InputValueDlg);
  Application.CreateForm(TOptionsControl, OptionsControl);
  Application.CreateForm(TPropVectCalcOptDlg, PropVectCalcOptDlg);
  Application.CreateForm(TSetMeanMomentValueDlg, SetMeanMomentValueDlg);
  Application.CreateForm(TSetMomentValueDlg, SetMomentValueDlg);
  Application.CreateForm(TSiteCalcOptDlg, SiteCalcOptDlg);
  Application.CreateForm(TStructViewPropDlg, StructViewPropDlg);
  Application.Run;
end.


program IIMaker;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  DataLoader in 'DataLoader.pas',
  PointSetViewer in 'PointSetViewer.pas',
  IntegralInt in 'IntegralInt.pas',
  Unit3 in 'Unit3.pas' {InputWavelengthDlg},
  TableComp in '..\..\Library\Common\TableComp.pas',
  ComponentList in '..\..\Library\Common\ComponentList.pas',
  FFDataFile in '..\..\Library\FFeditor\FFDataFile.pas',
  NumericGrid in '..\..\Library\NewVCL\NumericGrid.pas',
  Math3d in '..\..\Library\Math\Math3d.pas',
  Tools in '..\..\Library\Tools\Tools.pas',
  DataClasses in '..\..\Library\MMaster\DataClasses.pas',
  DownhillSimplexContainer in '..\..\Library\Algorithms\DownhillSimplexContainer.pas',
  DownhillSimplexAlgorithm in '..\..\Library\Algorithms\DownhillSimplexAlgorithm.pas',
  Decisions in '..\..\Library\Algorithms\Decisions.pas',
  Algorithm in '..\..\Library\Algorithms\Algorithm.pas',
  AlgorithmContainer in '..\..\Library\Algorithms\AlgorithmContainer.pas',
  Runner in '..\..\Library\Common\Runner.pas',
  CombEnumerator in '..\..\Library\Algorithms\CombEnumerator.pas',
  SimpMath in '..\..\Library\Math\SimpMath.pas',
  SelfCopied in '..\..\Library\Common\SelfCopied.pas',
  ObjSavingStringList in '..\..\Library\Common\ObjSavingStringList.pas',
  Plotter2 in '..\..\Library\MMaster\Plotter2.pas',
  Unit12 in 'Unit12.pas' {AboutBox},
  CBRCComponent in '..\..\Library\Common\CBRCComponent.pas',
  SelfSaved in '..\..\Library\Common\SelfSaved.pas',
  ClassInheritIDs in '..\..\Library\MMaster\ClassInheritIDs.pas',
  Minimizer_S in 'Minimizer_S.pas',
  Minimizer in 'Minimizer.pas',
  MainCalcProcess in 'MainCalcProcess.pas',
  MSCRDataClasses in 'MSCRDataClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'IIMAK';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TInputWavelengthDlg, InputWavelengthDlg);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.

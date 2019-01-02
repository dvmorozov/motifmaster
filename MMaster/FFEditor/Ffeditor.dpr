program FFEditor;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {EnterNewData},
  FFDataFile in '..\..\Library\FFeditor\FFDataFile.pas',
  ComponentList in '..\..\Library\Common\ComponentList.pas',
  DataClasses in '..\..\Library\MMaster\DataClasses.pas',
  NumericGrid in '..\..\Library\NewVCL\NumericGrid.pas',
  Tools in '..\..\Library\Tools\Tools.pas',
  SyntMessages in '..\..\Library\MMaster\SyntMessages.pas',
  SimpMath in '..\..\Library\Math\SimpMath.pas',
  Math3d in '..\..\Library\Math\Math3d.pas',
  TableComp in '..\..\Library\Common\TableComp.pas',
  SelfCopied in '..\..\Library\Common\SelfCopied.pas',
  DownhillSimplexContainer in '..\..\Library\Algorithms\DownhillSimplexContainer.pas',
  DownhillSimplexAlgorithm in '..\..\Library\Algorithms\DownhillSimplexAlgorithm.pas',
  Decisions in '..\..\Library\Algorithms\Decisions.pas',
  Algorithm in '..\..\Library\Algorithms\Algorithm.pas',
  AlgorithmContainer in '..\..\Library\Algorithms\AlgorithmContainer.pas',
  Runner in '..\..\Library\Common\Runner.pas',
  CombEnumerator in '..\..\Library\Algorithms\CombEnumerator.pas',
  ObjSavingStringList in '..\..\Library\Common\ObjSavingStringList.pas',
  Plotter2 in '..\..\Library\MMaster\Plotter2.pas',
  CBRCComponent in '..\..\Library\Common\CBRCComponent.pas',
  SelfSaved in '..\..\Library\Common\SelfSaved.pas',
  ClassInheritIDs in '..\..\Library\MMaster\ClassInheritIDs.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Form - Factors Editor';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TEnterNewData, EnterNewData);
  Application.Run;
end.

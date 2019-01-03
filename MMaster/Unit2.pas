


{------------------------------------------------------------------------------}
{       Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)                  }
{------------------------------------------------------------------------------}
unit Unit2;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons,LResources
  ;

type

  { TOptionsControl }

  TOptionsControl = class(TForm)
    OKBut: TButton;
    CancelBut: TButton;
    EditPlayerPath: TEdit;
    Label1: TLabel;
    EditFFPath: TEdit;
    Label2: TLabel;
    procedure OKButClick(Sender: TObject);
  private
  public
  end;

var
  OptionsControl: TOptionsControl;

implementation

{ TOptionsControl }

procedure TOptionsControl.OKButClick(Sender: TObject);
begin

end;

initialization
{$I Unit2.lrs}
end.
 

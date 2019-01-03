


{------------------------------------------------------------------------------}
{       Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)                  }
{------------------------------------------------------------------------------}
unit Unit12;

{$MODE Delphi}

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, LResources, HelpIntfs;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    Panel1: TPanel;
    OKButton: TButton;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText6: TStaticText;
    StaticText8: TStaticText;
    StaticText9: TStaticText;
    StaticText5: TStaticText;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation

initialization
{$I Unit12.lrs}
end.
 
 

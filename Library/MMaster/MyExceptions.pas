{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}
unit MyExceptions;

{$MODE Delphi}

interface

uses
    Classes, SysUtils;

type
    EMyException = class(Exception);        //  для отличия исключений программы
                                            //  от исключений среды

implementation

initialization
end.


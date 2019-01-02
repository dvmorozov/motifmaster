{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit Algorithm;

interface

uses Classes;

type
    TAlgorithm = class(TComponent)
    public
        procedure AlgorithmRealization; virtual; abstract;
    end;


implementation

initialization
    RegisterClass(TAlgorithm);
end.

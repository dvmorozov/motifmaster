{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit ClassInheritIDs;

interface

const
    SelfSavedInheritID: Byte = 0;   SelfSavedCurVerNum: Byte = 0;

        RSDClassInheritID: Byte = 1;    RSDCurVerNum: Byte = 0;

            RSDAlClassInheritID: Byte = 2;  RSDAlCurVerNum: Byte = 0;

        RCDClassInheritID: Byte = 1;    RCDCurVerNum: Byte = 0;

            RCDAlClassInheritID: Byte = 2;  RCDAlCurVerNum: Byte = 0;

        RUClassInheritID: Byte = 1;     RUCurVerNum: Byte = 0;

            RUAlClassInheritID: Byte = 2;   RUAlCurVerNum: Byte = 0;

        DSDClassInheritID: Byte = 1;    DSDCurVerNum: Byte = 0;

            DSDAlClassInheritID: Byte = 2;  DSDAlCurVerNum: Byte = 0;

        DCDClassInheritID: Byte = 1;    DCDCurVerNum: Byte = 0;

            DCDAlClassInheritID: Byte = 2;  DCDAlCurVerNum: Byte = 0;

        DUClassInheritID: Byte = 1;     DUCurVerNum: Byte = 0;

            DUAlClassInheritID: Byte = 2;   DUAlCurVerNum: Byte = 0;

        CCClassInheritID: Byte = 1;     CCClassCurVerNum: Byte = 0;

            CCAlClassInheritID: Byte = 2;   CCAlCurVerNum: Byte = 0;

        CBRCClassInheritID: Byte = 1;   CBRCCurVerNum: Byte = 0;

            CLClassInheritID: Byte = 2;     CLCurVerNum: Byte = 0;

                SFClassInheritID: Byte = 3;     SFCurVerNum: Byte = 0;

                    SFAlClassInheritID: Byte = 4;   SFAlCurVerNum: Byte = 0;

                HKLLClassInheritID: Byte = 3;   HKLLCurVerNum: Byte = 0;

                SSClassInheritID: Byte = 3;     SSCurVerNum: Byte = 0;

                SCCLClassInheritID: Byte = 3;   SCCLCurVerNum: Byte = 0;

                    SCCLAlClassInheritID: Byte = 4; SCCLAlCurVerNum: Byte = 0;

                    OSSLClassInheritID: Byte = 4;   OSSLCurVerNum: Byte = 0;

                        SLClassInheritID: Byte = 5;     SLCurVerNum: Byte = 0;

                            SLAlClassInheritID: Byte = 6;   SLAlCurVerNum: Byte = 0;

                        WVLClassInheritID: Byte = 5;    WVLCurVerNum: Byte = 0;

                            WVLAlClassInheritID: Byte = 6;  WVLAlCurVerNum: Byte = 0;

                        RLClassInheritID: Byte = 5;     RLCurVerNum: Byte = 0;

                            RLAlClassInheritID: Byte = 6;   RLAlCurVerNum: Byte = 0;

                    TCLClassInheritID: Byte = 4;    TCLCurVerNum: Byte = 0;

                        RCLClassInheritID: Byte = 7;    RCLCurVerNum: Byte = 0;

                            ALClassInheritID: Byte = 5;     ALCurVerNum: Byte = 0;

                                ALAlClassInheritID: Byte = 6;   ALAlCurVerNum: Byte = 0;

                            NCLClassInheritID: Byte = 5;    NCLCurVerNum: Byte = 0;

                                NCLAlClassInheritID: Byte = 6;  NCLAlCurVerNum: Byte = 0;

                                NCLMClassInheritID: Byte = 6;   NCLMCurVerNum: Byte = 0;

                                    NCLMAlClassInheritID: Byte = 7; NCLMAlCurVerNum: Byte = 0;

                        STCLClassInheritID: Byte = 5;   STCLCurVerNum: Byte = 0;

                            STCLAlClassInheritID: Byte = 6; STCLAlCurVerNum: Byte = 0;

                        CCLClassInheritID: Byte = 7;    CCLCurVerNum: Byte = 0;

                            RClassInheritID: Byte = 5;      RCurVerNum: Byte = 2;

                                RAlClassInheritID: Byte = 6;    RAlCurVerNum: Byte = 0;

            DRPClassInheritID: Byte = 2;    DRPCurVerNum: Byte = 0;

                SClassInheritID: Byte = 3;      SCurVerNum: Byte = 1;

                    SAlClassInheritID: Byte = 4;    SAlCurVerNum: Byte = 0;

                WVClassInheritID: Byte = 3;     WVCurVerNum: Byte = 0;

                    WVAlClassInheritID: Byte = 4;   WVAlCurVerNum: Byte = 0;

            SCCClassInheritID: Byte = 2;    SCCCurVerNum: Byte = 0;

                AClassInheritID: Byte = 3;      ACurVerNum: Byte = 0;

                    AAlClassInheritID: Byte = 4;    AAlCurVerNum: Byte = 0;

                NCClassInheritID: Byte = 3;     NCCurVerNum: Byte = 1;

                    NCAlClassInheritID: Byte = 4;   NCAlCurVerNum: Byte = 0;

                NCMClassInheritID: Byte = 3;    NCMCurVerNum: Byte = 0;

                    NCMAlClassInheritID: Byte = 4;  NCMAlCurVerNum: Byte = 0;

                PPCClassInheritID: Byte = 3;    PPCCurVerNum: Byte = 0;

                    PPCAlClassInheritID: Byte = 4;  PPCAlCurVerNum: Byte = 0;

                HKLClassInheritID: Byte = 3;    HKLCurVerNum: Byte = 0;

                    HKLAlClassInheritID: Byte = 4;  HKLAlCurVerNum: Byte = 0;

                STClassInheritID: Byte = 3;     STCurVerNum: Byte = 0;

                    STAlClassInheritID: Byte = 4;   STAlCurVerNum: Byte = 0;

                GClassInheritID: Byte = 3;      GCurVerNum: Byte = 0;

                    GAlClassInheritID: Byte = 4;    GAlCurVerNum: Byte = 0;

                CRClassInheritID: Byte = 3;     CRCurVerNum: Byte = 0;

                    CRAlClassInheritID: Byte = 4;   CRAlCurVerNum: Byte = 0;

                BFClassInheritID: Byte = 3;     BFCurVerNum: Byte = 0;

                    BFAlClassInheritID: Byte = 4;   BFAlCurVerNum: Byte = 0;

                SPPClassInheritID: Byte = 3;    SPPCurVerNum: Byte = 0;

                    SPPAlClassInheritID: Byte = 4;  SPPAlCurVerNum: Byte = 0;

                SLPPClassInheritID: Byte = 3;   SLPPCurVerNum: Byte = 0;

                    SLPPAlClassInheritID: Byte = 4; SLPPAlCurVerNum: Byte = 0;

            V3DCClassInheritID: Byte = 2;   V3DCurVerNum: Byte = 0;

                CV3DCClassInheritID: Byte = 3;  CV3DCurVerNum: Byte = 0;

implementation
end.

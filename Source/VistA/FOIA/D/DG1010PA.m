DG1010PA ;ALB/REW - 1010 PRINT--INQUIRY PATIENT -ADDITIONL ; 28-MAY-93
 ;;5.3;Registration;**18,28,86,108,113,570,624**;Aug 13, 1993
 ;
NOREG(DFN) ; DOES PROMPTS FOR 10/10 PRINT W/O REGISTRATION
 ;INPUT: DFN
 ;OUTPUT: VARIABLES NEEDED FOR DIFFERENT PRINTOUTS
 ;   DGMTYPT - MT=1 Copay=2 None=0
 ;   DGOPT   - WHICH REPORTS TO PRINT
 ;   DGPMDA  - Admissions Report Info
 ;   PRF     - FLAG FOR RX PROFILE
 ;   PSOINST - STATION NUMBER (INSTITUTION FILE) FOR ACTION PROFILE PRINT
 ;   PSONOPG - USED FOR PRINTING ACTION PROFILE (SET TO 1)
 ;   PSOPAR  - PRINT BARCODES FOR ACTION PROFILE (SET TO 1)
 ;   PSTYPE  - TYPE OF DRUG PROFILE
 ;   GMTSTYP - TYPE OF HEALTH SUMMARY
 ;   EASMTIEN - Means Test IEN used for EZ/EZR
 ;
 ;
 N DG1,I,X,Y,FORM
 S FREE=+$P(^DG(43,1,0),U,8),DGOPT=""
 S PRF=0 G QTNOREG:'$D(^DG(43,1,0)) ;NEED MAS PARAMETERS TO CONTINUE
 I $$PROMPRN("THIRD") G:DG1<0 QTNOREG S DGOPT=DGOPT_3
 S FORM=$$SEL1010^DG1010P()
 S EASMTIEN=$$MTPRMPT^DG1010P(DFN,$G(DGMTI))
 I FORM<0 S DG1=-1 G QTNOREG
 I FORM="EZ" S DGOPT=DGOPT_0
 I FORM="EZR" S DGOPT=DGOPT_1
 I $$PROMPRN("HS") G:DG1<0 QTNOREG S DGOPT=DGOPT_8
 I $$PROMPRN("PRO") G:DG1<0 QTNOREG S DGOPT=DGOPT_5,PRF=1
QTNOREG S:$G(DG1)<0 DGOPT=""
 Q
PROMPRN(DGX) ; PROMPTS FOR PRINT
 ; RETURNS DGX [1=YES;0=NO;-1=DIRUT CONDITIONS]
 ; OUTPUT:DG1 VALUE
 S DG1=0
 I $$FAILCOND(DGX) S DG1=$S($D(DG1):DG1,1:2) G QTPROMP ;DEFAULT=NO
 F  D  Q:$G(DG1)
 .S DG1=$$ASK(DGX)
 .S:DG1=1 DG1=$$AFTERASK(DGX)
QTPROMP Q $S(DG1=2:0,(DG1=1):1,1:DG1)
ASK(DGX) ; PROMPTS FOR PRINT
 ;
 ; RETURNS DGX [2=NO,1=YES;0=?,-1=DIRUT CONDITIONS]
 W !,"PRINT "
 W $S(DGX="THIRD":"ERROR",(DGX="HS"):"HEALTH SUMMARY",(DGX="PRO"):"DRUG PROFILE",(DGX="EF"):"ENCOUNTER FORMS",1:"ERROR")
 S %=1 D YN^DICN I '% W !,"ENTER 'Y'ES TO PRINT A ",DGX,".  OTHERWISE ENTER 'N'O."
 Q $G(%)
FAILCOND(DGX) ;CHECKS IF PROMPT SHOULD BE ASKED
 ;
 ;   DGI: 2=NO;1=YES;-1=DIRUT
 ;RETURNS 1=DON'T ASK AND SKIP TO NEXT;0=ASK
 ;
 N DGFAIL
 S DGFAIL=0
 I DGX=1010 G QTFAIL
 I DGX="THIRD" F  D  Q:$G(%)  G QTFAIL
 .N DGNOQ,DGDEF
 .D ADM
 .S DGFAIL=1
 .I DGPMDA>0!$D(^DGS(41.1,"B",DFN)) D
 ..S (DGNOQ,DGDEF)=1 D ASK^DGBLRV
 ..S DG1=%
 I DGX="HS" S DGFAIL=1 D  G QTFAIL
 .S X="GMTSDVR" X ^%ZOSF("TEST") I $T D
 ..S:$T(ENXQ^GMTSDVR)]""&($P(^DG(43,1,0),U,42)) DGFAIL=0
 I DGX="PRO" S DGFAIL=1 D  G QTFAIL
 .S X="PSOSD1" X ^%ZOSF("TEST") I '$T Q
 .I '$P(^DG(43,1,0),U,17) Q
 .S DGFAIL=0
 I DGX="EF" D  G QTFAIL
 .I $P(^DG(43,1,0),U,47)'=1 S DGFAIL=1 Q
QTFAIL Q DGFAIL
AFTERASK(DGX) ;ACTIONS AFTER REPONSE OF YES TO PRINT
 ;NOTE: Reports removed from DG REGISTRATION 10/10 REPRINT option are
 ;       remaining to support any outside integrations.
 ;
 ; RETURNS DGGO[2=DON'T PRINT,1=PRINT,-1=ABORT]
 ; SETS PRINT-SETUP VARIABLES
 ;   PSOINST - STATION NUMBER (INSTITUTION FILE) FOR ACTION PROFILE PRINT
 ;   PSONOPG - USED FOR PRINTING ACTION PROFILE (SET TO 1)
 ;   PSOPAR  - PRINT BARCODES FOR ACTION PROFILE (SET TO 1)
 ;   PSTYPE = DRUG PROFILE TYPE
 ;   GMTSTYP = POINTER TO HEALTH SUMMARY TYPE
 ;
 N DGGO,DIR,X,Y
 S DGGO=1
 I DGX="HS" D  ;HEALTH SUMMARY
 .S X=$P($G(^DG(43,1,0)),U,43),DIC=142,DIC(0)="NX"
 .D ^DIC K DIC
 .S:+Y DIR("B")=$P(Y,U,2)
 .S DIR(0)="PO^142:QAMEZ"
 .D ^DIR
 .I Y'>0 W !,*7,"No Type Selected.  HS will not print" S DGGO=2 K DIR,DIRUT,DUOUT Q
 .S GMTSTYP=+Y
 I DGX="PRO" D  ;DRUG PROFILE
 .S DGGO=0
 .N DGDEF
 .S DGDEF=$P(^DG(43,1,0),U,45)
 .I $P(^DG(43,1,0),U,44) D
 ..S:DGDEF]"" DIR("B")=$S(DGDEF="A":"ACTION",(DGDEF="I"):"INFORMATIONAL",1:"")
 ..S DIR(0)="SM^A:ACTION;I:INFORMATIONAL"
 ..S DIR("A")="Select type of Drug Profile"
 ..D ^DIR
 ..S DGDEF=Y
 .I '$D(DIRUT) D
 ..S (PSOPAR,PSTYPE)=$S(DGDEF="A":1,(DGDEF="I"):0,1:0),(DGGO,PSONOPG)=1
 ..S PSOINST=+$P($G(^DIC(4,+$P($G(^XMB(1,1,"XUS")),U,17),99)),U)
 Q DGGO
ADM K DGPMDA I $D(^DGPM("ATID1",DFN)) F I=0:0 S I=$O(^DGPM("ATID1",DFN,I)) Q:'I!(I>(DFN1+.9999))  S DGPMDA=$O(^(I,0))
 S DGPMDA=$S($D(DGPMDA):DGPMDA,1:0)
 Q

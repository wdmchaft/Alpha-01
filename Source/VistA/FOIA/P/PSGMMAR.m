PSGMMAR ;BIR/CML3-MULTIPLE DAY MARS - MAIN DRIVER ;14 Oct 98 / 4:28 PM
 ;;5.0; INPATIENT MEDICATIONS ;**15,20,110,111,131,145**;16 DEC 97;Build 17
 F  R !!,"HOW MANY DAYS? (7/14) ",PSGMARDF:DTIME S:'$T PSGMARDF="^" Q:"^"[PSGMARDF  Q:PSGMARDF=7!(PSGMARDF=14)  W $C(7),"  7 OR 14 DAY MAR!!"
 G:"^"[PSGMARDF DONE G EN
 ;
EN7 ;
 S PSGMARDF=7 G EN
 ;
EN14 ;
 S PSGMARDF=14
 ;
EN ;
 NEW DRUGNAME,F,MARLB,NAME,UP,PSGOP
 D ENCV^PSGSETU G:$D(XQUIT) DONE K PSGMAROF
 ;
EN1 ;
 D MARFORM^PSGMUTL G:'PSGMARB DONE
 ;
ENOE ;
 D SD^PSGMMARH W ! D ^DIR K DIR,DTOUT,DUOUT,DIRUT,DIROUT G:"^"[$E(Y) DONE S PSGMARS=$F("CPBO",Y)-1
 ;
DATE ;
 S %DT="ETSX",Y=-1 F  W !!,"Enter START DATE/TIME for "_PSGMARDF_" day MAR: " R X:DTIME W:'$T $C(7) S:'$T X="^" Q:"^"[X  D:X?1."?" DH^PSGMMARH D ^%DT Q:Y>0
 I Y'>0 W $C(7),!!?5,"(No date selected, or MAR run.)" G DONE
 S PSGMARSD=+Y,X1=$P(+Y,"."),X2=PSGMARDF-1 D C^%DTC S PSGMARFD=X
 D NOW^%DTC S PSGDT=%,(PSGMARWG,PSJPWDO)=0,PSGRBPPN="",PSGMARWD=+$G(PSJPWD)
 I '$G(PSGMAROF),'$G(PSGOENOF)!($G(PSGSS)="") S (PSGP,PSGPAT,PSGMARWD)=0,PSGSSH="MAR" D ^PSGSEL G:"^"[PSGSS OUT D @PSGSS G:$G(PSJSTOP) OUT
 I PSGMARB'=1 G:$$MEDTYPE^PSJMDIR(PSGMARWD) OUT S PSGMTYPE=Y
 D DEV I POP!$D(IO("Q")) G DONE
 ;
ENQ ; when queued
 N DRGI,DRGN,DRGT,LN,P,PSIVUP,PSJORIFN,PSGMSORT
 D ^PSGMMAR0 I $D(^TMP($J))>9 D ^PSGMMAR1 K ^TMP($J)
 ;DAM 5-01-07
 I $D(PSGREP) K ^XTMP(PSGREP)
 ;END DAM
 D ^%ZISC G DONE
 ;
OUT W $C(7),!!?5,"(No patient(s) selected for MAR run.)" K PSGPLF,PSGPLS
DONE ;
 I '$D(PSGOENOF),'$D(PSGVBY) D ENKV^PSGSETU
 K:'$D(PSGVBY) PSGSS,PSGSSH
 D ENKV^PSGLOI
 K AD,ASTERS,BD,BLN,CNTR,DA1,DA2,DAO,DIC,DRG,DX,EXPIRE,FD,HX,L,LN1,LN14,LN2,LN3,LN31,LN32,LN4,LN5,LN6,LN7
 K MOS,MSG1,MSG2,N,ND2,NAMENEED,NEED,OPST,PSJJORD,PAGE,PN,PND,PNN,PPN,PRB,PSEX,PSSN,PSGMAPA,PSGMAPB,PSGMAPC,PSGMAPD,PSGADR,PSGALG,PSGS0Y,PSGXDT
 K PSGD,PSGDW,PSGMAR,PSGMARB,PSGMARDF,PSGMARED,PSGMARGD,PSGMARFD,PSGMARFP,PSGMAROC,PSGMAROF,PSGMARPT,PSGMARS,PSGMARSD,PSGMARSM,PSGMARSP
 K PSGMARTS,PSGMARWD,PSGMARWG,PSGMARWN,PSGMARWS,PSGMPG,PSGMPGN,PSGORD,PSGPAT,PSJDIET
 K DFN,NG,NO,ON,PST,PTM,PWDN,QST,PSJACNWP,R,RB,RCT,S,SD,SM,SPACES,TM,T,TD,TS,WD,WDN,WG,WGN,WS,WT,X1,X2,Y1
 K PSJSTOP,PSJPWDO,PSGMARO,PSGMTYPE,PSGTM,PSGTMALL,XTYPE,PSGLRPH,PSGPG
 K HT,PSGOENOF,PSGOES,PSGRBPPN,PSGS0XT,PSGST,PSGTIR,PSGWD,XQUIT,ZTDESC,ONHOLD
 Q
 ;
G ;
 S DIC="^PS(57.5,",DIC(0)="QEAMI",DIC("A")="Select WARD GROUP: " W ! D ^DIC K DIC D  I $G(PSJSTOP)=1 Q
 . I X="^OTHER" S PSGMARWG="^OTHER" Q
 . S PSGMARWG=+Y
 . I +Y'>0 S PSJSTOP=1
 D RBPPN^PSJMDIR
 Q
 ;
W ;
 S DIC="^DIC(42,",DIC(0)="QEAMI",DIC("A")="Select WARD: " W ! D ^DIC K DIC S PSGMARWD=+Y I +Y<0 S PSJSTOP=1 Q
 S PSGWD=PSGMARWD D ADMTM^PSJMDIR S Y=PSGMARWD
 D:'PSJSTOP RBPPN^PSJMDIR
 Q
 ;
P ;
 K PSGPAT S PSGPAT=0 F CNTR=0:1 S:CNTR PSGDICA="another" D ENP^PSGGAO:'PSGMARB,ENDPT^PSGP:PSGMARB Q:PSGP'>0  D
 . S PSGPAT(PSGP)="",PSGPAT=PSGP
 . ;*** PSGMARWD=1 when all patients are select from the same ward.
 . S:'$G(PSJPWDO) (PSGMARWD,PSJPWDO)=PSJPWD S PSGMARWD=$S('$G(PSGMARWD):0,PSJPWDO=PSJPWD:PSJPWD,1:0)
 S Y=PSGPAT S:Y'>0 PSJSTOP=1 K PSGDICA
 Q
 ;
C ;
 ;DAM 5-01-07 Add new variable to hold numerical value of CLINIC
 S PSGCLNC=""
 K DIR S DIR(0)="FAO",DIR("A")="Select CLINIC: "
 S DIR("?")="^D CDIC^PSGVBW" W ! D ^DIR
CDIC ;
 K DIC S DIC="^SC(",DIC(0)="QEMIZ" D ^DIC K DIC S:+Y>0 CL=+Y S PSGCLNC=+Y I +Y<0 S PSJSTOP=1 Q
 W:X["?" !!,"Enter the clinic you want to use to select patients for processing.",!
 Q
L ;
 K DIR S DIR(0)="FAO",DIR("A")="Select CLINIC GROUP: "
 S DIR("?")="^D LDIC^PSGVBW" W ! D ^DIR
LDIC ;
 K DIC S DIC="^PS(57.8,",DIC(0)="QEMI" D ^DIC K DIC S:+Y>0 CG=+Y I +Y<0 S PSJSTOP=1 Q
 W:X["?" !!,"Enter the name of the clinic group you want to use to select patients for processing."
 Q
DEV ;
 K ZTSAVE S PSGTIR="ENQ^PSGMMAR",ZTDESC=PSGMARDF_" DAY MAR" F X="PSGMARWG","PSGMARWD","PSGP","PSGPAT(","PSGDT","PSGMARSD","PSGMARFD","PSGSS","PSGMARB","PSGMARDF","PSGMARS","PSGINCL","PSGINCLG","PSGINWD","PSGINWDG" S ZTSAVE(X)=""
 F X="PSGMTYPE","PSGRBPPN","^TMP($J," S ZTSAVE(X)=""
 I PSGSS="W" F X="PSGTMALL","PSGTM","PSGTM(" S ZTSAVE(X)=""
 D ENDEV^PSGTI W:POP !!?3,"No device selected for "_PSGMARDF_" day MAR run." W:$D(ZTSK) !?3,PSGMARDF_" Day MAR Queued!" K ZTSK Q
 I 'IO("Q") U IO
 ;
ENOR S PSGP=+ORVP
ENLM ;
 NEW VADM
 D ENCV^PSGSETU I $D(QUIT) K PSGMARDF Q
 D ^PSJAC S PSGPAT=1,PSGPAT(PSGP)="",PSGMAROF=1,PSGSS="P" G EN1

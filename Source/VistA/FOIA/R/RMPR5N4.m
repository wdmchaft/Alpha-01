RMPR5N4 ;HIN/RVD-PRINT LAB ITEM SUMMARY BY EMPLOYEE ;3/13/1998
 ;;3.0;PROSTHETICS;**33,37,159**;Feb 09, 1996;Build 2
EN N RMEMP D DIV4^RMPRSIT
 K ^TMP($J),RMPRFLG,RMPRI S RMPREND=0 D HOME^%ZIS G:$D(X) EXIT1
 S DIC="^VA(200,",DIC(0)="MAEQ"
EN1 R !!,"Enter 'ALL' for all Lab Employees or 'RETURN' to select individual names: ",RMENTER:DTIME G:$D(DTOUT)!$D(DUOUT)!(RMENTER="^") EXIT1
 G:RMENTER["?" EN1
 S X=RMENTER X ^%ZOSF("UPPERCASE") S RMENTER=Y I RMENTER="ALL" S RMPRI(0)=1 G CONT
 W ! F EMP=1:1 S DIC("A")="Select EMPLOYEE "_EMP_": " D ^DIC G:$D(DTOUT)!(X["^")!(X=""&(EMP=1)) EXIT1 Q:X=""  D
 .S RMEMP=$P(^VA(200,+Y,0),U,1) Q:RMEMP=""
 .I $D(RMPRI(RMEMP)) W $C(7)," ??",?40,"..Duplicate employee name" S EMP=EMP-1 Q
 .S RMPRI(RMEMP)=+Y
CONT G:'$D(RMPRI) EXIT1 S RMPRCOUN=0 W !! S %DT("A")="Beginning Date: ",%DT="AEPX",%DT("B")="T-30" D ^%DT S RMPRBDT=Y G:Y<0 EXIT1
ENDATE S %DT("A")="Ending Date: ",%DT="AEX",%DT("B")="TODAY" D ^%DT G:Y<0 EXIT1 I RMPRBDT>Y W !,$C(7),"Invalid Date Range Selection!!" G ENDATE
 G:Y<0 EXIT1 S RMPREDT=Y,Y=RMPRBDT D DD^%DT S RMPRX=Y,Y=RMPREDT D DD^%DT S RMPRY=Y
 S %ZIS="MQ" K IOP D ^%ZIS G:POP EXIT1
 I '$D(IO("Q")) U IO G PRINT
 K IO("Q") S ZTDESC="PROS LAB ITEM SUMMARY",ZTRTN="PRINT^RMPR5N4",ZTIO=ION,ZTSAVE("RMPRBDT")="",ZTSAVE("RMPREDT")="",ZTSAVE("RMPRI(")="",ZTSAVE("RMPRX")="",ZTSAVE("RMPRY")="",ZTSAVE("RMPR(""STA"")")="",ZTSAVE("RMPR(")=""
 D ^%ZTLOAD W:$D(ZTSK) !,"REQUEST QUEUED!" H 1 G EXIT1
PRINT I $E(IOST)["C" W !!,"Processing report......"
 ;ENTRY POINT FOR PRINTING REPORT
 W:$E(IOST)["C" @IOF S RMPAGE=1,RMPREND=0 D:$D(RMPRI(0)) ALL
 I '$D(RMPRI) D NONEALL G EXIT
 ;D HEAD
 S RMD="" F  S RMD=$O(RMPRI(RMD)) Q:RMD=""  S RMI=RMPRI(RMD) D PRI1
 G:RMPREND EXIT D WRI D:'$D(^TMP($J)) NONEALL G EXIT
 ;
PRI1 F RO=0:0 S RO=$O(^RMPR(661.2,"F",RMI,RO)) Q:(RO'>0)!(RMPREND)  D REST
 D:'$D(^TMP($J,RMD))&'$D(RMPRI(0)) NONE
 Q
 ;
EXIT ;Exit
 ;I RMPRCOUN>0,$D(RMPREDT),'$D(KILL) D  W !!?32,"END OF REPORT"
 ;I $E(IOST)["C"&($Y<22) F  W ! Q:$Y>20
 I $D(RMPREDT),$E(IOST)["C",'$D(DUOUT),'$D(DTOUT),'RMPREND K DIR S DIR(0)="E" D ^DIR
EXIT1 ;
 D ^%ZISC
 N RMPR,RMPRSITE D KILL^XUSCLEAN
 Q
 ;
HEAD ;write heading
 W !,?18,"PROSTHETICS LAB ITEM SUMMARY BY EMPLOYEE",?64,"Page: ",RMPAGE
 W !,RMPRX," to ",RMPRY
 S RMPAGE=RMPAGE+1
 Q
HEAD1 I $E(IOST)["C"&($Y>(IOSL-7)) S DIR(0)="E" D ^DIR S:$D(DTOUT)!(Y=0) RMPREND=1 Q:RMPREND  W @IOF D HEAD
 I $E(IOST)'["C"&($Y>(IOSL-6)) W @IOF D HEAD
 W !,RMPR("L"),!,"Employee: ",I
 W !,"  DATE",?17,"ITEM",?43,"HCPCS",?51,"TIME SPENT(hr)",?68,"LABOR COST"
 W !,"  ----",?17,"----",?43,"-----",?51,"--------------",?68,"----------"
 S RMPRFLG=1
 Q
 ;
SUM W !,?53,"--------",?70,"--------",!,?39,"***TOTALS =",?53,$J(RMTITOT,7,2),?67,$J(RMDOTOT,10,2),!
 W @IOF K RMPRFLG
 Q
 ;
REST Q:'$D(^RMPR(661.2,RO,1))
 S RM20=$G(^RMPR(661.2,RO,0)),RMDAT=$P(RM20,U,1)
 Q:(RMDAT<RMPRBDT)!(RMDAT>RMPREDT)
 S ^TMP($J,RMD,RO)=RMDAT
 Q
WRI ;
 ;S RMD="" F  S RMD=$O(RMPRI(RMD)) Q:RMD=""
 ;
 S I="" F  S I=$O(^TMP($J,I)) Q:I=""  Q:RMPREND  S (RMDOTOT,RMTITOT,RMSUF)=0 F J=0:0 S J=$O(^TMP($J,I,J)) Q:RMPREND  D:J'>0 SUM Q:J'>0  D
 .S RMSUF=1
 .I '$D(RMPRFLG) D HEAD,HEAD1
 .S RMDAT=^TMP($J,I,J)
 .S RM20=$G(^RMPR(661.2,J,0)),RM21=$G(^RMPR(661.2,J,1))
 .S RMPAT=$P(RM20,U,2)
 .S RMIT=$P(RM20,U,9)
 .S RMDAHC=$P(RM20,U,4)
 .S RMDAIT=$P(RMIT,"-",2)
 .Q:'RMDAHC
 .S:RMDAIT RM1=$P($G(^RMPR(661.1,RMDAHC,3,RMDAIT,0)),U,1)
 .S:'RMDAIT RM1=$P($G(^RMPR(661.1,RMDAHC,0)),U,2) Q:RM1=""
 .S RMHCPC=$P(^RMPR(661.1,RMDAHC,0),U,1)
 .S RMITEM=$E($P(RM1,U,1),1,31)
 .S RMHOUR=$P(RM21,U,1),RMDOL=$P(RM21,U,2)
 .S RMDAT=$E(RMDAT,4,5)_"/"_$E(RMDAT,6,7)_"/"_$E(RMDAT,2,3)
 .W !,RMDAT,?11,RMITEM,?43,RMHCPC,?56,$J(RMHOUR,4,2),?70,$J(RMDOL,7,2)
 .S:RMDOL RMDOTOT=RMDOTOT+RMDOL
 .S:RMHOUR RMTITOT=RMTITOT+RMHOUR
 .S RMPRFLG=1
 .I $E(IOST)["C"&($Y>(IOSL-7)) S DIR(0)="E" D ^DIR S:$D(DTOUT)!(Y=0) RMPREND=1 Q:RMPREND  W @IOF D HEAD,HEAD1 Q
 .I $Y>(IOSL-6) W @IOF D HEAD,HEAD1 S RMPRFLG=1
 Q
 ;
ALL ;process all lab employees.
 S RMEMP=""
 F I=0:0 S I=$O(^RMPR(661.2,"F",I)) Q:I'>0  S RMEMP=$P(^VA(200,I,0),U,1) S RMPRI(RMEMP)=I
 Q
 ;
NONE ;No data for certain employee
 W !,RMPR("L"),!,"No Lab data for Employee: ",RMD," ....for this date range !!!"
 Q
NONEALL W !!,"NO DATA FOR THIS DATE RANGE!!!"
 Q

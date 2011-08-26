LRARCMR ;DALISC/CKA  -  SETUP ARCHIVED WORKLOAD REPORT PARAMETERS;5/22/95
 ;;5.2;LAB SERVICE;**59**;Aug 31, 1995
 ;same as LRCAPMR except archived wkld file
EN ;called by LRARCML,LRARCTS,LRARCMA
PARMS ; SET PARAMATERS
 D GETINST
 D:'LREND BDT
 D:'LREND EDT
 D:'LREND GETAA
 D:'LREND SUMQ
 D:'LREND DEVICE
 Q
GETINST R !,"SELECT ALL INSTITUTIONS?  YES// ",LRIN:DTIME
 I '$T!(LRIN["^") S LREND=1 Q
 I LRIN["?" W !,"ENTER YES OR NO, Y OR N" G GETINST
 I LRIN=""!(LRIN="Y")!(LRIN="YES") S LRIN=0,LRINN="" Q
 S LRIN=$S(+DUZ(2):+DUZ(2),+$P($G(^XMB(1,1,"XUS")),U,17):+$P(^("XUS"),U,17),1:0)
 D INS
 Q
INS ;
 K DIC S DIC="^LAR(64.19999,",DIC(0)="AEQM" S:LRIN DIC("B")=$P($G(^DIC(4,LRIN,0)),U)
 D ^DIC I Y<0 S LREND=1 Q
 S LRIN=+Y,LRINN=$P(Y,"^",2)
 Q
BDT ;
 K %DT,DTOUT,DUOUT
 S %DT="AESX",%DT("A")="BEGINNING DATE/TIME:  ",%DT("B")="T-31"
 D ^%DT I Y=-1 S LREND=1 Q
 S LRCDTB=$P(Y,".")
 S LRCTMB=($S(+$P(Y,".",2):"."_$P(Y,".",2),1:0.0001)-.00001)
 S Y1=Y,Y2=1,LRDT1=$$DDDATE^LRAFUNC1(Y1,Y2)
 Q
EDT ;
 K %DT,DTOUT,DUOUT
 S %DT="AESX",%DT("A")="ENDING DATE/TIME:  ",%DT("B")="T"
 D ^%DT I Y=-1 S LREND=1 Q
 S LRCDTE=$P(Y,"."),LRCTME=$S(+$P(Y,".",2):"."_$P(Y,".",2)*10000,1:2400)
 S Y1=Y,LRDT2=$$DDDATE^LRAFUNC1(Y1,Y2) K Y1,Y2
 Q
GETAA S LRAA=0 W !
 K DIR,X,Y S DIR(0)="S^Y:YES;N:NO",DIR("B")="NO"
 S DIR("A")="Do you want to select accession areas (YES or NO) "
 S DIR("?")="Enter 'YES' to limit report to one or more accession areas."
 D ^DIR
 Q:Y="N"
 I ($D(DTOUT))!($D(DUOUT)) S LREND=1 Q
 K DIC S DIC=68,DIC(0)="AEMQZ"
 F  D ^DIC Q:Y=-1  D
 .S LRAA=+Y,LRAA(+Y)=$P(Y(0),U,11),LRAAX(Y(0,0))=Y(0,0)
 I ($D(DTOUT))!($D(DUOUT)) S LREND=1 Q
 Q
DEVICE ;
 K %ZIS,POP S %ZIS="QN" D ^%ZIS
 I POP S LREND=1
 Q
SUMQ ;
 R !!,"SUMMARY REPORT ONLY?  NO//",LRSUMM:DTIME
 I '$T!(LRSUMM="^") S LREND=1 Q
 I LRSUMM["?" W !,"Do you want only the summary?  YES or NO.",! G SUMQ
 S LRSUMM=$S($E(LRSUMM,1)="Y"!(LRSUMM="YES")!($E(LRSUMM,1)="y")!(LRSUMM="yes"):1,1:0) W !
 Q

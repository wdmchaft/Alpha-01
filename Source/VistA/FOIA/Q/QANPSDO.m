QANPSDO ;HISC/GJC Pseudo VA 10-2633 ; 10/1/92
 ;;2.0;Incident Reporting;**1,31**;08/07/1992
 ;
 ;CHOOSE PATIENT, THEN THE INCIDENT PUT INTO REPORT OPTION
 ;***QANDFN IS FILE 742'S IEN ***:*** QANIEN IS FILE 742.4'S IEN ***
PAT ;
 S QANXIT=0 K DIR S DIR("A")="Do you wish to generate a blank 10-2633? "
 S DIR(0)="YA",DIR("?")="Enter 'Y' for yes, 'N' for no."
 D ^DIR K DIR G:$D(DIRUT)!($D(DIROUT)) KILL
 S QANBLNK=+Y G:QANBLNK SETUP
 K DIC S DIC="^QA(742,",DIC(0)="QEAMZ",DIC("A")="Select Patient: "
 S DIC("S1")="I ""013""[+$P(^QA(742.4,+$P(^QA(742,+Y,0),U,3),0),U,8)"
 S DIC("S2")="&('$D(^QA(742,""BPRS"",-1,+Y)))"
 S DIC("S")=DIC("S1")_DIC("S2")
 S DIC("W")="D DICW^QANUTL1",D="B^BS5" D MIX^DIC1 K D,DIC
 I +Y=-1 S QANXIT=1 W !!,*7,"Patient not selected, exiting!!" G EXIT
PAT1 W !?5,Y(0,0)_" OK" S %=1 D YN^DICN G:%=2 PAT
 S:%<0 QANXIT=1 W:QANXIT !!,*7,"Patient not selected, exiting!!" G:QANXIT EXIT
 I %=0 W !!,*7,"Enter ""Y""es if the patient choice is correct, ""N""o if the patient choice is ",!,"incorrect.",! G PAT
 S QANDFN=+Y,QANIEN=$P(Y(0),U,3),QANAME=Y(0,0)
 I '$D(QANIEN)!('$D(QANDFN)) W !!,*7,"Incomplete data, exiting the report." Q
 S QAN742=$G(^QA(742,QANDFN,0)),QAN7424=$G(^QA(742.4,QANIEN,0)),QANPAT=$P(QAN742,U) Q:+QANPAT<1
SETUP ;"Jump" here to set up vars for blank report, fall through for normal.
 S QANHEAD="PATIENT INCIDENT WORKSHEET",PAGE=0,$P(QANEQ,"=",81)="",$P(QANEQ1,"-",81)="",QANFLAG=0
TASK ;Task off to a device.
 S Y=DT X ^DD("DD") S TODAY=Y,QANFIN=""
 ;*** Choose device ***
 K IOP,%ZIS S %ZIS("A")="Print on device: ",%ZIS="MQ" W ! D ^%ZIS W !!
 G:POP KILL
 I $D(IO("Q")) S ZTRTN="STRT^QANPSDO",ZTDESC="Generate Patient Incident Worksheet(s)." D QLOOP,^%ZTLOAD W !,$S($D(ZTSK):"Request queued!",1:"Request cancelled!"),! G EXIT
STRT ;
 U IO D HDR G:QANBLNK BLANK
 S QANAME=$P($P(^DPT(QANPAT,0),U),",",2)_" "_$P($P(^DPT(QANPAT,0),U),","),QANPID=$P(QAN742,U,2),QANSSN=$E($P(^DPT(QANPAT,0),U,9),1,3)_"-"_$E($P(^DPT(QANPAT,0),U,9),4,5)_"-"_$E($P(^DPT(QANPAT,0),U,9),6,9),QANCASE=$P(QAN7424,U)
 S QANDOB=$P(^DPT(QANPAT,0),U,3),X=DT,X1=X,X2=QANDOB,X="" D:+X2>0 ^%DTC S X=X\365.25,QANAGE=X K X,X1,X2,QANDOB
 K C,Y S Y=$P(QAN742,U,4),C=$P(^DD(742,.04,0),U,2) D:Y]"" Y^DIQ S QANADMT=Y K C,Y
 K C,Y S Y=$P(QAN742,U,6),C=$P(^DD(742,.06,0),U,2) D:Y]"" Y^DIQ S QANWARD=Y K C,Y
 K C,Y S Y=$P(QAN742,U,8),C=$P(^DD(742,.08,0),U,2) D:Y]"" Y^DIQ S QANSERV=Y K C,Y
 K C,Y S Y=$P(QAN7424,U,2),C=$P(^DD(742.4,.02,0),U,2) D:Y]"" Y^DIQ S QANINCD=Y K C,Y
 K C,Y S Y=$P(QAN7424,U,11),C=$P(^DD(742.4,.12,0),U,2) D:Y]"" Y^DIQ S QANLREV=Y K C,Y
 K C,Y S Y=$P(QAN7424,U,4),C=$P(^DD(742.4,.04,0),U,2) D:Y]"" Y^DIQ S QANILOC=Y K C,Y
 K C,Y S Y=$P(QAN7424,U,9),C=$P(^DD(742.4,.1,0),U,2) D:Y]"" Y^DIQ S QANINIT=Y K C,Y
 K C,Y S Y=$P(QAN7424,U,3),C=$P(^DD(742.4,.03,0),U,2) D:Y]"" Y^DIQ S QANDATE=Y K C,Y
 K C,Y S Y=$P(QAN742,U,10),C=$P(^DD(742,.1,0),U,2) D:Y]"" Y^DIQ S QANSLVL=Y K C,Y
 K C,Y S Y=$P(QAN7424,U,7),C=$P(^DD(742.4,.08,0),U,2) D:Y]"" Y^DIQ S QANWIT=Y K C,Y
BLANK D ^QANPSD1 ; Do prints
 I $E(IOST)'="C" F  W ! W:$Y>(IOSL-4) "VA Form 10-2633" Q:$Y>(IOSL-4)
EXIT W ! D ^%ZISC,HOME^%ZIS
KILL ;Kill and Quit
 K %T,%W,%Y,DTOUT,DUOUT,DIROUT,DIRUT,QANBLNK,QANLNCT
 K %,%ZIS,BA,C,D,DIC,DIW,DIWF,DIWI,DIWL,DIWR,DIWT,DIWTC,DIWX,DN,PAGE
 K POP,QAN,QAN742,QAN7424,QANADMT,QANAGE,QANAME,QANCASE,QANDATE,QANDFN
 K QANDOB,QANEQ,QANEQ1,QANFIN,QANFLAG,QANHEAD,QANIEN,QANILOC,QANINCD
 K QANINIT,QANLBL,QANLREV,QANMN,QANPAT,QANPID,QANSERV,QANSLVL,QANSSN
 K QANTYPE,QANWARD,QANWIT,QANXIT,QANXXX,TODAY,X,X1,X2,Y,Z,ZTDESC,ZTRTN
 K X3,ZTSAVE,ZTSK
 Q
QLOOP ;
 F BA="^UTILITY($J,","PAGE","TODAY","QAN*","QAQ*" S ZTSAVE(BA)=""
 Q
HDR ;Header generator.
 I PAGE,($E(IOST)'="C") W !,"VA Form 10-2633"
 S PAGE=PAGE+1 W @IOF,!?69,TODAY,!?69,"Page: ",PAGE,!!
 W ?(IOM-$L(QANHEAD)\2),QANHEAD,!
 ;D EN6^QAQAUTL
 W QANEQ1,!,QANEQ1,!
 W:$D(QANPID) !,"PATIENT ID: ",QANPID,!
 W:QANFLAG&($D(QANLBL)) !?5,QANLBL_"(cont)"
 Q
HDH ;Check for end of screen.
 I $E(IOST)="C" K DIR S DIR(0)="E" D ^DIR K DIR S:+Y=0 QANFIN="^"
 Q:QANFIN["^"  D HDR
 Q

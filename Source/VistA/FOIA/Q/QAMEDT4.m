QAMEDT4 ;HISC/DAD-EDIT SAMPLE SIZE IN MONITOR HISTORY FILE ;8/26/93  13:14
 ;;1.0;Clinical Monitoring System;;09/13/1993
MONITOR K DIC S DIC="^QA(743,",DIC(0)="AEMNQ",DIC("A")="Select MONITOR: ",QAMDFLT=1
 W ! D ^DIC K DIC G:Y'>0 EXIT S QAMD0=+Y
 S QAMZERO=$S($D(^QA(743,QAMD0,0))#2:^(0),1:""),QAMONE=$S($D(^QA(743,QAMD0,1))#2:^(1),1:"")
 I $P(QAMZERO,"^",5)'>0 W !!?5,"*** THIS MONITOR IS STILL UNDER CONSTRUCTION, CANNOT CONTINUE ***",*7 G MONITOR
 I $P(QAMONE,"^",5)'>0 W !!?5,"*** THIS MONITOR'S ON/OFF SWITCH IS TURNED OFF, CANNOT CONTINUE ***",*7 G MONITOR
 S QAMFRAME=+QAMONE I QAMFRAME'>0 W !!?5,"*** NO TIME FRAME FOUND FOR THIS MONITOR, CANNOT CONTINUE ***",*7 G MONITOR
DATE D NOW^%DTC S QAMDT=X K %DT S %DT="AEX",%DT(0)=-QAMDT,%DT("A")="Date for SAMPLE SIZE data: " S:QAMDFLT %DT("B")="TODAY"
 W ! D ^%DT K %DT G:Y'>0 MONITOR S QAMTODAY=Y
 I (QAMTODAY<$S($P(QAMONE,"^",6):$P(QAMONE,"^",6),1:9999999))!(QAMTODAY>$S($P(QAMONE,"^",7):$P(QAMONE,"^",7),1:9999999)) W !!?5,"*** OUTSIDE START/END DATE RANGE OF MONITOR ***",*7
 I  S Y=$P(QAMONE,"^",6) X ^DD("DD") W !?5,"*** START DATE: ",$S(Y]"":Y,1:"NONE") S Y=$P(QAMONE,"^",7) X ^DD("DD") W "    END DATE: ",$S(Y]"":Y,1:"NONE")," ***" G DATE
 D EN^QAMTIME0 I (QAMSTART'>0)!(QAMEND'>0) W !!?5,"*** NO START/END DATE FOR TIME FRAME, CANNOT CONTINUE ***",*7 G MONITOR
 S QAMHISD0=$O(^QA(743.2,"AA",QAMD0,QAMSTART,QAMEND,0))
 I QAMHISD0'>0 K DD,DIC,DINUM,DO S DIC="^QA(743.2,",DIC(0)="LM",DIC("DR")=".02///^S X="_QAMSTART_";.03///^S X="_QAMEND,DLAYGO=743.2,X=QAMD0 D FILE^DICN K DIC S QAMHISD0=+Y
 S QAMHIST=^QA(743.2,QAMHISD0,0),Y=$P(QAMHIST,"^",11) X ^DD("DD")
 W !!?5,"SAMPLE SIZE data was last edited: ",$S(Y]"":Y,1:"*** NEVER ***"),!?5,"Current FALL OUTS:   ",$J($P(QAMHIST,"^",4),7,0),!?5,"Current SAMPLE SIZE: ",$J($P(QAMHIST,"^",5),7,0)
ASK K DIR S DIR(0)="NOA^-1000000:1000000:0",DIR("A")="Add/Subtract SAMPLE SIZE  (-1000000 -> 1000000): ",DIR("?",1)="Enter the number you wish to add to or",DIR("?")="subtract from the current SAMPLE SIZE."
 W ! D ^DIR K DIR S QAMDFLT=0 G:($E(X)="^")!(+Y=0) DATE S QAMSAMPL=Y+$P(QAMHIST,"^",5) I QAMSAMPL<0 W " ??",*7 G ASK
 S DIE="^QA(743.2,",DR="2///^S X="_QAMSAMPL_";8///T",DA=QAMHISD0 D ^DIE W !!?5,"New SAMPLE SIZE:     ",$J(QAMSAMPL,7,0)
 G DATE
EXIT ;
 K %,%DT,%H,%I,DA,DIC,DIR,DLAYGO,QAMD0,QAMFRAME,QAMONE,QAMSTART,QAMEND,QAMTODAY,QAMHISD0,QAMHIST,QAMSAMPL,QAMDT,QAMDFLT,QAMZERO
 Q

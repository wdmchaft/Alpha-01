QAMPINQ1 ;HISC/DAD-INQUIRE: FALL OUT FILE ;2/10/92  07:33
 ;;1.0;Clinical Monitoring System;;09/13/1993
EN1 D XIT S QAQDIC="^QA(743.1,",QAQDIC(0)="AEMNQ",QAQDIC("A")="Select PATIENT: ",QAQUTIL=743.1 D ^QAQSELCT G:QAQQUIT EXIT
 K %ZIS,IOP S %ZIS="MQ",%ZIS("B")="HOME" W ! D ^%ZIS G:POP EXIT I $D(IO("Q")) K IO("Q") S ZTRTN="ENTSK^QAMPINQ1",ZTSAVE("^UTILITY($J,")="",ZTDESC="Fall out file inquire" D ^%ZTLOAD G EXIT
ENTSK ;
 S QAMQUIT=0,QAMPTXT="" K QAMUNDL S $P(QAMUNDL,"=",81)="" U IO
 F  S QAMPTXT=$O(^UTILITY($J,743.1,QAMPTXT)) Q:QAMPTXT=""!QAMQUIT  F QAMD0=0:0 S QAMD0=$O(^UTILITY($J,743.1,QAMPTXT,QAMD0)) Q:QAMD0'>0!QAMQUIT  D LOOP0
EXIT ;
 W ! D ^%ZISC
XIT K %ZIS,DFN,DGA1,DGT,DIR,POP,QA,QAM,QAMCREAT,QAMD0,QAMD1,QAMELEM,QAMEVENT,QAMMON,QAMNAME,QAMPAGE,QAMPTXT,QAMQUIT,QAMUNDL,QAQDIC,QAQQUIT,QAQUTIL,X,Y,ZTRTN,ZTSAVE,^UTILITY($J,"QAMPINQ1")
 S:$D(ZTQUEUED) ZTREQ="@"
 Q
LOOP0 ;
 S QAM=$S($D(^QA(743.1,QAMD0,0))#2:^(0),1:"") Q:QAM=""  S Y=$P(QAM,"^",3) X ^DD("DD") S QAMEVENT=Y,Y=$P(QAM,"^",4) X ^DD("DD") S QAMCREAT=Y
 S QAMNAME=+QAM,QAMNAME=$S($D(^DPT(QAMNAME,0))#2:$P(^(0),"^"),1:QAMNAME),QAMMON=$P(QAM,"^",2),QA=$S($D(^QA(743,+QAMMON,0))#2:^(0),1:QAMMON),QAMMON=$P(QA,"^")_$S($P(QA,"^",4):" (a)",1:" (m)")
 K ^UTILITY($J,"QAMPINQ1") F QAMD1=0:0 S QAMD1=$O(^QA(743.1,QAMD0,1,QAMD1)) Q:QAMD1'>0  D LOOP1
 S QAMPAGE=1 D HEAD W !,"NAME: ",QAMNAME,?37,"MONITOR: ",QAMMON,!,"EVENT DATE: ",QAMEVENT,?37,"CREATION DATE: ",QAMCREAT,!
 S QAMELEM="" F  S QAMELEM=$O(^UTILITY($J,"QAMPINQ1",QAMELEM)) Q:QAMELEM=""!QAMQUIT  F QAMD1=0:0 S QAMD1=$O(^UTILITY($J,"QAMPINQ1",QAMELEM,QAMD1)) Q:QAMD1'>0!QAMQUIT  D LOOP2
 I 'QAMQUIT W !!,QAMUNDL D WAIT
 Q
LOOP1 ;
 S QA=+^QA(743.1,QAMD0,1,QAMD1,0),QAMELEM=$S($D(^QA(743.4,QA,0))#2:$P(^(0),"^"),1:QA),^UTILITY($J,"QAMPINQ1",QAMELEM,QAMD1)=$S($D(^QA(743.1,QAMD0,1,QAMD1,"E"))#2:$P(^("E"),"^"),1:"")
 Q
LOOP2 ;
 S QA=^UTILITY($J,"QAMPINQ1",QAMELEM,QAMD1) W !,QAMELEM,?37,$E(QA,1,43) D PAUSE:($O(^UTILITY($J,"QAMPINQ1",QAMELEM))]"")!($O(^(QAMELEM,QAMD1)))
 Q
PAUSE Q:$Y'>(IOSL-4)  D WAIT,HEAD:'QAMQUIT
 Q
HEAD W:(QAMPAGE>1)!($E(IOST)="C") @IOF W !,QAMUNDL,! S QAMPAGE=QAMPAGE+1
 Q
WAIT I $E(IOST)="C" K DIR S DIR(0)="E" D ^DIR S QAMQUIT=$S(Y'>0:1,1:0)
 Q

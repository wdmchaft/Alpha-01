PSAHIS ;BIR/LTL,JMB-Drug Transaction History ;7/23/97
 ;;3.0; DRUG ACCOUNTABILITY/INVENTORY INTERFACE;**3,15**; 10/24/97
 ;This routine prints a report of all or specific drugs in a pharmacy
 ;location for a user-specified number of days.
 ;
 ;References to ^PSDRUG( are covered by IA #2095
 ;
LOC ;Gets locations & drugs to print
 S (PSACNT,PSAOUT)=0 D ^PSAUTL3 G:PSAOUT EXIT1
 S PSACNT=0,PSACHK=$O(PSALOC(""))
 I PSACHK="",'PSALOC W !,"There are no active pharmacy locations." G EXIT1
 W ! S PSALOCN="" F  S PSALOCN=$O(PSALOC(PSALOCN)) Q:PSALOCN=""!(PSAOUT)  S PSALOC=0 F  S PSALOC=$O(PSALOC(PSALOCN,PSALOC)) Q:'PSALOC!(PSAOUT)  D
 .W @IOF W:$L(PSALOCN)>79 !,$P(PSALOCN,"(IP)",1)_"(IP)",!?17,$P(PSALOCN,"(IP)",2) W:$L(PSALOCN)<80 !,PSALOCN
 .D DRUG Q:PSAOUT  Q:X["^A"
 G:PSAOUT EXIT G DAYS
DRUG ;Gets drugs to print
 W !!,"You may select one, several, or ^ALL drugs."
 F  S DIC="^PSD(58.8,"_+PSALOC_",1,",DIC(0)="AEMQ",DIC("A")="Select Drug: " D  Q:PSAOUT!(Y<0)!(X["^A")
 .D ^DIC K DIC I X'["^A"&(Y<1)&('PSACNT) S PSAOUT=1 Q
 .I X["^A" D ALL^PSAHIS1 Q
 .Q:Y<0  I $G(DTOUT)!($G(DUOUT)) S PSAOUT=1 Q
 .S ^TMP("PSADRG",$J,PSALOC,$P($G(^PSDRUG(+Y,0)),"^"),+Y)="" S PSACNT=PSACNT+1
 .I PSACNT=1,'$O(^PSD(58.81,"F",+$O(^TMP("PSADRG",$J,PSALOC,$P($G(^PSDRUG(+Y,0)),"^"),0)),0)) W !!,"There have been no transactions for this drug.",!
 I '$D(PSALOC) W !!,"There are no drugs in all the selected location(s)." G EXIT
 Q
 ;
DAYS G:$O(^TMP("PSADRG",$J,""))="" EXIT
 S DIR(0)="D:AEP",DIR("A")="How many days back do you want to search for the drug: ",DIR("B")="T-6M",DIR("?")="I will list transactions for your selected drug(s) within the last six months if you press return" W ! D ^DIR K DIR
 S PSABDT=Y G:$G(DIRUT) EXIT
 ;
DEV ;Asks device & queueing info
 K IO("Q") N %ZIS,IOP,POP S %ZIS="Q" W !
 D ^%ZIS I POP W !,"NO DEVICE SELECTED OR OUTPUT PRINTED!" Q
 I $D(IO("Q")) D  G EXIT
 .N ZTDESC,ZTIO,ZTRTN,ZTSAVE,ZTDTH,ZTSK
 .S ZTRTN="START^PSAHIS",ZTDESC="Drug Acct.-Drug Transaction History"
 .S ZTSAVE("PSALOC(")="",ZTSAVE("PSABDT")="",ZTSAVE("^TMP(""PSADRG"",$J,")="" D ^%ZTLOAD
 ;
START ;Compiles & prints output data
 S PSARPDT=$E($$HTFM^XLFDT($H),1,12),PSADT=$P(PSARPDT,"."),PSATM=$P(PSARPDT,".",2)
 S PSARPDT=$E(PSADT,4,5)_"-"_$E(PSADT,6,7)_"-"_$E(PSADT,2,3),PSARUN=PSARPDT_"@"_PSATM
 S PSADLN="=====================================|==========|=====|=====|==========|========"
 S PSABDTR=$E(PSABDT,4,5)_"-"_$E(PSABDT,6,7)_"-"_$E(PSABDT,2,3),PSAOUT=0
 S PSALOCN="" F  S PSALOCN=$O(PSALOC(PSALOCN)) Q:PSALOCN=""  D  G:PSAOUT EXIT
 .S PSALOC=0 F  S PSALOC=$O(PSALOC(PSALOCN,PSALOC)) Q:'PSALOC  D SITES^PSAUTL1,FIND Q:PSAOUT
 ;
EXIT W:$E(IOST,1,2)="C-" @IOF D ^%ZISC S:$D(ZTQUEUED) ZTREQ="@"
 K IO("Q"),^TMP("PSA",$J),^TMP("PSADRG",$J),^TMP("PSAHIS",$J),PSALOC,PSALOCA,PSALOCN
 ;G:'PSAOUT LOC
EXIT1 K %ZIS,DIC,DIR,DIRUT,DTOUT,DUOUT,PSA50,PSABAD,PSABAD1,PSABAL,PSABDT,PSABDTR,PSACHK,PSACNT,PSACOMB,PSADJT,PSADJDT,PSADLN,PSADRG,PSADRUG,PSADT
 K PSAFIRST,PSAHOLD,PSAHOLDN,PSAIPT,PSAISIT,PSAISITN,PSALN,PSALOC,PSALOCA,PSALOCN,PSANONE,PSAOPT,PSAOSIT,PSAOSITN,PSAOUT,PSAPC,PSAPC1,PSAPCS,PSAPG
 K PSAREA,PSARECT,PSARPDT,PSARPDT,PSARUN,PSAS,PSASEL,PSASITES,PSASS,PSATM,PSATR,PSATR0,PSATRANL,PSATRCNT,PSAWRT,X,Y,ZTDESC,ZTDTH,ZTIO,ZTRTN,ZTSAVE,ZTSK
 Q
FIND ;Finds drugs & puts in alpha order in ^TMP("PSAHIS",$J)
 K ^TMP("PSAHIS",$J),^TMP("PSA",$J),PSAFIRST
 S (PSACNT,PSAPG)=0,PSADRG=""
 F  S PSADRG=$O(^TMP("PSADRG",$J,PSALOC,PSADRG)) Q:PSADRG=""  S (PSABAD,PSA50)=0 F  S PSA50=$O(^TMP("PSADRG",$J,PSALOC,PSADRG,PSA50)) Q:'PSA50  D
 .;I '$G(PSATR),$O(^TMP("PSADRG",$J,PSALOC,PSADRG,0)) S PSA50=$O(^TMP("PSADRG",$J,PSALOC,PSADRG,0))
 .S (PSAFIRST,PSATR)=0 F  S PSATR=+$O(^PSD(58.81,"F",PSA50,PSATR)) Q:'PSATR!(PSAOUT)  D  Q:PSAOUT
 ..S PSATR0=$G(^PSD(58.81,PSATR,0)) Q:$P(PSATR0,"^",3)'=PSALOC
 ..I $P(PSATR0,"^",4)'<PSABDT S ^TMP("PSAHIS",$J,PSADRG,$E($P(PSATR0,"^",4),1,7),PSATR)="" S:'PSAFIRST PSAFIRST=PSATR,^TMP("PSA",$J,PSADRG)=PSATR Q
 ..I PSAFIRST,PSATR>PSAFIRST S PSABAD1=$S($P(PSATR0,"^",2)=2!($P(PSATR0,"^",2)=15)!($P(PSATR0,"^",2)=6):-$P(PSATR0,"^",6),1:$P(PSATR0,"^",6)) S PSABAD(PSADRG)=$G(PSABAD(PSADRG))+PSABAD1
 I $D(^TMP("PSAHIS",$J)) D ^PSAHIS1 Q
 I '$D(^TMP("PSAHIS",$J)) W !!,"No transactions were found for the pharmacy location." H 1
 Q

SOWKOPEN ;B'HAM ISC/SAB ROUTINE TO OPEN CASES & ENTER PATIENTS INTO THE SW PATIENT  ; 10 Mar 94 / 8:28 AM [ 08/07/96  2:14 PM ]
 ;;3.0; Social Work ;**7,21,39,47,52**;27 Apr 93
 S (SOWKCOR,OUT)=0,DIE("NO^")="OUTOK" I '$O(^SOWK(650.1,0)) W !!,*7,"PLEASE ENTER SOCIAL WORK SITE PARAMETERS !!!" G Q
OP K HR W ! S DIC="^DPT(",DIC(0)="AQEM",DIC("A")="SELECT PATIENT: " D ^DIC G:"^"[X Q G:+Y'>0 OP S SWPT=+Y K DIC,Y
 I '$P(^SOWK(650.1,1,0),"^",19),$D(^VA(200,DUZ,654)),$P(^VA(200,DUZ,654),"^"),'$P(^VA(200,DUZ,0),"^",11)!($P(^(0),"^",11)'<DT) S SWSW=DUZ G DIV
WRK W ! S DIC("S")="I $D(^VA(200,+Y,654)),$P(^VA(200,+Y,654),""^""),'$P(^VA(200,+Y,0),""^"",11)!($P(^(0),""^"",11)'<DT)",DIC="^VA(200,",DIC(0)="AEQM",DIC("A")="SELECT WORKER: " D ^DIC G:"^"[X Q G:+Y'>0 WRK S SWSW=+Y K DIC
DIV I $O(^SOWK(650.1,1)) S DIC="^SOWK(650.1,",DIC(0)="AEQM",DIC("A")="SELECT DIVISION: " D ^DIC G:"^"[X Q G:+Y'>0 DIV S SWSITE=+Y K DIC
 S:'$D(SWSITE) SWSITE=1
CDC ;set Cost Distribution Account for case
 K DIR,X,Y,DA S DIR(0)="650,3.1" D ^DIR G:$D(DIRUT)!$D(DIROUT) Q G:Y'>0 CDC S SWCDC=+Y K DIR
ADP D WAIT^DICD,CHECK S SOWKCN=$O(^SOWK(650,"AC",SWPT,SWSW,SWSITE,0))
 ;put check for any open cases for that patient E3R #6389
 I $D(^SOWK(650,"AC",SWPT)) W !!,"Case #",?15,"Open date",?25,"Social Worker",?55,"Division",! F SRX=1:1:75 W "-"
 I $D(^SOWK(650,"AC",SWPT)) S SWX=0 F  S SWX=$O(^SOWK(650,"AC",SWPT,SWX)) Q:'SWX  S SWX2=0 F  S SWX2=$O(^(SWX,SWX2)) Q:'SWX2  S SOWKIEN=$O(^SOWK(650,"AC",SWPT,SWX,SWX2,0)) D:SOWKIEN
 .W !,SOWKIEN,?15,$S($P(^SOWK(650,SOWKIEN,0),U,2)'="":$E($P(^(0),U,2),4,5)_"/"_$E($P(^(0),U,2),6,7)_"/"_$E($P(^(0),U,2),2,3),1:"UNKNOWN"),?25,$P(^VA(200,SWX,0),U),?55,$P(^SOWK(650.1,SWX2,0),U)
 I SOWKCN W !!,*7,"CASE FOR ",$P(^DPT(SWPT,0),"^")," IS ALREADY OPENED BY ",$P(^VA(200,SWSW,0),"^")," ON ",$S($P(^SOWK(650,SOWKCN,0),U,2):$E($P(^(0),U,2),4,5)_"/"_$E($P(^(0),U,2),6,7)_"/"_$E($P(^(0),U,2),2,3),1:"UNKNOWN"),! K SOWKCN G OP
HR K SOWKCN I $D(^SOWK(655,SWPT)),$P(^SOWK(655,SWPT,0),"^",5)="HR",$P(^(0),"^",6)="" D FA
SEA S (DIE,DIC)="^SOWK(650,",DIC(0)="L",DIC("DR")="2////"_SWSW_";4////"_SWSITE_";7////"_SWPT_";3.1////"_SWCDC,DLAYGO=650
 K DD,DO L +^SOWK(650,0) S ND=^SOWK(650,0) F X=$P(ND,"^",3)+1:1 I '$D(^SOWK(650,X,0)) S DINUM=X D FILE^DICN S DA=+Y K DIC,DLAYGO L -^SOWK(650,0) Q
 S:DA ^SOWK(650,"AC",SWPT,SWSW,SWSITE,DA)=""
SC S SOWKFLAG=0,DR="[SOWKOPEN]" W ! D ^DIE W ! S PN=DA,AL=+$P(^SOWK(650,DA,0),"^",13),SWSW=$P(^(0),"^",3) I SOWKFLAG W !!,"HOMES MUST BE ADDED.  THIS ENTRY WILL BE DELETED" S DIK="^SOWK(650," D ^DIK K DIK G K
 I $D(Y) D REC G K
 I $D(^SOWK(651,+$P(^SOWK(650,DA,0),"^",13),0)),$P(^(0),"^",6)["R" D RCH I SOWKFLAG G K
CL F Q=0:0 W !!,"DO YOU WANT TO CLOSE CASE" S %=2 D YN^DICN Q:%  I %Y["?" D YN^SOWKHELP
 G:%=2 OC G:%=-1 Q
 S DA=PN,DIE="^SOWK(650,",DR="[SOWKCLOT]" D ^DIE I $D(Y) G REC
 K SWA,^SOWK(650,"AC",SWPT,SWSW,SWSITE,DA)
 I $P(^SOWK(651,AL,0),"^",6)="R" F A=0:0 S A=$O(^SOWK(655,SWPT,4,A)) Q:'A!(OUT)  I $P(^SOWK(655,SWPT,4,A,0),"^",5)=PN,'$P(^(0),"^",6) D EDT
K K AL,PN,SWSW,DA,DIC,DIE,DR,HR,SWA,SWPT,X,Y,A,DINUM,DUP,DTOUT,DUOUT,ND,SOWKFLAG S DIE("NO^")="OUTOK" G OC
Q K AL,DTOUT,DUOUT,HR,ND,SWSITE,PN,SWSW,DA,DIC,DIE,DR,I,SWA,SWPT,X,Y,SWCDC,SWBDT,SOWKFLAG,SOWKCOR,A,DINUM,SUP,%,%Y,Q Q
OC F Q=0:0 W !!!,"DO YOU WANT TO OPEN ANOTHER NEW CASE" S %=2 D YN^DICN Q:%  I %Y["?" D YN^SOWKHELP
 G:%=2!(%=-1) Q G OP
FA F Q=0:0 W !,"Was this a High Risk Patient" S %=2 D YN^DICN Q:%  I %Y["?" D YN^SOWKHELP
 Q:%=-1
 S HR=^SOWK(655,SWPT,0)
 S:%=2 $P(^SOWK(655,SWPT,0),"^",6)="S",$P(^(0),"^",7)="F" S:%=1 $P(^SOWK(655,SWPT,0),"^",6)="S"
 Q
RCH I '$D(^SOWK(655,SWPT)) S DLAYGO=655,(X,DINUM)=SWPT,DIC(0)="L",DIC="^SOWK(655," K DD,DO D FILE^DICN K DLAYGO
 S DA=SWPT,DIE="^SOWK(655,",DR=".01;1" W ! D ^DIE I $D(Y) S SOWKFLAG=1 G REC
 S DIC="^SOWK(652,",DIC(0)="AEMQ",DIC("A")="SELECT HOME: " D ^DIC S:Y<1 SOWKFLAG=1 G:Y<1 REC I '$D(^SOWK(655,SWPT,4,0)) S ^(0)="^655.02P^0^0"
 S DA=+Y,DA(1)=SWPT,DIC="^SOWK(655,"_DA(1)_",4,",DIC("DR")=".01;1;2;5"_"///"_PN,DIC(0)="L",(DA,X)=+Y K DD,DO D FILE^DICN K DIC("DR") I $D(DTOUT)!$D(DUOUT)!(Y<1) S SOWKFLAG=1 G REC
 Q
REC W !!,*7,"INCOMPLETE DATA!!  RECORD DELETED." G:$G(PN)="" Q S DA=PN,DIK="^SOWK(650," D ^DIK K DIK
 I AL,$P(^SOWK(651,AL,0),"^",6)="R" F A=0:0 S A=$O(^SOWK(655,SWPT,4,A)) Q:'A  I $P(^SOWK(655,SWPT,4,A,0),"^",5)=PN!($P(^SOWK(655,SWPT,4,A,0),"^",5)="") S DA=A,DA(1)=SWPT,DIK="^SOWK(655,"_DA(1)_",4," D ^DIK
 I $D(HR) S ^SOWK(655,SWPT,0)=HR K HR
 Q
EDT D DISP Q:OUT  S DA=A,DA(1)=SWPT,DIE="^SOWK(655,"_DA(1)_",4,",DR="3;I 'X S Y=""@4"";4;@4" D ^DIE I $D(Y) S DIK=DIE D ^DIK S DA=PN D REC Q
 S DA=SWPT,DIE="^SOWK(655,",DR="3;I 'X S Y=""@4"";2;@4" D ^DIE I $D(Y) D REC Q
 Q
CHECK ;cleanup 'AC' xref
 S SHEMP=0 F  S SHEMP=$O(^SOWK(650,"AC",SWPT,SWSW,SWSITE,SHEMP)) Q:'SHEMP  I '$D(^SOWK(650,SHEMP,0)) K ^SOWK(650,"AC",SWPT,SWSW,SWSITE,SHEMP)
 K SHEMP
 Q
DISP ;disposition from RCH
 S DIE=650,DR="20",DA=PN D ^DIE K DIE I $D(Y) S OUT=1 D REC
 Q

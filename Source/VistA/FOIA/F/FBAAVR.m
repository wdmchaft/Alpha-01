FBAAVR ;AISC/GRR-FINALIZE BATCH ;01JAN86
 ;;3.5;FEE BASIS;;JAN 30, 1995
 ;;Per VHA Directive 10-93-142, this routine should not be modified.
 S Q="",$P(Q,"=",80)="=",IOP=$S($D(ION):ION,1:"HOME") D ^%ZIS
 D DT^DICRW
BT K DIC S (FBRFLAG,FBINTOT)=0 K QQ W !! S DIC="^FBAA(161.7,",DIC(0)="AEQ",DIC("S")="I $G(^(""ST""))=""T""!($G(^(""ST""))=""V"")" D ^DIC K DIC("S") G Q:X="^"!(X=""),BT:Y<0 S FBN=+Y,FZ=^FBAA(161.7,FBN,0),FBTYPE=$P(FZ,"^",3)
 I FBTYPE="B9",$P(FZ,"^",15)="" S FBCNH=1
 I '$D(^XUSEC("FBAASUPERVISOR",DUZ)) W !!,*7,"Sorry, only Supervisor can Finalize batch!" G Q
 S FBAAON=$P(FZ,"^",2),FBAARA=0,FBAAB=$P(FZ,"^"),FBAAOB=$P(FZ,"^",8)_"-"_FBAAON,FBCOMM="Rejected items from batch "_FBAAB
 I $G(^FBAA(161.7,FBN,"ST"))'="T",$G(^("ST"))'="V" G ERR^FBAAVR0
 S DIC="^FBAA(161.7,",DA=FBN,DR="0;ST" W !! D EN^DIQ
BTL S B=FBN S DIR(0)="Y",DIR("A")="Want line items listed",DIR("B")="NO" D ^DIR K DIR G Q:$D(DIRUT) W:Y @IOF D:Y LIST^FBAACCB:FBTYPE="B3",LISTP^FBAACCB:FBTYPE="B5",LISTT^FBAACCB0:FBTYPE="B2",LISTC^FBAACCB1:FBTYPE="B9"
RD0 S DIR(0)="Y",DIR("A")="Want to reject the entire Batch",DIR("B")="NO",DIR("?")="'Yes' will flag all payment items in batch as rejected, 'No' will prompt for rejection of specific line items." D ^DIR K DIR G Q:$D(DIRUT),^FBAADD:Y
RD1 S DIR(0)="Y",DIR("A")="Want to reject any line items",DIR("B")="NO" D ^DIR K DIR G Q:$D(DIRUT)
 I Y,($P(FZ,U,9)'>0!($P(FZ,U,11)'>0)) G NOLINE^FBAADD
 I $G(Y) D CK1358^FBAAUTL1 G Q:$D(FBERR) D DELT^FBAAVR1:FBTYPE="B2",DELM^FBAAVR2:FBTYPE="B3",DELP^FBAAVR1:FBTYPE="B5",DELC^FBAAVR0:FBTYPE="B9"
RDD S DIC="^FBAA(161.7,",DA=FBN,DR="0;ST" W !! D EN^DIQ
RDD1 D:FBRFLAG PLUSOB^FBAAUTL1 S FBRFLAG=0 S DIR(0)="Y",DIR("A")="Do you want to Finalize Batch as Correct",DIR("B")="NO" D ^DIR K DIR  G Q:$D(DIRUT) W:'Y&($P($G(^FBAA(161.7,FBN,"ST")),U)'="V") !!,"Batch has NOT been Finalized!",*7 G BT:'Y
 D MEDV:FBTYPE="B3",VCHNH^FBAAVR0:FBTYPE="B9"
FIN S DA=FBN,(DIC,DIE)="^FBAA(161.7,",DIC(0)="LQ",DR="13////^S X=DT;14////^S X=DUZ;11////^S X=""V""",DLAYGO=161.7 D ^DIE K DIE,DIC,DA,DLAYGO
 W !!," Batch has been Finalized!" D Q G FBAAVR
Q K B,J,K,L,M,X,Y,Z,DIC,ERR,FBN,FBAAOUT,FBAC,FBAP,FBFD,FBPDT,FBSC,FBTD,FBVP,POP,FBRFLAG,Q,QQ,A,A1,A2,DO,DA,DL,DR,DRX,DX,FBAAAP,FBAACB,FBAACPT,FBAAON,FBAARA,FBINTOT,FBIN,FBRR,FBTYPE,FZ,HX,I,P3,P4,S,V,VAL,VID,XY,ZS,FBAAB,FBAAOB,DIRUT
 K FBAAON,FBCOMM,FBERR,FBI,FBLIST,PRCS("TYPE"),FBLISTC,FBINOLD,FBDX,FBK,FBL,FBPROC,FBCNH,FBAAMT,FBII78
 Q
MEDV F J=0:0 S J=$O(^FBAAC("AC",B,J)) Q:J'>0  F K=0:0 S K=$O(^FBAAC("AC",B,J,K)) Q:K'>0  F L=0:0 S L=$O(^FBAAC("AC",B,J,K,L)) Q:L'>0  F M=0:0 S M=$O(^FBAAC("AC",B,J,K,L,M)) Q:M'>0  D SETXFR
 Q
SETXFR I '$D(^FBAAC(J,1,K,1,L,1,M,"FBREJ")),$D(^FBAAC(J,1,K,1,L,1,M,0)) S DA(3)=J,DA(2)=K,DA(1)=L,DA=M,DIE="^FBAAC(DA(3),1,DA(2),1,DA(1),1,",DR="5///^S X=DT" D ^DIE K DIE,DA,DR
 Q

RMPRFO2 ;PHX/RFM,HPL-CONTINUATION OF RMPRFO1 ;11/01/1994
 ;;3.0;PROSTHETICS;**77,105**;Feb 09, 1996
 ;
 ;RVD patch #77 - check for variable %
VIEW ;VIEW LETTERS FROM ELIG SCREEN2 UNDER ISSUE FROM STOCK
 N RMPRDA,DA,RMPRIN,RMPRAA68
 S:$D(%) RMPRAA68=%
 ;Q:'$D(RMPRAA68)
 Q:$G(DFN)=""
 S RMPRFF=1 I '$D(^RMPR(665.4,"B",DFN)) G RO1
 K KILL
 W !!,"Letters on file:"
 ;!,?5,"Type",?29,"Employee",?55,"Date or Vendor"
ASK1 ;SET UP REVERSE LETTER LIST & ASK IF USER WANTS TO VIEW MORE LETTERS
 D EN^RMPRUTL2
 S DA=RMPRIN G:$G(DA)'>0 ASK2
 S RMPRIN=DA D PRINT^RMPRFO1
MOLET K DA,RMPRDA,RMPRIN
 S %=2 W !,"Would you like to see more letters" D YN^DICN
 S RMPRAA68=% I RMPRAA68=-1 S KILL=1 Q
 I RMPRAA68=0 W !,"'YES' will let you review another letter for this patient",!,"'NO' will let you continue the program"
 I  W !,"Enter '^' to exit the correspondence screen totally" G MOLET
 I RMPRAA68=2 S KILL=1 D RO1 Q
 I RMPRAA68=1 G ASK1
 G RO1
ASK2 ;Q:RMPRIN=-1
 S %=2 W !,"Do you wish to view a letter" D YN^DICN Q:$D(DTOUT)  S RMPRAA68=% S:RMPRAA68<0 KILL=1 G:RMPRAA68=2!(RMPRAA68<0) RO1
 I RMPRAA68=0 W !,"Answer `YES` or `NO`" G ASK2
 I RMPRAA68=1 G VIEW
 ;I %=1 S %=3 G VIEW
ASK3 S RMPRAA68=% I RMPRAA68=2 K X R !!,"Enter the number: ",X:DTIME Q:'$T!(X="")  Q:X="^"  I X>(I-1)!(+X<1)!(X'?1N.N) W !,$C(7),"Enter a number between 1 and ",(I-1)_" or `^` to quit." G ASK3
 I RMPRAA68=1 I $G(X)'="" I $G(RMPR9VA($G(X)))=""&($G(RMPRDFN)'="") S RMPR9VA($G(X))=RMPRDFN
 I RMPRAA68=1,$D(^RMPR(665.4,RMPR9VA(+X),0)) S RMPRIN=RMPR9VA(+X),RMPREN=1 D PRINT^RMPRFO1 G VIEW
RO1 K RMPREN S %=2 W !,"Do you wish to create a correspondence letter" D YN^DICN
 S RMPRAA68=% I RMPRAA68=1 D CUM^RMPRFO Q
 S RMPRAA68=% I RMPRAA68=0 W !,"Answer `YES` to create a form letter, `NO` to continue." G RO1
 I RMPRAA68=2 K RMPRBB,RMPRFF,RMPR9ZRO,RMPR9VA(1),RMPR9VA(2)
 S RMPRAA68=3 Q
EN4 ;EDIT A SKELETON
 K DIC S DIC="^RMPR(665.2,",DIC(0)="AEQLM",DLAYGO=665.2 D ^DIC K DLAYGO
 I +Y<0!($P(Y,U,4)["1") W !!,"SORRY, THIS IS A NON-EDITABLE LETTER" G EXIT^RMPRFO1
 S RMPRIN=+Y L +^RMPR(665.2,RMPRIN,0):1 I $T=0 W !,$C(7),?5,"Someone else is Editing this entry!" K RMPRIN G EXIT^RMPRFO1
 S DIE="^RMPR(665.2,",DA=RMPRIN,DR=".01;1",DIE("NO^")="" D ^DIE L -^RMPR(665.2,RMPRIN,0) I '$D(DA)!($D(DTOUT))!($D(DUOUT)) G EXIT^RMPRFO1
DEN S %=$S($P(^RMPR(665.2,RMPRIN,0),U,2)=1:1,1:2) W !,"Is this a Denial type of letter" D YN^DICN
 S RMPRAA68=% G:RMPRAA68<0 EXIT^RMPRFO1
 I RMPRAA68=0 G QUES1
 S $P(^RMPR(665.2,RMPRIN,0),U,2)=$S(RMPRAA68=2:0,RMPRAA68=1:1,1:"")
 G EXIT^RMPRFO1
QUES1 W !,"Enter `YES` if letter is an AMIS Denial" G DEN
EN3 ;PRINT FORM LETTER
 I '$D(RMPR("SIG")) D DIV4^RMPRSIT Q:$D(X)
 D HOME^%ZIS
 ;CHECK IF IT IS THE ADP FL 10-90
 K DIC S DIC="^RMPR(665.2,",DIC(0)="AEQM" D ^DIC G:+Y<0 EXIT^RMPRFO1
 S RMPRIN=+Y K DIC
 ;check if it is the ADP FL 10-90
 I $P(^RMPR(665.2,RMPRIN,0),U,4)["1" K DA D PRNT1^RMPRFO3 D EXIT^RMPRFO1 Q
PR S DIWF="^RMPR(665.2,RMPRIN,1,",DIWF(1)=665.2,BY="@NUMBER",FR=RMPRIN,TO=RMPRIN D EN2^DIWF
 G EXIT^RMPRFO1
SET K DIC S DIC="^RMPR(665.4,",DIC(0)="L",X=DFN,DLAYGO=665.4 K DD,DO,DINUM D FILE^DICN K DLAYGO
 G:Y<0 EXIT^RMPRFO1
 S RMPRIN=+Y,$P(^RMPR(665.4,RMPRIN,0),U,2)=RMPRFA,$P(^(0),U,3)=DT,$P(^(0),U,4)=DUZ,$P(^RMPR(665.4,RMPRIN,0),U,5)=$P(^RMPR(665.2,RMPRFA,0),U,2),$P(^RMPR(665.4,RMPRIN,0),U,6)=RMPR("STA") S DIK=DIC,DA=RMPRIN D IX1^DIK
 S %X="^TMP($J,1,",%Y="^RMPR(665.4,+Y,1," D %XY^%RCR
 G PRINT^RMPRFO1
WRITE S:$G(RMPR9ZRO)'=""&(RO="") RO=RMPR9ZRO
 I I#15=0 S DIR(0)="FAOU^1:245",DIR("A")="End of page: select a letter by number or enter'^' to continue listing" D  I $G(X)="^" Q
 .D ^DIR
 .I $G(X)="" Q
 .I $G(X)>0&($G(RO)>0&($G(X)<(RO+1))) S DA=^TMP($J,"RMPR",RO) W !,"***",DA Q
 W !,I_" ",?4,$S($D(^RMPR(665.2,+$P(^RMPR(665.4,^TMP($J,"RMPR",RO),0),U,2),0)):$E($P(^(0),U,1),1,20),1:"UNKNOWN")
 S:$D(^RMPR(665.4,^TMP($J,"RMPR",RO),2)) RMPR2=$P(^RMPR(665.4,^TMP($J,"RMPR",RO),2),U,1)
 ;W ?27,$S($D(^VA(200,+$P(^RMPR(665.4,^TMP($J,"RMPR",RO),0),U,4),0)):$E($P(^(0),U),1,15),1:"")
 S RMPRPP=$G(^VA(200,+$P(^RMPR(665.4,^TMP($J,"RMPR",RO),0),U,4),0))'="" W ?27,$E($P(^(0),U),1,15) K RMPRPP
 S Y=$S($P(^RMPR(665.4,^TMP($J,"RMPR",RO),0),U,3):$P(^RMPR(665.4,^TMP($J,"RMPR",RO),0),U,3),$D(RMPR2):$P(^PRC(440,RMPR2,0),U,1),1:"") D DD^%DT W ?55,$E(Y,1,24) S RMPR9VA(I)=^TMP($J,"RMPR",RO)
 Q

PRCFAC2 ;WISC/CTB-PROCESS RECEIVING REPORTS ;3/30/93  09:38
V ;;5.1;IFCAP;;Oct 20, 2000
 ;Per VHA Directive 10-93-142, this routine should not be modified.
EN8 ;PROCESSING OF RECEIVING REPORT
 S PRCFASYS="",PRCF("X")="AS" D ^PRCFSITE G:'% OUT K DIC("A")
 S D="C",DIC("S")="I +$P(^(0),U,1)=PRC(""SITE""),$D(^(7)),+^(7)>0 S FSO=$P(^PRCD(442.3,+^(7),0),U,3) I FSO>29,FSO<40"
 S DIC("A")="Select Purchase Order Number: ",DIC=442,DIC(0)="AEQZ" D IX^DIC
 K DIC("S"),DIC("A"),FSO G:+Y<0 OUT S PO(0)=Y(0),PRCFA("PODA")=+Y,PO=Y,DIC("A")="Partial Number: ",DIC="^PRC(442,"_+PO_",11,",DIC(0)="AEQMNZ" D ^DIC K DIC("A")
 G:Y<0 OUT S PO(11)=Y(0),PRCFA("PARTIAL")=+Y
 I $P(PO(11),U,6)="Y" W $C(7) S %A="A CODE SHEET HAS ALREADY BEEN COMPLETED FOR THIS PARTIAL,",%A(1)="DO YOU WISH TO CONTINUE",%B="" S %=2 D ^PRCFYN I %'=1 K P,DIC,Y G OUT
 S PO(2)=$P(PO(11),"^")\1 I $P(PO(0),"^",19)=2!($P(PO(0),"^",19)=3) G X
C S PRCFA("TT")="924.00" D TT^PRCFAC G OUT:'%
 S PRCFA("TTDATE")=$E(PO(2),4,7)_$E(PO(2),2,3),PRCFA("REF")=$P($P(PO(0),"^"),"-",2) D NEWCS^PRCFAC G:'$D(DA) OUT
 S X="^^^^^^^"_+$P(PO(11),U,2)_U_$S($P(PO(11),U,5)=0:"",1:$J($P(PO(11),U,3)*100,0,0))_U
 S ^PRCF(423,DA,1)=X_$S($P(PO(11),U,4)="":"$",1:+$P(PO(11),U,4))_U_$S($P(PO(11),U,5)=0:"",1:$J($P(PO(11),U,5)*100,0,0))_U_$S("30~31~15"[$P(^PRC(442,+PO,7),U):"P",1:"")_"^^^"
 S $P(^PRCF(423,DA,1),"^",16)="$"
 I PRCFA("EDIT")'["921." S DR=".1;17;S:X]"""" PRCFA(""LIQ"")=X",PRCFA("CSDA")=DA,DIE="^PRCF(423," D ^DIE
 I PRCFA("EDIT")["924.00" D ^PRCFA924 G EN82
 S DIE="^PRCF(423,",DR=PRCFA("EDIT"),DA=PRCFA("CSDA") D ^DIE K PRCFA("DIE") G:$D(Y)=0 EN82
 W !,$C(7) S %A="THIS CODE SHEET WAS ABORTED.  DATA MAY BE INCORRECT.",%A(1)="DO YOU WISH TO DELETE",%B="Failure to delete could cause incorrect data to be transmitted",%B(1)="A 'YES' or an '^' will delete the code sheet"
EN82 S PRCFA("REC")="" D ^PRCFACXM I $D(PRCFDEL)!($D(PRCFA("CSHOLD"))) K PRCFDEL,PRCFA("CSHOLD") S X="  No further processing is being taken on this receiving report.*" D MSG^PRCFQ G OUT8
 I $G(PRCFA("PODA"))>0 D EN72^PRCFAC1,LOAD^PRCFARRQ
OUT8 K PRCFA("PODA"),PRCFA("REC"),PRCFA("PARTIAL") G EN8
X W !,"LIQUIDATION CODE: " R X:DTIME G OUT8:'$T,OUT8:X["^"
 I "PCF"'[$E(X)!(X="") W ! S X="Enter a (P)artial, (F)inal, or (C)omplete only.*" D MSG^PRCFQ G X
 S PRCFA("LIQ")=$E(X),X="Since this is a "_$S($P(PO(0),"^",19)=3:"CASCA",1:"SUPPLY FUND")_" receiving report, no code sheet is required.*"
 D MSG^PRCFQ,EN72^PRCFAC1 I $P(PO(0),"^",19)=2,$G(PRCFA("PODA"))>0 D LOAD^PRCFARRQ
 K PRCFA("PODA"),PRCFA("REC"),PRCFA("PARTIAL") G EN8
EN9 ;DELETE A CODE SHEET IF NOT PRINTED
 S:'$D(PRCFASYS) PRCFASYS="FEEFENIRSISMCLI" K Q1 S DIC="^PRCF(423,",DIC(0)="AEMNQ",DIC("S")="I $P(^(0),U,10)]"""",PRCFASYS[$P(^(0),U,10)" D ^DIC K DIC("A") I Y<0 K PRCFASYS G OUT9
 S DA=+Y W !,$C(7) S %A="ARE YOU SURE",%B="ANSWERING 'YES' WILL CAUSE ALL REFERENCE TO THIS CODE SHEET TO BE DELETED" S %=2 D ^PRCFYN I %'=1 W ?$X+5,"<NOTHING DELETED>",$C(7) R X:2 D OUT9 G EN9
 D DEL,OUT9 S DIC("A")="Select Next CODE SHEET ID: " G EN9
OUT9 K %,DA,DIC,I,J,K,X,Y Q
DEL ;KILL THE CODE SHEET AND CROSS REFERENCES
 S DIK="^PRCF(423," D WAIT^PRCFYN,^DIK S PRCFDEL="" W $C(7),"   <CODE SHEET DELETED>" R X:3
OUT K %,%Y,B,D0,DA,DG,DIC,DIE,DIG,DIH,DIK,DIR,DIU,DIV,DIW,DLAYGO,DR,FSO,J,K,P,PRCFA,Q,Q1,S,X,Y Q
EN1 ;MODIFY BATCH PRIORITY OF CODE SHEET
 S:'$D(PRCFASYS) PRCFASYS="FEEFENIRSISMCLI" S DIC="^PRCF(423,",DIC(0)="AEMNQ",DIC("S")="I $P(^(0),U,10)]"""",PRCFASYS[$P(^(0),U,10)" D ^DIC K DIC("A") I Y<0 K PRCFASYS G OUT1
 S DA=+Y,DIE=DIC,DR=".8;" D ^DIE G EN1
OUT1 K %,%Y,D0,DA,DIC,DIE,DQ,DR,I,J,K,X,Y,Z Q
 Q

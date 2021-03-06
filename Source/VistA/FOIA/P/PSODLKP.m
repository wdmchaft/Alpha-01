PSODLKP ;BHAM ISC/JrR - CREATE/EDIT DUE ANSWER FILE ENTRY ; 11/17/92 10:19
 ;;7.0;OUTPATIENT PHARMACY;**268**;DEC 1997;Build 9
 Q
CREATE ;Create a new DUE ANSWER entry
 W !!
 D NEW
 S PSA=+Y W !,"SEQUENCE NUMBER: ",PSA
 S DIC="^PSRX(",DIC("A")="RX #: ",DIC(0)="QEAMZ"
 D ^DIC K DIC
 I $D(DUOUT)!$D(DTOUT) D DELETE G EXIT
 S RXN=+Y,RX0=$S($D(Y(0)):Y(0),1:""),RXM=$S($D(Y(0,0)):Y(0,0),1:"")
 D STUFF,QAIRE
 I '$D(PSQA) D DELETE G EXIT
 D DIE
EXIT K CNT,D,DA,DIC,DIE,DIK,DINUM,DIR,DIRUT,DIROUT,DLAYGO,DR,DTOUT,DUOUT
 K DZ,FLAG,I,K,L,LL,PZPOP,PSA,PSDFN,PSDIG,PSDRUG,PSHI,PSLEN,PSLO,PSMARG
 K PSPROV,PSQ,PSQA,PSQN,PSQNUM,PSQP,PSTXT,PSTYP,PSWRAP,RX0,RXM,RXN,X,Y
 K PSKIP,PID
 W !! Q
 ;
DIE ;Enter here from PSODLKP,PSODEDT. Edit the DUE Answer sheet
 S DIE="^PS(50.0731,",DA=PSA,DR="[PSOD DUE EDIT]" L +^PS(50.0731,DA):$S(+$G(^DD("DILOCKTM"))>0:+^DD("DILOCKTM"),1:3) I '$T W !,"Entry is being edited by another user. Try Later!" K DA,DR,DIE,PSA Q
 D ^DIE K DIE,DR L -^PS(50.0731,DA) K DA
GETQUES F PSQNUM=0:0 S PSQNUM=$O(^PS(50.0731,PSA,1,"B",PSQNUM)) Q:'PSQNUM  S PSQN=$O(^(PSQNUM,0)),PSQP=$P(^PS(50.0731,PSA,1,PSQN,0),"^",2) I $D(^PS(50.0732,PSQP,0)) S PSQ=^(0) D ASK Q:PZPOP
 Q
ASK S PZPOP=0
 D WRAP^PSODEDT
 S PSTYP=$S($P(PSQ,"^",2):$P(PSQ,"^",2),1:1),PSLO=$S($P(PSQ,"^",3)]"":$P(PSQ,"^",3),1:-999),PSHI=$S($P(PSQ,"^",4)]"":$P(PSQ,"^",4),1:999)
 S PSDIG=$S($P(PSQ,"^",5)]"":$P(PSQ,"^",5),1:2),PSLEN=$S($P(PSQ,"^",6)]"":$P(PSQ,"^",6),1:70)
 S DIR("??")="^D QUES2^PSODEDT",DIR("A")="    ANSWER: "
 S DIR(0)=$S(PSTYP=1:"S^Y:YES;N:NO;U:UNKNOWN",PSTYP=2:"F^1:"_PSLEN,PSTYP=3:"N^"_PSLO_":"_PSHI_":"_PSDIG,1:"Y")
 S $P(DIR(0),"^")=$P(DIR(0),"^")_"AO"
 K DIR("B")
 I $D(^PS(50.0731,PSA,1,PSQN,1)),^(1)]"" S DIR("B")=^(1)
 D ^DIR K DIR
 I $D(DUOUT)!$D(DTOUT) S PZPOP=1 Q
 S X=$S($D(Y(0)):Y(0),1:Y)
 S ^PS(50.0731,PSA,1,PSQN,1)=X
 Q
 ;
NEW L +^PS(50.0731,0):$S(+$G(^DD("DILOCKTM"))>0:+^DD("DILOCKTM"),1:3) E  W $C(7),!,"Trying to Lock ^PS(50.0731,0)" G NEW
 S X=$P(^PS(50.0731,0),"^",3)
LOOP S X=X+1 G:$D(^PS(50.0731,X)) LOOP
 K DIC,DD,DO S DIC="^PS(50.0731,",DIC(0)="XL",DIC("DR")="6///NOW"_$S($D(DUZ)#2:";5////"_DUZ,1:""),DLAYGO=50.0731,DINUM=X D FILE^DICN L -^PS(50.0731,0)
 K DIC,DLAYGO,DINUM
 Q:$P(Y,"^",3)
 G NEW
 ;
QAIRE K PSQA,DA S DIR(0)="50.0731,1" D ^DIR K DIR
 Q:$D(DUOUT)!$D(DTOUT)
 I 'Y W !,$C(7),"   REQUIRED!" G QAIRE
 I $S('$D(^PS(50.073,+Y,2,0)):1,'$O(^(0)):1,1:0) W !!,"  Sorry, that Questionnaire is incomplete.",!,"  Please review it before proceeding!" Q
 S PSQA=+Y,$P(^PS(50.0731,PSA,0),"^",2)=PSQA
MOVE S FLAG=0
 F I=0:0 S I=$O(^PS(50.073,PSQA,2,I)) Q:'I  S:$D(^PS(50.0732,$P(^(I,0),"^",2),0)) ^PS(50.0731,PSA,1,I,0)=^PS(50.073,PSQA,2,I,0),$P(^PS(50.0732,$P(^(0),"^",2),0),"^",7)=1,FLAG=1
 S:FLAG $P(^PS(50.073,PSQA,0),"^",4)=1,^PS(50.0731,PSA,1,0)="^50.07311IA^"_$P(^PS(50.073,PSQA,2,0),"^",3,4)
 ;S DIK="^PS(50.0731,"_PSA_",1,",DA(1)=PSA D IXALL^DIK K DIK,DA
 S DIK="^PS(50.0731,",DA=PSA D IX^DIK K DIK,DA
 K FLAG
 Q
STUFF K PSKIP
 Q:RXN<1
 S PSKIP=""
 S PSDRUG=$P(RX0,"^",6),PSPROV=$P(RX0,"^",4),PSDFN=$P(RX0,"^",2)
 S DIE="^PS(50.0731,",DA=PSA,DR="2////"_PSDRUG_";3////"_RXN_";4////"_PSPROV_";7////"_PSDFN_";10////"_PSOSITE D ^DIE K DIE,DA,DR
 S Y=PSDRUG,C=$P(^DD(50.0731,2,0),"^",2) D Y^DIQ W:Y]"" !,"DRUG: ",Y
 S Y=PSDFN,C=$P(^DD(50.0731,7,0),"^",2) D Y^DIQ W:Y]"" !,"PATIENT: ",Y
 Q:'$D(^PS(50.073,"AD",PSDRUG))
 S CNT=0 F L=0:0 S L=$O(^PS(50.073,"AD",PSDRUG,L)) Q:'L  I $P(^PS(50.073,L,0),"^",3) S CNT=CNT+1,LL=L
 I CNT=1 S DIR("B")=$P(^PS(50.073,LL,0),"^") Q
 W !?5,"This Drug requires the following Active Questionnaires:"
 S DIC="^PS(50.073,",DIC(0)="QEM",D="B",DZ="??",DIC("S")="I $D(^PS(50.073,""AD"",PSDRUG,Y))&($P(^PS(50.073,Y,0),""^"",3))" D DQ^DICQ K DIC,D,DZ
 Q
DELETE W $C(7),!,"Deleting SEQUENCE NUMBER: ",PSA
 S DA=PSA,DIK="^PS(50.0731," D ^DIK
 Q
QUES2 Q  I PSTYP=1 W !!,?5,"Enter Y for YES, N for NO, U for UNKNOWN."
 I PSTYP=2 W !!,?5,"Enter a FREE TEXT answer from 1 to ",PSLEN," characters."
 I PSTYP=3 W !!,?5,"Enter a number between ",PSLO," and ",PSHI,!,?5,"with a maximum of ",PSDIG," decimal digits."
 W !?5,"Enter '^' to bypass."
 D WRAP^PSODEDT
 Q
CHECK ;CHECK FOR DRUG MATCH FROM ORDER ENTRY
 F PSODDRG=0:0 S PSODDRG=$O(^PS(50.073,"AD",PSODDRG)) Q:'PSODDRG  I PSODDRG=$P(^PSRX(PSONEW("IRXN"),0),"^",6) D CHECK1
 Q
CHECK1 F PSOST=0:0 S PSOST=$O(^PS(50.073,"AD",PSODDRG,PSOST)) Q:'PSOST  S PSOSTE=$P(^PS(50.073,PSOST,0),"^",5) Q:PSOSITE'=PSOSTE  S RXN=PSONEW("IRXN"),RX0=^PSRX(RXN,0) D CREATE1,EXIT
 Q
CREATE1 ;Create a new DUE ANSWER entry
 W !!
 D NEW
 S PSA=+Y W !,"SEQUENCE NUMBER: ",PSA
 S (RX0,RXM)=$S($D(^PSRX(RXN,0)):^(0),1:"")
 D STUFF,QAIRE
 I '$D(PSQA) D DELETE G EXIT
 D DIE
 Q

PRCFACP ;WISC/CTB/BGJ-BATCH PRINT CODE SHEETS ;2/18/93  14:49
V ;;5.1;IFCAP;;Oct 20, 2000
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 S PRCFASYS="CLMFEEFENIRSCLI"
EN S PRCF("X")="AS" D ^PRCFSITE G:'% OUT
 S ZTDESC="BATCH AND PRINT CODE SHEETS",ZTRTN="V^PRCFACP1",ZTSAVE("PRC*")="",ZTSAVE("PRCFASYS")="" D ^PRCFQ Q
 Q
COUNTER ;RETREIVE NEXT COUNTER NUMBER FROM FILE 422.2  NEEDS VARIABLE X AS INPUT RETURNS NEXT NUMBER IN VARIABLE Y
 K DA S Y=-1 Q:'$D(X)  Q:X=""
 I '$D(^PRCF(422.2,"B",X)) S DIC=422.2,DIC(0)="MLX",DLAYGO=422.2 D ^DIC K DIC,DLAYGO Q:Y<0  S DA=+Y G LOCK
 S DA=$O(^PRCF(422.2,"B",X,0))
LOCK L +^PRCF(422.2,DA):60 G COUNTER:'$T S Y=$P(^PRCF(422.2,DA,0),"^",2)+1 I $D(PRCFLN),Y>PRCFLN S Y=1 K PRCFLN
 S $P(^PRCF(422.2,DA,0),"^",2)=Y L -^PRCF(422.2,DA) Q
OUT W $C(7),"  <OPTION ABORTED>" R X:5 Q
LOG S PRCFASYS="LOG" G EN

PSGWLSI2 ;BHAM ISC/PTD,CML-Print Stock Items in Alphabetical Order ; 30 Aug 93 / 10:15 AM
 ;;2.3; Automatic Replenishment/Ward Stock ;;4 JAN 94
ENQ ;ENTRY POINT WHEN QUEUED
 K ^TMP("PSGWSTK",$J) S PGCT=1,$P(LN,"-",132)="",AOU=0,OUT=0
AOU S AOU=$O(AOULP(AOU)) G:'AOU PRINT
DRUG ;LOOP THROUGH DRUGS FOR AOU
 S DRGDA=0
DRGLP S DRGDA=$O(^PSI(58.1,AOU,1,DRGDA)) G:'DRGDA AOU S DRGNM=$P(^PSI(58.1,AOU,1,DRGDA,0),"^"),DRGNAME=$P(^PSDRUG(DRGNM,0),"^")
INACT I $P(^PSI(58.1,AOU,1,DRGDA,0),"^",10)="Y",$P(^(0),"^",3)="" S $P(^(0),"^",10)=""
 I $P(^PSI(58.1,AOU,1,DRGDA,0),"^",3)'="" D NOW^%DTC S Y=$P(%,".") I $P(^(0),"^",3)'>Y G DRGLP
SETGL S LOCN=$P(^PSI(58.1,AOU,1,DRGDA,0),"^",8),STLEV=$P(^(0),"^",2),RELEV=$P(^(0),"^",11),MIN=$P(^(0),"^",12),EXP="" I $D(^PSI(58.1,AOU,1,DRGDA,"EXP")),^("EXP") S EXP=^("EXP")
 S ^TMP("PSGWSTK",$J,AOU,DRGNAME)=LOCN_"^"_STLEV_"^"_RELEV_"^"_MIN_"^"_EXP G DRGLP
 ;
PRINT S AOU=0
AOULP S AOU=$O(^TMP("PSGWSTK",$J,AOU)) G:'AOU DONE D HDR G:OUT END W !?2,"==> ",$P(^PSI(58.1,AOU,0),"^") S DRG=0
DRLOOP S DRG=$O(^TMP("PSGWSTK",$J,AOU,DRG)) G:DRG="" AOULP S LOC=^TMP("PSGWSTK",$J,AOU,DRG)
 D:$Y>(IOSL-6) HDR G:OUT END W !?8,DRG,?55,$S($P(LOC,"^")'="":$P(LOC,"^"),1:"NOT LISTED"),?74,$S($P(LOC,"^",2)'="":$J($P(LOC,"^",2),6),1:"NOT LISTED")
 W ?89,$S($P(LOC,"^",3)'="":$J($P(LOC,"^",3),6),1:"NOT LISTED"),?106,$S(+$P(LOC,"^",4):$J($P(LOC,"^",4),6),1:"NOT LISTED") S EXP=$P(LOC,"^",5) I EXP S Y=EXP X ^DD("DD")
 W ?119,$S(EXP:Y,1:"NOT LISTED") G DRLOOP
 ;
DONE I $E(IOST)'="C" W @IOF
 I $E(IOST)="C" W !!,"Press RETURN to continue: " R AUTO:DTIME
END K G,IO("Q"),X,Y,ZTSK,AOU,EXP,DRG,DRGDA,DRGNAME,DRGNM,J,JJ,SEL,IGDA,LOC,LOCN,LN,MIN,PGCT,AOULP,ANS,AOU,RELEV,STLEV,%,%I,%H,^TMP("PSGWSTK",$J),AUTO,OUT
 D ^%ZISC
 S:$D(ZTQUEUED) ZTREQ="@" Q
 ;
HDR ;PRINT REPORT MAIN HEADER
 I $E(IOST)="C"&(PGCT>1) S DIR(0)="E" D ^DIR K DIR I Y'=1 S OUT=1 Q
 D NOW^%DTC S Y=$P(%,".") X ^DD("DD") W:$Y @IOF W !,"ALPHABETICAL LISTING OF AOU STOCK","  -  DATE: ",Y,?121,"PAGE: ",PGCT I $D(SEL),SEL="I",$D(IGDA) W !,"FOR INVENTORY GROUP - ",$P(^PSI(58.2,IGDA,0),"^")
 W !!?2,"AREA OF USE",?104,"MINIMUM QTY",?119,"EXPIRATION",!?18,"ITEM",?57,"LOCATION",?72,"STOCK LEVEL",?87,"REORDER LEVEL",?104,"TO DISPENSE",?122,"DATE",!,LN S PGCT=PGCT+1 Q

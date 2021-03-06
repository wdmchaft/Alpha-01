RMPR29LS ;HIN/RVD-LAB STOCK ISSUE SET UTILITY ;11/05/98
 ;;3.0;PROSTHETICS;**33,37**;Feb 09,1996
ST ;set data in 2529-3 file
 S RMPRDFN=$P(^RMPR(664.1,RMPRDA,0),U,2),DA=RMPRDA,DIE="^RMPR(664.1,"
 S DR=".03////^S X=$G(RMPR(""STA""));.04////^S X=$G(RMPR(""STA""));.09///^S X=$G(DT);2///O;.11////^S X=$G(RMPR(""STA""))"
 D ^DIE D:$D(Y)!($D(DTOUT)) CHK^RMPR29LU
 Q
SG ;set 2529-3 global
 S $P(^RMPR(664.1,RMPRDA,0),U,13)=$G(RMPRWO)
 S $P(^RMPR(664.1,RMPRDA,0),U,5)=DUZ,$P(^(0),U,18)=DT D ^RMPR29LA
 I $G(RMPRWO)'="" W !!,?5,"Assigned Work Order Number: ",RMPRWO D
 .S RMWO=$O(^RMPR(664.2,"B",RMPRWO,0))
 .F I=0:0 S I=$O(^RMPR(664.1,RMPRDA,2,I)) Q:I'>0  S RM0=$G(^RMPR(664.1,RMPRDA,2,I,0)) D
 ..S RMITEM=$P(RM0,U,1),RMQTY=$P(RM0,U,2),RMCO=$P(RM0,U,4),RMUNI=$P(RM0,U,3)
 ..S RM660=$P(RM0,U,5)
 ..Q:'RMWO  S DA(1)=RMWO,DIC="^RMPR(664.2,"_DA(1)_",1,",X=RMITEM,DIC("P")="664.22PA"
 ..K DD,DO I '$D(^RMPR(664.2,RMWO,1,"B",RMITEM)) S DIC(0)="L",DLAYGO=664.2 D FILE^DICN
 ..S RMIDA=$O(^RMPR(664.2,RMWO,1,"B",RMITEM,0))
 ..S ^RMPR(664.2,RMWO,1,RMIDA,0)=RMITEM_"^"_RMQTY_"^"_RMCO_"^^^"_RMUNI_"^^^^^^"_RM660_"^"_RMPRDA
 ..S DA=RMIDA,DIK=DIC D IX1^DIK K DA,DD,DO
 S DIE="^RMPR(664.1,",DA=RMPRDA,DR="16///S" D ^DIE
 Q
 ;
GD ;Display work order
 D DIS^RMPR29W(RMPRDFN,RMPRDA) I Y'>0 S RMFLG=1 Q
 K DR,DA,DIC,DIE S DIC="^RMPR(664.1,"_RMPRDA_",1,"
 S DIC("P")="664.15PA",DA(1)=RMPRDA
 S DIC(0)="EQMZL",X=Y(0,0),ELG=$P(Y(0),U,3) D ^DIC Q:+Y'>0
 S DIE=DIC,DA(1)=RMPRDA,DA=+Y K DIC
 S DR="1///^S X=ELG;.01;1" D ^DIE D:$D(DTOUT)!($D(Y)) CHK^RMPR29LU
 K DR,DIE
 Q
 ;
INV S DIC="^PRCP(445,",DIC(0)="AEQM",DIC("S")="I $P(^(0),U,2)=""Y"",$D(^PRCP(445,+Y,4,DUZ,0))" S:$D(RMGIP) DIC("B")=RMGIP
 D ^DIC I Y<0!$D(DTOUT)!$D(DUOUT) S RMEXIT=1 Q
 S (PRCP("I"),RMGIP)=+Y,PRCP("ITEM")=RMITEMS
 S PRCP("TYP")="R"
INVITEM I $D(^PRCP(445,PRCP("I"),1,PRCP("ITEM"),0)) G GIP
 W !!,"*** ITEM IS NOT IN GIP, UNABLE TO ISSUE THIS ITEM ......."
 S DA(1)=RMPRDA,DA=RMIDA,DIK="^RMPR(664.1,"_DA(1)_",2," D ^DIK
 K ^RMPR(664.1,RMPRDA,2,RMIDA)
 S RDEL=1 Q
GIP ;gip on
 S RMINVF="GIP"
V I +$P($G(^PRCP(445,PRCP("I"),1,PRCP("ITEM"),0)),U,12),$D(^PRC(440,+$P(^(0),U,12),0)) S DIC("B")=+$P($G(^PRCP(445,PRCP("I"),1,PRCP("ITEM"),0)),U,12)
 Q
 ;
SET S DIE(0)="AEQM",DA(1)=RMPRDA,DA=RMIDA,DIE="^RMPR(664.1,"_RMPRDA_",2,"
 S DR="2///^S X=$G(RMQTYS);4///^S X=$G(RMCOS);12///^S X=$G(RMSER);8///^S X=$G(RMTYPS);9///^S X=$G(RMCATS);10///^S X=$G(RMSPES);16///^S X=$G(RMIT);14///^S X=$G(RMSOR);13///^S X=$G(RMHS)"
 D ^DIE G:$D(DTOUT)!$D(DUOUT) EXIT
 ;S RM0=$G(^RMPR(664.1,RMPRDA,2,DA,0)),RMQTY=$P(RM0,U,2),RMCO=$P(RM0,U,4)
 S:$G(RMQTY) RMTOCO=RMQTY*RMCOS,DR="11///^S X=$G(RMTOCO)" D ^DIE
 S:$G(RMLOC) $P(^RMPR(664.1,RMPRDA,2,DA,3),U,4)=$G(RMLOC),$P(^RMPR(664.1,RMPRDA,2,DA,0),U,13)=""
 S:$G(RMGIP) $P(^RMPR(664.1,RMPRDA,2,DA,3),U,4)="",$P(^RMPR(664.1,RMPRDA,2,DA,0),U,13)=RMGIP
 S:$G(RMVEN) $P(^RMPR(664.1,RMPRDA,2,DA,3),U,2)=$G(RMVEN)
 Q
 ;
EXIT ;common exit
 Q

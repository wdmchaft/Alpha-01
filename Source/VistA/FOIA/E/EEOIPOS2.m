EEOIPOS2 ;HISC/JWR - POST INIT CLEANUP ;02/08/93 11:15
 ;;2.0;EEO Complaint Tracking;;Apr 27, 1995
TEST ;Loop through DA's for file 785 for conversion
 S (NO,STATE,N5,ADD,ZIP,DA,ICC)=""
 F  S DA=$O(^EEO(785,"ANODE",DA)) Q:DA=""  D SET,CLNODE^EEOIPCON W "."
 G KILL
SET ;Global sets for new and rearranged fields
 Q:'$D(^EEO(785,DA,0))  S EEO0=^EEO(785,DA,0),STATE=$P(EEO0,"^",6) S EEO5=$G(^EEO(785,DA,5)),ADD=$P(EEO5,"^",2),ZIP=$P(ADD,",",2),CITY=$P(ADD,",",1)
 D STRIP K EEO3,EEO1
 S:$D(^EEO(785,DA,3)) EEO3=^(3) S:$D(^EEO(785,DA,1)) EEO1=^EEO(785,DA,1)
 D IN1,BASIS
 Q
STRIP ;Breaks doun City, State, Zip, and assigns value to Case No field
 S ZIP=$TR(ZIP," ABCDEFGHIJKLMNOPQRSTUVWXYZ.,")
 I STATE'="" S $P(^EEO(785,DA,5),"^",4)=STATE
 I CITY'="" S $P(^EEO(785,DA,5),"^",3)=CITY
 I ZIP?5N!(ZIP?5N1"-"4N) S $P(^EEO(785,DA,5),"^",5)=ZIP
 S CASE=$S('$D(^EEO(785,DA,1)):"P",'$P(^(1),"^",3):"P",$E($P(^(1),"^",3),4,5)>9:$E($P(^(1),"^",3),2,3)+1,1:$E($P(^(1),"^",3),2,3))
 S CASE=CASE_"-"_(DA\100000)_"-"_(100000-(DA-((DA\100000)*100000)))
 S $P(^EEO(785,DA,5),"^",6)=CASE
 S (ADD,ZIP,N5,STATE,CITY,CASE,ICC)=""
 S DIE=785,DR=".07///@;7///@;19///@" D ^DIE
 Q
KILL ;Kills some obsolete fields and files, Reindexes certain fields in 785
 W !!,"DELETING OBSOLETE AND DUPLICATED FIELDS FROM FILE 785 "
 S DIK="^DD(785,",DA(1)=785 F DA=.07,1,7,17,17.1,17.2,18,18.1,18.2,27,28,30,31,33.5,33.6,33.7,33.8,34,35,36,37,38,39,39.5,39.6,39.7,39.8,40,43 W "." D ^DIK
 W !,"DELETING OBSOLETE FILES "
 F FI=787,788,789.2,789.3,789.4,789.6 I $D(^EEO(FI,0)) S DIU="^EEO("_FI_",",DIU(0)="DST" W !,"  ",FI D EN^DIU2
 K FI,ANS,DA,DA(1),DIC,DIE,DIK,DR,NO,X
 W !!!!,"RE-INDEXING 'C','D',& 'E' CROSS REFERENCES (FILE #785)"
 S DIK="^EEO(785," F DIK(1)=2,1.2,1.3 D ENALL^DIK
OPT S DIE="^DIC(19," W !!,"PLACING OBSOLETE OPTIONS OUT OF ORDER"
 F EEOPTION="EEO TASKED UPLINK BULLETIN","EEOREXMIT" D
 .I $D(^DIC(19,"B",EEOPTION)) S DA=$O(^(EEOPTION,"")) I DA>0 S DR="2///OUT OF ORDER;25///@" D ^DIE
 D COUNTER^EEOENF
 W !! D ^EEOIPCON
 W !!!,"         *** INITIALIZATION COMPLETE ***",!!!," Remember to Task the EEO TASKED BULLETIN option to run nightly...",!!!!!
 K DIK,EEO1IN,EEO0,EEO31,EEO5,EEONI,ICC,ZIP,STATE,N5,ADD,CITY Q
IN1 ;Converts Investigator fields to the new multiple format
 Q:$G(EEO3)'>0
 F EEONI=1,2,7,10 I $P($G(EEO3),U,EEONI)'="" D
 .S EEO31=$P($G(EEO3),U,EEONI) I $D(^EEO(787.5,EEO31)) D PRE
 .I EEO31<1000,$D(^EEO(787,EEO31)) D
 ..S X=EEO31,DIC="^EEO(787.5,",DIC(0)="M"
 ..D ^DIC I Y'>0 S $P(EEO3,U,EEONI)=2126 Q
 ..S $P(EEO3,U,EEONI)=+Y
 ..D PRE
 .;I $P($G(EEO3),U,EEONI)'="" I '$D(^EEO(787.5,$P($G(EEO3),U,EEONI)))&('$D(^EEO(787,$P($G(EEO3),U,EEONI)))) S $P(EEO3,U,EEONI)=2126
 S (SPIT,EEOINV1,EEOINV2)=""
 I $P(EEO3,U)'="" S EEOINV1=$P(EEO3,U)_"^"_$P(EEO3,U,3)_"^"_$G(EEOIN(1))_"^^"_$P(EEO3,U,5)_"^"_$P(EEO3,U,2)_"^"_$P(EEO3,U,4)_"^"_$P(EEO3,U,8) S SPIT=1
 I $P(EEO3,U,7)'="" S EEOINV2=$P(EEO3,U,7)_"^"_$P(EEO3,U,9)_"^"_$G(EEOIN(7))_"^^"_$P(EEO3,U,11)_"^"_$P(EEO3,U,10)_"^"_$P(EEO3,U,13)_"^"_$P(EEO3,U,14) S:SPIT=1 SPIT=2
 I SPIT>0 S ^EEO(785,DA,11,1,0)=$S($P(EEO3,U)'="":EEOINV1,1:EEOINV2) S ^EEO(785,DA,11,"B",$P(EEO3,U),1)=""
 I SPIT=2 S ^EEO(785,DA,11,SPIT,0)=EEOINV2 S ^EEO(785,DA,11,"B",$P(EEO3,U,7),2)=""
 S:SPIT'="" ^EEO(785,DA,11,0)="^785.03P^"_SPIT_"^"_SPIT
 I $D(^EEO(785,DA,3)) S EEOOINV=$G(^(3)),^(3)="^^"_$P(EEOOINV,U,3)_"^^^"_$P(EEOOINV,U,6)
 K EEOINV1,EEOINV2,SPIT,INV1,DA1,FIN1,REVI1,RAD,RRD,INV2,DA2,FIN2,REVI2,REVD2,RRD2,N3,EEOOINV
 Q
PRE ;Makes type of investigator determination
 I EEONI=1 S EEO1IN=$P(EEO3,U),EEODOA=$P(EEO3,U,3) D TYPE
 I EEONI=7 S EEO1IN=$P(EEO3,U,7),EEODOA=$P(EEO3,U,9) D TYPE
 Q
BASIS ;Converts basis to new multiple format
 K BA,IS I $G(EEO1)'="" D
 .S BA(1)=$P(EEO1,U,5),BA(2)=$P(EEO1,U,7),BA(3)=$P(EEO1,U,8)
 .F NO=1:1:3 D
 ..I BA(NO)'="" I $D(^EEO(785.1,BA(NO))) S BA(NO)=$P(^(BA(NO),0),U)
 .S IS(1)=$P(EEO1,U,4),IS(2)=$P(EEO1,U,9),IS(3)=$P(EEO1,U,10)
 .F NO=1:1:3 D
 ..I IS(NO)'="" I $D(^EEO(786,IS(NO))) S IS(NO)=$P(^(IS(NO),0),U)
SETB .F NO=1:1:3 I $G(BA(NO))'="" D BA
 .F NO=1:1:3 I $G(IS(NO))'="" D IS
 K BA1,BA2,BA3,IS1,IS2,IS3,LAB,NO,N1,BA,IS
 S $P(^EEO(785,DA,2),U,8)=""
 I $D(^EEO(785,DA,1)) S EEOOIS=$G(^(1)) D
 .F EX=4,5,7,8,9,10 S $P(EEOOIS,U,EX)=""
 .S ^EEO(785,DA,1)=EEOOIS
 K EEOOIS,EX Q
BA ;Enters converted Basis into file 785
 S DR="18.5///"_BA(NO),DR(2,785.01)=".01///"_BA(NO)
DIE S DIE=785 D ^DIE K DR Q
IS ;Enters converted Issue codes into file 785
 S DR="17.5///"_IS(NO),DR(2,785.02)=".01///"_IS(NO)_";1///"_$P(EEO1,U,11)
 D DIE
 Q
TYPE S (EEODATE,EEOIN(EEONI))="",EEOCN=0
 Q:EEODOA'>0
 Q:'$D(^EEO(787.5,EEO1IN))  I $D(^(EEO1IN,1)) F  S EEOCN=$O(^(1,EEOCN)) Q:EEOCN'>0  N AEE S AEE=$G(^(EEOCN,0)) D
 .I $P(AEE,U,2)<EEODOA&($P(AEE,U,3)>EEODOA!($P(AEE,U,3)="")) I EEODATE'>EEODOA S EEODATE=EEODOA,EEOIN(EEONI)=$P(AEE,U)
 I $D(^EEO(787.5,EEO1IN,1))&(EEOIN(EEONI)'>0) S EEOIN(EEONI)=$P($G(^(1,1,0)),U)
 K EEODATE,EEODOA

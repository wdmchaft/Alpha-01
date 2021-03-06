ABSVT ;VAMC ALTOONA/CTB - TRANSMIT VOL CODE SHEETS TO AUSTIN ;7/10/95  3:09 PM
V ;;4.0;VOLUNTARY TIMEKEEPING;**6**;JULY 6, 1994
 S ABSVXA="This program should ONLY be run during the first six (6) workdays of each month."
 S ABSVXA(1)="ARE YOU SURE YOU WANT TO CONTINUE",%=2 D ^ABSVYN Q:%'=1
 W !!
 D ^ABSVSITE Q:'%
 S ABSVQ("FORCEQ")="",ZTRTN="QUE^ABSVT",ZTSAVE("ABSV*")="",ZTDESC="Transmit Voluntary Service Code Sheets" D ^ABSVQ
 Q
QUE ;RELEASE CODE SHEETS TO AUSTIN
 N CPUJOB
 S CPUJOB=^%ZOSF("VOL")_"_"_$J
 S BATCH=3
 I $D(ZTQUEUED) D KILL^%ZTLOAD
 ;CALCULATE CODE SHEET STRING FOR CODE SHEETS IN CROSS REFERENCE "AF"
 K ^TMP(CPUJOB,"ABSVLIST"),^("ABSVBATCH")
 K ^ABS(503335,"AF",1,-1) ; KILLS BAD CROSS REFERENCE IF IT EXISTS
 S DA=0,(KOUNT,BATCH)=1 F I=1:1 S DA=$O(^ABS(503335,"AF",1,DA)) Q:'DA  I $P(^ABS(503335,DA,0),"^",12)=ABSV("SITE") D CREATE S:%=1 ^TMP(CPUJOB,"ABSVLIST",DA)=""
 ;PRINT TRANSMISSION LIST AND ERROR LIST
 S IOP=ABIOP,DIC="^ABS(503335,",L=0,DIS(0)="I $P($G(^ABS(503335,D0,0)),U,12)=ABSV(""SITE"")",(BY,FLDS)="[ABSV TRANSMIT]",BATCH=1,KOUNT=1 D EN1^DIP
 S IOP=ABIOP,DIC="^ABS(503335,",L=0,DIS(0)="I $P($G(^ABS(503335,D0,0)),U,12)=ABSV(""SITE"")",(BY,FLDS)="[ABSV ERROR LIST]" D EN1^DIP
 ;BUILD AND TRANSMIT MESSAGES
 S KOUNT=0 F ZX=1:1 S KOUNT=$O(^TMP(CPUJOB,"ABSVBATCH",KOUNT)) Q:'KOUNT  D BATCH
 ;POST TRANSMISSION STATUS AND CLEAN UP ^TMP
 S DA=0 F I=1:1 S DA=$O(^TMP(CPUJOB,"ABSVLIST",DA)) Q:'DA  D UPDATE
 K ^TMP(CPUJOB,"ABSVLIST"),^("ABSVBATCH"),%,%H,ABSVXI,ABIOP,ABSVX,BATCH,COMB,D1,DA,DIJ,DP,I,J,K,KOUNT,USIO,X,Y,ZTSK,ZX Q
BATCH ;BUILD INDIVIDUAL MESSAGE
 S XMDUZ=$S($D(DUZ)#2:DUZ,1:.5),XMSUB="VOLUNTEER TIME CARDS - MESSAGE "_ZX_" OF "_BATCH,XMTEXT="^TMP("""_CPUJOB_""",""ABSVBATCH"","_KOUNT_","
 S XMY("XXX@Q-NST.VA.GOV")=""
 S XMY("G.NST@"_$G(^XMB("NETNAME")))=""
 D ^XMD
 W !,XMZ," - Message Filed"
 Q
ERROR ;RECORD ERROR ON TIME CARD - RESET TRANSMISSION STATUS TO ERROR
 S $P(^ABS(503335,DA,0),"^",6)=2,^ABS(503335,"AF",2,DA)="" K ^ABS(503335,"AF",1,DA) D K S %=0 Q
CREATE ;CREATE TIME CARD STRING FOR CODE SHEET
 S %=0 G ERROR:'$D(^ABS(503335,DA,0)),ERROR:'$D(^(1)) S TIMEREC=^(0),TIMEREC1=^(1),VOL=+TIMEREC G ERROR:'TIMEREC
 S VOLREC=$S($D(^ABS(503330,VOL,0)):^(0),1:"") G ERROR:VOLREC=""
 S STRING="",PSEUDO=$E($P(VOLREC,"^",18)_" ",1),SSN=$E($P(VOLREC,"^",2)_"         ",1,9),NAME=$P(VOLREC,"^"),FNAME=$E($P(NAME,",",2)_"          ",1,10),LNAME=$E($P(NAME,",")_"              ",1,14)
 S COMB=$E($P(TIMEREC,"^",2)_"        ",1,8),MOYR=$P(TIMEREC,"^",5),MO=$E(MOYR,4,5),YR=$E(MOYR,2,3),MOYR=$E(MO_YR_"    ",1,4),HUO=$E($P(TIMEREC1,"^",34)_"  ",1,2)
 S DAYS="" F I=1:1:31 I +$P(TIMEREC1,"^",I)>0 S DAYS=DAYS_($E($P(TIMEREC1,"^",I)_" ",1))
 S SITE=$E($P(TIMEREC,"^",12)_"    ",1,4)
 I DAYS="",HUO="  " G ERROR
 S STRING="06"_SITE_PSEUDO_SSN_COMB_FNAME_LNAME_HUO_MOYR_$E(DAYS,1,26)_"$"
 S ^ABS(503335,DA,2)=STRING
 D K Q
UPDATE ;UPDATE TRANSMISSION STATUS
 D NOW^ABSVQ S TIMEREC=^ABS(503335,DA,0),TRANS=$P(TIMEREC,"^",4) I TRANS="" S $P(TIMEREC,"^",4)=X,$P(TIMEREC,"^",9)=DUZ,$P(TIMEREC,"^",6)=3,^ABS(503335,DA,0)=TIMEREC,^ABS(503335,"AF",3,DA)="" K ^ABS(503335,"AF",1,DA) G K
 S $P(TIMEREC,"^",10,11)=X_"^"_DUZ,$P(TIMEREC,"^",6)=4,^ABS(503335,DA,0)=TIMEREC,^ABS(503335,"AF",4,DA)="" K ^ABS(503335,"AF",1,DA)
K K %,ABSVXX,ABSVXY,TIMEREC,TIMEREC1,VOL,VOLREC,STRING,PSEUDO,SSN,NAME,FNAME,LNAME,COMP,MOYR,YR,MO,HUO,DAYS,SITE S %=1 Q
PRINT ;PRINT LIST OF QUEUED CODE SHEETS AND SUSPENDED CODE SHEETS
 NEW %X,%Y,B,DP
 D ^ABSVSITE Q:'%  D WAIT^ABSVYN
 S ZTDESC="VOLUNTARY SERVICE PRE-TRANSMISSION LISTINGS",ZTSAVE("ABSV*")="",ZTRTN="P1^ABSVT" D ^ABSVQ
 QUIT
P1 S DIC="^ABS(503335,",L=0,DIS(0)="I $P($G(^ABS(503335,D0,0)),U,12)=ABSV(""SITE"")",(FR,TO)="SUSPENDED",(BY,FLDS)="[ABSV BATCH LIST]",DHD="VOLUNTARY TIME CARD PRE-TRANSMISSION LISTING FOR "_ABSV("SITENAME")_" - SUSPENDED"
 S:$D(ABIOP) IOP=ABIOP D EN1^DIP
 S DIC="^ABS(503335,",L=0,DIS(0)="I $P($G(^ABS(503335,D0,0)),U,12)=ABSV(""SITE"")",(FR,TO)="READY FOR TRANSMISSION"
 S (BY,FLDS)="[ABSV BATCH LIST]",DHD="VOLUNTARY TIME CARD PRE-TRANSMISSION LISTING FOR "_ABSV("SITENAME")_" - READY FOR TRANSMISSION"
 S:$D(ABIOP) IOP=ABIOP D EN1^DIP
 K ABSVXX,ABSVXY,DCC,DIJ,DIOP,DIPT,F,FLDS,L,O,P,W,X,ZTSK Q

SROASWPD ;B'HAM ISC/MAM - REMOVE CONVERSION OPTION ; 28 APR 1992
 ;;3.0; Surgery ;;24 Jun 93
 I $O(^SRA(0)) Q
DEL W !!,"Queuing task to delete all SRA options from your system."
 S ZTRTN="SRA^SROASWPD",ZTDESC="Delete all SRA options from OPTION file (19)",ZTIO="",ZTDTH=$H D ^%ZTLOAD K ZTSK
 S SRISK=$O(^DIC(19,"B","SROA RISK ASSESSMENT",0)) I 'SRISK Q
 S SRCONV=$O(^DIC(19,"B","SROA CONVERT",0)) I 'SRCONV Q
 S INT=$O(^DIC(19,SRISK,10,"B",SRCONV,0)) I 'INT Q
 W !!,"This option will self destruct now..."
 K DA,DIK S DA(1)=SRISK,DA=INT,DIK="^DIC(19,"_DA(1)_",10," D ^DIK K DA,DIK S DA=SRCONV,DIK="^DIC(19," D ^DIK
 Q
SRA S SROPT="SRA" F  S SROPT=$O(^DIC(19,"B",SROPT)) Q:$E(SROPT,1,3)'="SRA"  D CLEAN
 S ZTREQ="@"
 Q
CLEAN ; remove option from any menu
 S SRIFN=$O(^DIC(19,"B",SROPT,0)) S SHEMP=0 F  S SHEMP=$O(^DIC(19,"AD",SRIFN,SHEMP)) Q:'SHEMP  K DA,DIK S DA(1)=SHEMP,DA=$O(^DIC(19,"AD",SRIFN,SHEMP,0)),DIK="^DIC(19,"_DA(1)_",10," D ^DIK
 K DA,DIK S DA=SRIFN,DIK="^DIC(19," D ^DIK
 Q

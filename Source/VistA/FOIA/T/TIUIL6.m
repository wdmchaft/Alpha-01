TIUIL6 ; List Template Exporter ; 20-JUN-1997
 ;;1.0;TEXT INTEGRATION UTILITIES;;Jun 20, 1997
 D BMES^XPDUTL("'TIU REVIEW SCREEN READ ONLY' List Template...")
 S DA=$O(^SD(409.61,"B","TIU REVIEW SCREEN READ ONLY",0)),DIK="^SD(409.61," D ^DIK:DA
 K DO,DD S DIC(0)="L",DIC="^SD(409.61,",X="TIU REVIEW SCREEN READ ONLY" D FILE^DICN S VALM=+Y
 I VALM>0 D
 .S ^SD(409.61,VALM,0)="TIU REVIEW SCREEN READ ONLY^1^^135^4^18^1^1^Document^TIU ACTION MENU READ-ONLY^Clinical Documents"
 .S ^SD(409.61,VALM,1)="^TIU HIDDEN ACTIONS"
 .S ^SD(409.61,VALM,"ARRAY")=" ^TMP(""TIUR"",$J)"
 .S ^SD(409.61,VALM,"COL",0)="^409.621^13^9"
 .S ^SD(409.61,VALM,"COL",1,0)="NUMBER^1^5"
 .S ^SD(409.61,VALM,"COL",2,0)="STATUS^70^10^Status"
 .S ^SD(409.61,VALM,"COL",3,0)="PATIENT NAME^6^14^Patient"
 .S ^SD(409.61,VALM,"COL",4,0)="REF DATE^60^8^Ref Date"
 .S ^SD(409.61,VALM,"COL",5,0)="SIG DATE^82^8^Completed"
 .S ^SD(409.61,VALM,"COL",10,0)="LAST I/LAST 4^20^7"
 .S ^SD(409.61,VALM,"COL",11,0)="AUTHOR^92^15^Author"
 .S ^SD(409.61,VALM,"COL",12,0)="COSIGNER^109^15^Cosigner"
 .S ^SD(409.61,VALM,"COL",13,0)="DOCUMENT TYPE^28^31^Document"
 .S ^SD(409.61,VALM,"FNL")="D CLEAN^TIUR"
 .S ^SD(409.61,VALM,"HDR")="D HDR^TIURH"
 .S ^SD(409.61,VALM,"HLP")="D PROTOCOL^TIUHELP"
 .S ^SD(409.61,VALM,"INIT")="D MAKELIST^TIUR(38)"
 .S DA=VALM,DIK="^SD(409.61," D IX1^DIK K DA,DIK
 .D MES^XPDUTL("  Filed.")
 ;
 D BMES^XPDUTL("'TIU SEARCH LIST MGR' List Template...")
 S DA=$O(^SD(409.61,"B","TIU SEARCH LIST MGR",0)),DIK="^SD(409.61," D ^DIK:DA
 K DO,DD S DIC(0)="L",DIC="^SD(409.61,",X="TIU SEARCH LIST MGR" D FILE^DICN S VALM=+Y
 I VALM>0 D
 .S ^SD(409.61,VALM,0)="TIU SEARCH LIST MGR^1^^135^4^18^1^1^Document^TIU ACTION MENU MGR^CLINICAL DOCUMENTS"
 .S ^SD(409.61,VALM,1)="^TIU HIDDEN ACTIONS ADVANCED"
 .S ^SD(409.61,VALM,"ARRAY")=" ^TMP(""TIUR"",$J)"
 .S ^SD(409.61,VALM,"COL",0)="^409.621^13^9"
 .S ^SD(409.61,VALM,"COL",1,0)="NUMBER^1^5"
 .S ^SD(409.61,VALM,"COL",2,0)="STATUS^70^10^Status"
 .S ^SD(409.61,VALM,"COL",3,0)="PATIENT NAME^6^14^Patient"
 .S ^SD(409.61,VALM,"COL",4,0)="REF DATE^60^8^Ref Date"
 .S ^SD(409.61,VALM,"COL",5,0)="SIG DATE^82^8^Completed"
 .S ^SD(409.61,VALM,"COL",10,0)="LAST I/LAST 4^20^7^^^"
 .S ^SD(409.61,VALM,"COL",11,0)="AUTHOR^92^15^Author"
 .S ^SD(409.61,VALM,"COL",12,0)="COSIGNER^109^15^Cosigner"
 .S ^SD(409.61,VALM,"COL",13,0)="DOCUMENT TYPE^28^31^Document^^1"
 .S ^SD(409.61,VALM,"COL","AIDENT",1,13)=""
 .S ^SD(409.61,VALM,"FNL")="D CLEAN^TIUR"
 .S ^SD(409.61,VALM,"HDR")="D HDR^TIURH"
 .S ^SD(409.61,VALM,"HLP")="D PROTOCOL^TIUHELP"
 .S ^SD(409.61,VALM,"INIT")="D MAKELIST^TIUR(38)"
 .S DA=VALM,DIK="^SD(409.61," D IX1^DIK K DA,DIK
 .D MES^XPDUTL("  Filed.")
 ;
 D BMES^XPDUTL("'TIU SEARCH LIST MRT' List Template...")
 S DA=$O(^SD(409.61,"B","TIU SEARCH LIST MRT",0)),DIK="^SD(409.61," D ^DIK:DA
 K DO,DD S DIC(0)="L",DIC="^SD(409.61,",X="TIU SEARCH LIST MRT" D FILE^DICN S VALM=+Y
 I VALM>0 D
 .S ^SD(409.61,VALM,0)="TIU SEARCH LIST MRT^1^^135^4^18^1^1^Document^TIU ACTION MENU MRT^CLINICAL DOCUMENTS"
 .S ^SD(409.61,VALM,1)="^TIU HIDDEN ACTIONS ADVANCED"
 .S ^SD(409.61,VALM,"ARRAY")=" ^TMP(""TIUR"",$J)"
 .S ^SD(409.61,VALM,"COL",0)="^409.621^13^9"
 .S ^SD(409.61,VALM,"COL",1,0)="NUMBER^1^5"
 .S ^SD(409.61,VALM,"COL",2,0)="STATUS^70^10^Status"
 .S ^SD(409.61,VALM,"COL",3,0)="PATIENT NAME^6^14^Patient"
 .S ^SD(409.61,VALM,"COL",4,0)="REF DATE^60^8^Ref Date"
 .S ^SD(409.61,VALM,"COL",5,0)="SIG DATE^82^8^Completed"
 .S ^SD(409.61,VALM,"COL",10,0)="LAST I/LAST 4^20^7"
 .S ^SD(409.61,VALM,"COL",11,0)="AUTHOR^92^15^Author"
 .S ^SD(409.61,VALM,"COL",12,0)="COSIGNER^109^15^Cosigner"
 .S ^SD(409.61,VALM,"COL",13,0)="DOCUMENT TYPE^28^31^Document^^1"
 .S ^SD(409.61,VALM,"COL","AIDENT",1,13)=""
 .S ^SD(409.61,VALM,"FNL")="D CLEAN^TIUR"
 .S ^SD(409.61,VALM,"HDR")="D HDR^TIURH"
 .S ^SD(409.61,VALM,"HLP")="D PROTOCOL^TIUHELP"
 .S ^SD(409.61,VALM,"INIT")="D MAKELIST^TIUR(38)"
 .S DA=VALM,DIK="^SD(409.61," D IX1^DIK K DA,DIK
 .D MES^XPDUTL("  Filed.")
 ;
 D BMES^XPDUTL("'TIU SEND BACK' List Template...")
 S DA=$O(^SD(409.61,"B","TIU SEND BACK",0)),DIK="^SD(409.61," D ^DIK:DA
 K DO,DD S DIC(0)="L",DIC="^SD(409.61,",X="TIU SEND BACK" D FILE^DICN S VALM=+Y
 I VALM>0 D
 .S ^SD(409.61,VALM,0)="TIU SEND BACK^1^^^5^18^1^1^Document^TIU ACTION MENU SEND BACK^REVIEW/SEND BACK"
 .S ^SD(409.61,VALM,1)="^TIU HIDDEN ACTIONS BROWSE"
 .S ^SD(409.61,VALM,"ARRAY")=" ^TMP(""TIUVIEW"",$J)"
 .S ^SD(409.61,VALM,"FNL")="S VALMBCK=""Q"""
 .S ^SD(409.61,VALM,"HDR")="D HDR^TIUBR"
 .S ^SD(409.61,VALM,"HLP")="D FULL^VALM1,PROTOCOL^TIUHELP S VALMBCK=""R"""
 .S ^SD(409.61,VALM,"INIT")="D EN^TIUBR"
 .S DA=VALM,DIK="^SD(409.61," D IX1^DIK K DA,DIK
 .D MES^XPDUTL("  Filed.")
 ;
 G ^TIUIL7

MCPREDT ;HIRMFO/JCC-ENTER/EDIT PROCEDURE/SUBSPECIALTY ;8/19/96  09:21
 ;;2.3;Medicine;**8**;09/13/1996
EN ;
 S DIC="^MCAR(697.2,",DLAYGO=697.2,DIC(0)="AEQLM",DIC("S")="I $P(^(0),U,4)=""GEN"",$P(^(0),U,19)" D ^DIC G EXIT:Y<0
 S DIE=DIC,DA=+Y S DR="[MCBUILDGENERIC]" K DIC,DLAYGO D ^DIE
 I $$GET1^DIQ(697.2,DA,3,"I")="GEN" S DIE="^MCAR(697.2,",DR="12////MCKEYGEN" D ^DIE  ;MC*2.3*8
 S MCARDA=DA
 S DIC="^MCAR(690.2,",DLAYGO=690.2,DIC(0)="QL",X="Brief Generic" D ^DIC G NEXT:Y<0
 S MCARIEN=+$O(^MCAR(690.2,+Y,3,"B",MCARDA,0)) I MCARIEN G NEXT
 S DIC="^MCAR(690.2,"_+Y_",3,",DIC("P")=$$GET1^DID(690.2,4,"","SPECIFIER"),DIC(0)="L",D0=+Y,DA(1)=+Y,X=MCARDA
 K DD,DO,DINUM D FILE^DICN
NEXT S DIC="^MCAR(690.2,",DLAYGO=690.2,DIC(0)="QL",X="Full Generic" D ^DIC G EXIT:Y<0
 S MCARIEN=+$O(^MCAR(690.2,+Y,3,"B",MCARDA,0)) I MCARIEN G EXIT
 S DIC="^MCAR(690.2,"_+Y_",3,",DIC("P")=$$GET1^DID(690.2,4,"","SPECIFIER"),DIC(0)="L",D0=+Y,DA(1)=+Y,X=MCARDA
 K DD,DO,DINUM D FILE^DICN
EXIT K D0,DIC,DLAYGO,DIE,DA,DR,MCARDA,MCARIEN
 Q

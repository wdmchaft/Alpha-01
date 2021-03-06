PRCHAMXH ;WISC/DJM-'CHANGES' ROUTINES FOR 443.6 ;12/2/94  2:52 PM
V ;;5.1;IFCAP;;Oct 20, 2000
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 ;
 ;****NOTE-See PRCHAMXA for information on variable PRCHNORE and
 ;incidence of undefined DIK variable errors.
 ;
EN0 ;SAVES 'CHANGES' INFORMATION FOR 'ITEM' MULTIPLE, 'DESCRIPTION' MULTIPLE.
 D DELCHK
 N FF,PRCHDA1,RECORD,Y
 S FF="1;443.61:40",PRCHDA1=PRCHPO,RECORD=+PRCHI
 D SAVE(FF,PRCHDA1,RECORD)
 Q
 ;
SAVE(FF,PRCHDA1,RECORD) ;THIS WILL DO THE ACTUAL SAVING OF THE INFORMATION.
 ;'PRCHAM' IS DEFINED FROM AMENDMENT ROUTINES.
 ;IT IS THE 'AMENDMENT' FIELD'S RECORD NUMBER FOR THE AMENDMENT THAT
 ;IS BEING ENTERED.
 ;'PRCHAMDA' IS THE INTERNAL # OF THE AMENDMENT TYPE BEING USED, FROM
 ;FILE 442.2 (P.O.) OR 441.6 (REQUISITION).
 N PRCHDA,OLD,DIFLD,DIP,F2NUMBER,ALREADY,DS,D,D0,D1,D2,DIG,DIH,DISYS,DIU,DIV,J,L,DH,DU,DV,DW,DOV,LINE1,DIOV
 S F2NUMBER=0,ALREADY=$O(^PRC(443.6,"C",PRCHDA1,PRCHAM,FF,RECORD,F2NUMBER,0))
 Q:ALREADY>0  ;CHECK IF THIS FIELD HAS ALREADY BEEN ENTERED.  ONLY THE FIRST ENTRY IS NEEDED.
 S PRCHDA="",LINE1=$O(^PRC(442,PRCHDA1,2,RECORD,1,0)) Q:LINE1'>0
 S OLD=$G(^PRC(442,PRCHDA1,2,RECORD,1,LINE1,0)) Q:OLD=""
 N DA,X
 D NEXT(PRCHDA1,PRCHAM,.PRCHDA)
 N DIE,DC,DD,DE,DG,DIEL,DI,DK,DL,DM,DO,DP,DQ,DR
 S DA(2)=PRCHDA1,DA(1)=PRCHAM,DA=PRCHDA,DIE="^PRC(443.6,"_DA(2)_",6,"_DA(1)_",3,"
 S DR="1////^S X=PRCHAMDA;2////^S X=FF;3///^S X=OLD;4///^S X=RECORD;7////^S X=F2NUMBER" D ^DIE
 S DA(3)=DA(2),DA(2)=DA(1),DA(1)=DA,DIE="^PRC(443.6,"_DA(3)_",6,"_DA(2)_",3,"_DA(1)_",1,",ZERO=$G(^PRC(443.6,DA(3),6,DA(2),3,DA(1),1,0))
 F  S LINE1=$O(^PRC(442,PRCHDA1,2,RECORD,1,LINE1)) Q:LINE1'>0  D
 .S OLD=$G(^PRC(442,PRCHDA1,2,RECORD,1,LINE1,0)) Q:OLD=""
 .S DA=LINE1,^PRC(443.6,DA(3),6,DA(2),3,DA(1),1,DA,0)=OLD,$P(ZERO,U,3)=DA,$P(ZERO,U,4)=$P(ZERO,U,4)+1
 .Q
 S ^PRC(443.6,DA(3),6,DA(2),3,DA(1),1,0)=ZERO
 Q
 ;
NEXT(DA,DA1,DA2) ;COME HERE TO CREATE THE NEXT ENTRY IN THE 'CHANGES' MULTIPLE.
 ;DA2 IS RETURNED WITH THE 'CHANGES' INTERNAL RECORD NUMBER.
 N AA,BB,DIC,DD,DINUM,DO,X,Y
 S AA=$G(^PRC(443.6,DA,6,DA1,3,0)) I AA="" S AA=1,^PRC(443.6,DA,6,DA1,3,0)="^"_$P(^DD(443.67,14,0),"^",2) G ENTER
 S AA=$P(AA,U,3)
FIND S AA=AA+1,BB=$G(^PRC(443.6,DA,6,DA1,3,AA,0)) I BB'="" G FIND
ENTER K DD,DO S DA(2)=DA,DA(1)=DA1,DIC="^PRC(443.6,"_DA(2)_",6,"_DA(1)_",3,",DIC(0)="L",(DINUM,X)=AA D FILE^DICN G:+Y'>0 FIND
 S DA2=+Y Q
DELCHK ; Checks to see if any delivery schedule has a delivery schedule 
 ; quantity that is not >0.  If so and there is an entry in 442.8
 ; a delete flag is entered in 441.7 and the quantity is set to 0.
 ; If there is no entry in 442.8 the 441.7 entry is deleted.
 N NUM,J,K,DA
 S NUM=$P(^PRC(442,PRCHPO,0),U)
 I $D(^PRC(441.7,"AG",NUM)) D
 . S J=0 F  S J=$O(^PRC(441.7,"AG",NUM,J)) Q:J'>0  D
 . . S K=0 F  S K=$O(^PRC(441.7,"AG",NUM,J,K)) Q:K'>0  D
 . . . I $P(^PRC(441.7,K,0),U,5)'>0,($P(^PRC(441.7,K,0),U,7)']"") D  Q
 . . . . S DIK="^PRC(441.7,",DA=K D ^DIK K DIK
 . . . I $P(^PRC(441.7,K,0),U,5)'>0,($P(^PRC(441.7,K,0),U,7)]"") D  Q
 . . . . S $P(^PRC(441.7,K,0),U,6)="D"
 . . . . S $P(^PRC(441.7,K,0),U,5)=0
 Q

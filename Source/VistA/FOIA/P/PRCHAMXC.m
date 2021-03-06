PRCHAMXC ;WISC/DJM-'CHANGES' ROUTINES #3 FOR 443.6 ;5/12/95  11:58 AM
V ;;5.1;IFCAP;;Oct 20, 2000
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 ;
 ;****NOTE-See PRCHAMXA for information on variable PRCHNORE and
 ;incidence of undefined DIK variable errors.
 ;
EN1 ;SAVES 'ADMINISTRATIVE CERTIFICATIONS'
 N DIK,FF,PRCHDA1,RECORD,Y
 S PRCHDA1=DA(1),RECORD=DA,FF=".01;443.624:24"
 D SAVE(FF,PRCHDA1,RECORD)
 Q
 ;
EN2 ;SAVES 'DESCRIPTION LINE COUNT'
 N FF,PRCHDA1,RECORD,Y
 S PRCHDA1=DA(1),RECORD=DA,FF="1;443.624:24"
 D SAVE(FF,PRCHDA1,RECORD)
 Q
 ;
EN3 ;SAVES 'DIRECT DELIVERY PATIENT' IN MAIN FILE
 N FF,PRCHDA1,RECORD,Y
 S PRCHDA1=DA,RECORD=0,FF="5.3;"
 D SAVE(FF,PRCHDA1,RECORD)
 Q
 ;
EN4 ;SAVES 'EST.SHIPPING' IN MAIN FILE
 N FF,PRCHDA1,RECORD,Y
 S PRCHDA1=DA,RECORD=0,FF="13;"
 D SAVE(FF,PRCHDA1,RECORD)
 Q
 ;
EN5 ;SAVES 'FCP' IN MAIN FILE
 N FF,PRCHDA1,RECORD,Y
 S PRCHDA1=DA,RECORD=0,FF="1;"
 D SAVE(FF,PRCHDA1,RECORD)
 Q
 ;
EN6 ;SAVES 'COST CENTER' IN MAIN FILE
 N FF,PRCHDA1,RECORD,Y
 S PRCHDA1=DA,RECORD=0,FF="2;"
 D SAVE(FF,PRCHDA1,RECORD)
 Q
 ;
EN7 ;SAVES 'REQUESTING SERVICE' IN MAIN FILE
 N FF,PRCHDA1,RECORD,Y
 S PRCHDA1=DA,RECORD=0,FF="5.2;"
 D SAVE(FF,PRCHDA1,RECORD)
 Q
 ;
EN8 ;SAVES 'F.O.B. POINT' IN MAIN FILE
 N FF,PRCHDA1,RECORD,Y
 S PRCHDA1=DA,RECORD=0,FF="6.4;"
 D SAVE(FF,PRCHDA1,RECORD)
 Q
 ;
EN9 ;SAVES 'DISCOUNT ITEM' IN 'DISCOUNT' MULTIPLE
 N FF,PRCHDA1,RECORD,Y
 S PRCHDA1=DA(1),RECORD=DA,FF=".01;443.63:14"
 D SAVE(FF,PRCHDA1,RECORD)
 Q
 ;
EN10 ;SAVES 'PERCENT/DOLLAR AMOUNT' IN 'DISCOUNT' MULTIPLE
 N FF,PRCHDA1,RECORD,Y
 S PRCHDA1=DA(1),RECORD=DA,FF="1;443.63:14"
 D SAVE(FF,PRCHDA1,RECORD)
 Q
 ;
EN11 ;SAVES 'EST. SHIPPING BOC' IN MAIN FILE
 N FF,PRCHDA1,RECORD,Y
 S PRCHDA1=DA,RECORD=0,FF="13.05;"
 D SAVE(FF,PRCHDA1,RECORD)
 Q
 ;
EN12 ;SAVES 'GOV'T B/L NO.' IN MAIN FILE
 N FF,PRCHDA1,RECORD,Y
 S PRCHDA1=DA,RECORD=0,FF="13.2;"
 D SAVE(FF,PRCHDA1,RECORD)
 Q
 ;
EN13 ;SAVES 'SHIP VIA' IN MAIN FILE
 N FF,PRCHDA1,RECORD,Y
 S PRCHDA1=DA,RECORD=0,FF="13.3;"
 D SAVE(FF,PRCHDA1,RECORD)
 Q
 ;
EN14 ;SAVES 'GBL P.O. NUMBER' IN MAIN FILE
 N FF,PRCHDA1,RECORD,Y
 S PRCHDA1=DA,RECORD=0,FF="13.4;"
 D SAVE(FF,PRCHDA1,RECORD)
 Q
 ;
SAVE(FF,PRCHDA1,RECORD) ;THIS WILL DO THE ACTUAL SAVING OF THE INFORMATION.
 ;'PRCHAM' IS DEFINED FROM AMENDMENT ROUTINES.
 ;IT IS THE 'AMENDMENT' FIELD'S RECORD NUMBER FOR THE AMENDMENT THAT
 ;IS BEING ENTERED.
 ;'PRCHAMDA' IS THE INTERNAL # OF THE AMENDMENT TYPE BEING USED, FROM
 ;FILE 442.2.
 N PRCHDA,OLD,F2NUMBER,ALREADY,DS,DIFLD,DIP,D,D0,D1,D2,DIG,DIH,DISYS,DIU,DIV,J,L,DH,DU,DV,DW,DOV,DIOV
 S F2NUMBER=0,ALREADY=$O(^PRC(443.6,"C",PRCHDA1,PRCHAM,FF,RECORD,F2NUMBER,0))
 Q:ALREADY>0  ;CHECK IF THIS FIELD HAS ALREADY BEEN ENTERED.  ONLY THE FIRST ENTRY IS NEEDED.
 S PRCHDA="",OLD=X S:OLD="" OLD=0
 N DA,X
 D NEXT(PRCHDA1,PRCHAM,.PRCHDA)
 N DIE,DC,DD,DE,DG,DI,DIEL,DK,DL,DM,DO,DP,DQ,DR
 S DA(2)=PRCHDA1,DA(1)=PRCHAM,DA=PRCHDA,DIE="^PRC(443.6,"_DA(2)_",6,"_DA(1)_",3,"
 S DR="1////^S X=PRCHAMDA;2////^S X=FF;3///^S X=OLD;4///^S X=RECORD;7////^S X=F2NUMBER" D ^DIE
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

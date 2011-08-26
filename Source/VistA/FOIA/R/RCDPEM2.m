RCDPEM2 ;ALB/TMK - MANUAL ERA AND EFT MATCHING ;05-NOV-02
 ;;4.5;Accounts Receivable;**173,208**;Mar 20, 1995
 ;;Per VHA Directive 10-93-142, this routine should not be modified.
 Q
MATCH1 ; Manually 'match' an ERA to an EFT
 N DA,DR,DIE,DIC,DIR,X,Y,RCEFT,RCERA,RCNAME,RCMATCH,RCQUIT,DUOUT,DTOUT
 W !,"THIS OPTION WILL ALLOW YOU TO MANUALLY MATCH AN EFT DETAIL RECORD",!,"WITH AN ERA RECORD"
M1 S DIR("A")="SELECT THE UNMATCHED EFT TO MATCH TO AN ERA: "
 S DIR(0)="PAO^RCY(344.31,:AEMQ",DIR("S")="I '$P(^(0),U,8)"
 W ! D ^DIR K DIR
 I $D(DUOUT)!$D(DTOUT)!(Y<0) G M1Q
 S RCEFT=+Y,RCEFT(0)=$G(^RCY(344.31,+Y,0))
 W !
 S DIC="^RCY(344.31,",DR="0",DA=RCEFT D EN^DIQ
 W !
 S DIR("A")="ARE YOU SURE THIS IS THE EFT YOU WANT TO MATCH?: ",DIR(0)="YA",DIR("B")="YES" D ^DIR K DIR
 I $D(DUOUT)!$D(DTOUT) G M1Q
 I Y'=1 G M1
M12 S DIR("A")="SELECT THE UNMATCHED ERA TO MATCH TO EFT #"_RCEFT_": "
 S DIR(0)="PAO^RCY(344.4,:AEMQ",DIR("S")="I '$P(^(0),U,9),'$P(^(0),U,8)"
 W ! D ^DIR K DIR
 I $D(DUOUT)!$D(DTOUT)!(Y<0) G M1Q
 S RCERA=+Y,RCERA(0)=$G(^RCY(344.4,+Y,0))
 W !
 S DIC="^RCY(344.4,",DR="0",DA=RCERA D EN^DIQ
 W !
 S DIR("A")="ARE YOU SURE THIS IS THE CORRECT ERA TO MATCH TO?: ",DIR(0)="YA",DIR("B")="YES" D ^DIR K DIR
 I $D(DUOUT)!$D(DTOUT) G M1Q
 I Y'=1 G M12
 S RCMATCH=(+$P(RCERA(0),U,5)=+$P(RCEFT(0),U,7))
 S RCNAME=($P(RCERA(0),U,6)=$P(RCEFT(0),U,2))
 I 'RCMATCH!'RCNAME D  G:RCQUIT M1Q
 . N Z
 . S RCQUIT=0,Z=1
 . S DIR("A",1)="***WARNING***"
 . I 'RCNAME S Z=Z+1,DIR("A",Z)=$J("",3)_"> The payer names on these two records do not agree"
 . I 'RCMATCH S Z=Z+1,DIR("A",Z)=$J("",3)_"> The amount of payment on these two records do not agree"
 . S DIR(0)="YA",DIR("B")="NO",DIR("A")="ARE YOU SURE YOU WANT TO MATCH THESE 2 RECORDS?: " W ! D ^DIR K DIR
 . I $S($D(DUOUT)!$D(DTOUT):1,Y'=1:1,1:0) S RCQUIT=1 Q
 S DIE="^RCY(344.4,",DR=".09////1",DA=RCERA D ^DIE
 I '$D(Y) S DIE="^RCY(344.31,",DR=".08////1;.1////"_RCERA,DA=RCEFT D ^DIE
 S DIR(0)="EA",DIR("A",1)="EFT #"_RCEFT_" WAS "_$S('$D(Y):"SUCCESSFULLY",1:"NOT")_" MATCHED TO ERA #"_RCERA,DIR("A")="PRESS RETURN TO CONTINUE: " D ^DIR K DIR
M1Q Q
 ;
MATCH2 ; Manually 'match' a 0-balance EFT to a paper EOB
 N DUOUT,DTOUT,DA,DR,DIE,DIC,DIR,X,Y,RCEFT,RCRCPT
 W !,"THIS OPTION WILL ALLOW YOU TO MANUALLY MARK A 0-BALANCE EFT DETAIL RECORD",!,"AS MATCHED TO A PAPER EOB"
M2 S DIR("A")="SELECT THE UNMATCHED 0-BALANCE EFT TO MARK AS MATCHED TO PAPER EOB: "
 S DIR(0)="PAO^RCY(344.31,:AEMQ",DIR("S")="I '$P(^(0),U,8),'$P(^(0),U,7)"
 W ! D ^DIR K DIR
 I $D(DUOUT)!$D(DTOUT)!(Y'>0) G M2Q
 S RCEFT=+Y
 W !
 S DIC="^RCY(344.31,",DR="0",DA=RCEFT D EN^DIQ
 W !
 S DIR("A")="ARE YOU SURE THIS IS THE EFT YOU WANT TO MARK AS MATCHED?: ",DIR(0)="YA",DIR("B")="YES" D ^DIR K DIR
 I $D(DUOUT)!$D(DTOUT) G M2Q
 I Y'=1 G M2
 S DIE="^RCY(344.31,",DR=".08////2",DA=RCEFT D ^DIE
 S DIR(0)="EA",DIR("A")="EFT #"_RCEFT_" WAS "_$S('$D(Y):"SUCCESSFULLY",1:"NOT")_" MARKED AS MATCHED TO PAPER EOB" D ^DIR K DIR
M2Q Q
 ;
MANTR ; Mark an EFT detail record as 'TR' posted manually
 N DA,DR,DIC,DIE,DIR,X,Y,RCEFT,DUOUT,DTOUT,RCZ0,RCTR,RCHOW
 ; EFT detail cannot be associated with a receipt or TR document
 ;
 W !,"*****",!," YOU SHOULD ONLY USE THIS OPTION IF YOU HAVE AN EFT DETAIL RECORD ON YOUR",!," UNAPPLIED DEPOSIT REPORT WHOSE DETAIL WAS ENTERED ON LINE VIA A TR DOCUMENT",!,"*****",!
 S DIC(0)="AEMQ",DIC("S")="I $P(^(0),U,16)="""",$P(^(0),U,11)",DIC("A")="SELECT THE EFT DETAIL WHOSE 'TR' DOC WAS MANUALLY ENTERED ON LINE: ",DIC="^RCY(344.31,"
 W ! D ^DIC K DIC
 I Y'>0 G MANTRQ
 S RCEFT=+Y,RCZ0=$G(^RCY(344.31,RCEFT,0))
 S DIR(0)="FA^2:30^K:X'?1""TR"".E X",DIR("A")="ENTER THE TR DOC # THAT WAS ENTERED ON-LINE FOR THE EFT DETAIL: "
 W ! D ^DIR K DIR
 I $D(DTOUT)!$D(DUOUT) G MANTRQ
 S RCTR=Y,DR=""
 ;
 I '$P(RCZ0,U,8) D  G:RCQUIT MANTRQ  ;Unmatched
 . S DIR(0)="SA^E:ELECTRONIC ERA;P:PAPER EOB",DIR("A")="WAS THE EFT DETAIL RECEIVED BY (E)RA or (P)APER EOB?: " W ! D ^DIR K DIR
 . I $D(DTOUT)!$D(DUOUT) S RCQUIT=1 Q
 . S RCHOW=Y,DR=""
 . I RCHOW="E" D
 .. S DR=";.09R;.08////1"
 . I RCHOW="P" D
 .. S DR=";.08////2"
 ;
 S DIR(0)="YA",DIR("B")="NO",DIR("A",1)="THIS WILL MARK EFT DETAIL #: "_RCEFT_" AS MANUALLY POSTED",DIR("A",2)="  USING TR DOC: "_RCTR
 S DIR("A")="ARE YOU SURE YOU WANT TO CONTINUE?: " W ! D ^DIR K DIR
 I Y'=1 D  G MANTRQ
 . S DIR(0)="EA",DIR("A")="EFT NOT UPDATED - PRESS RETURN TO CONTINUE: " W ! D ^DIR K DIR
 S DIE="^RCY(344.31,",DA=RCEFT,DR=".16R"_DR D ^DIE
 I $D(Y) D
 . S DIE="^RCY(344.31,",DA=RCEFT,DR=".16///@;.08///"_$S($P(RCZ0,U,8)'="":$P(RCZ0,U,8),1:"@") D ^DIE
 . S DIR("A")="EFT NOT UPDATED - PRESS RETURN TO CONTINUE"
 E  D
 . S DIR("A")="STATUS UPDATED FOR EFT DETAIL #: "_RCEFT_" - PRESS RETURN TO CONTINUE: "
 S DIR(0)="EA"
 W ! D ^DIR K DIR
 ;
MANTRQ Q
 ;
CHK() ; Function returns the ien of CHECK/MO payment type
 Q +$O(^RC(341.1,"AC",4,0))
 ;
POSTED ; Mark an ERA as posted when the data was previously posted using
 ; paper EOB information
 N DIC,DIE,DIR,DA,DR,X,Y
 ; Must be unmatched or matched to paper check, must not already have a receipt, must not be posted yet
 W !!,"THIS OPTION IS USED WHEN YOU HAVE POSTED AN ERA PAID WITH A PAPER CHECK",!,"BY USING THE PAPER EOB AND YOU DID NOT REFERENCE THE ERA IN THE RECEIPT",!!
 S DIC("S")="I ""02""[+$P(^(0),U,9),$S('$P(^(0),U,8):1,1:'$P(^(0),U,5)),$P(^(0),U,14)=0",DIC="^RCY(344.4,",DIC(0)="AEMQ"
 D ^DIC K DIC
 ;
 I Y'>0 G POSTEDQ
 ;
 S DIE="^RCY(344.4,",DR=".08R;.09////2;.14////2;20.03////1",DA=+Y D ^DIE
 I '$D(Y) D
 . S DIR(0)="EA",DIR("A",1)="ERA HAS BEEN MARKED AS POSTED USING PAPER EOB",DIR("A")="PRESS RETURN TO CONTINUE " D ^DIR K DIR
 ;
POSTEDQ Q
 ;
MATCH3 ; Manually 'match' a 0-balance ERA that has no check or EFT
 N DUOUT,DTOUT,DA,DR,DIE,DIC,DIR,X,Y,RCERA,RCRCPT
 W !,"THIS OPTION WILL ALLOW YOU TO MANUALLY MARK A 0-BALANCE ERA WITH NO",!,"CHECK OR EFT AS 'MATCH-0 PAYMENT' TO REMOVE IT FROM THE ERA AGING REPORT"
M3 S DIR("A")="SELECT THE UNMATCHED 0-BALANCE ERA TO MARK AS MATCHED: "
 S DIR(0)="PAO^RCY(344.4,:AEMQ",DIR("S")="I '$P(^(0),U,9),'$P(^(0),U,5)"
 W ! D ^DIR K DIR
 I $D(DUOUT)!$D(DTOUT)!(Y'>0) G M3Q
 S RCERA=+Y
 W !
 S DIC="^RCY(344.4,",DR="0",DA=RCERA D EN^DIQ
 W !
 S DIR("A")="ARE YOU SURE THIS IS THE ERA YOU WANT TO MARK AS MATCH-0 PAYMENT?: ",DIR(0)="YA",DIR("B")="YES" D ^DIR K DIR
 I $D(DUOUT)!$D(DTOUT) G M3Q
 I Y'=1 G M3
 S DIE="^RCY(344.4,",DR=".09////3",DA=RCERA D ^DIE
 S DIR(0)="EA",DIR("A")="ERA #"_RCERA_" WAS "_$S('$D(Y):"SUCCESSFULLY",1:"NOT")_" MARKED AS MATCH-0 PAYMENT" D ^DIR K DIR
M3Q Q
 ;
UNMATCH ; Used to 'unmatch' an ERA matched in error
 N X,Y,DIR,DIC,DIE,DIK,DA,DR,RCWL,RCEFT,RCQUIT
 S DIC(0)="AEMQ",DIC="^RCY(344.4,",DIC("S")="I '$P(^(0),U,8),$S('$P(^(0),U,14):1,1:$P(^(0),U,9)=3),$P(^(0),U,9)" D ^DIC K DIC
 Q:Y'>0
 S RCWL=+Y,RCQUIT=0
 I $D(^RCY(344.49,RCWL,0)) D  Q:RCQUIT
 . S DIR(0)="YA",DIR("A",1)="THIS ERA ALREADY HAS A WORKLIST ENTRY AND MUST BE DELETED BEFORE IT CAN BE UNMATCHED",DIR("A")="DO YOU WANT TO DELETE THE WORKLIST ENTRY FOR THIS ERA NOW?: "
 . W ! D ^DIR K DIR
 . I Y'=1 S RCQUIT=1 Q
 . S DIK="^RCY(344.49,",DA=RCWL D ^DIK
 I $O(^RCY(344.31,"AERA",RCWL,0)) S RCEFT=+$O(^(0)) D  Q:RCQUIT
 . S DIR("A",1)="THIS ERA IS MATCHED TO EFT #"_RCEFT,DIR("A")="ARE YOU SURE YOU WANT TO UNMATCH THEM?: ",DIR(0)="YA"
 . W ! D ^DIR K DIR
 . I Y'=1 S RCQUIT=1 Q
 . S DIE="^RCY(344.31,",DR=".1///@;.08////0",DA=RCEFT D ^DIE
 . W !,"EFT #"_RCEFT_" IS NOW UNMATCHED",!
 S DIE="^RCY(344.4,",DR=".09////0;.13///@;.14////0",DA=RCWL D ^DIE
 S DIR("A")="ERA HAS BEEN SUCCESSFULLY UNMATCHED - PRESS RETURN TO CONTINUE "
 S DIR(0)="EA" W ! D ^DIR K DIR
 Q
 ;
RETN ; Entrypoint for returned ERA
 N DIR,X,Y,DTOUT,DUOUT,DIC,RCY,DIE,DA,DR
 W !!,"USE THIS OPTION ONLY IF YOU HAVE A PAPER CHECK THAT HAS BEEN RETURNED TO",!,"THE PAYER WITHOUT BEING DEPOSITED AND YOU WANT TO MARK THE CORRESPONDING",!,"ERA AS NOT POSTED/RETURNED TO PAYER",!
 S DIC="^RCY(344.4,",DIC(0)="AEMQ",DIC("S")="I $S($P(^(0),U,9)=0!($P(^(0),U,9)=2):1,1:0),'$P(^(0),U,14)" D ^DIC
 Q:Y'>0
 S RCY=+Y
 S DIR(0)="YA",DIR("A",1)="THIS WILL MARK THE ERA # "_+Y_" AS RETURNED TO PAYER/NOT POSTED",DIR("A")="ARE YOU SURE YOU WANT TO CONTINUE?: " W ! D ^DIR K DIR
 I $D(DTOUT)!$D(DUOUT)!Y=0 D
 . S DIR("A",1)="NO CHANGES MADE TO ERA # "_RCY
 E  D
 . S DIR("A",1)="ERA # "_RCY_" HAS BEEN MARKED AS RETURNED TO PAYER/NOT POSTED"
 . S DIE="^RCY(344.4,",DR=".14////4;.09////2",DA=RCY D ^DIE
 S DIR(0)="EA",DIR("A")="PRESS RETURN TO CONTINUE: "
 W ! D ^DIR K DIR
 Q
 ;

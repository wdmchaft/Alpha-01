DGMTXD1 ; ;08/13/05
 S X=DE(25),DIC=DIE
 K DIV S DIV=X,D0=DA,DIV(0)=D0 S Y(0)=X X ^DD(408.22,.11,1,1,79.2) S X=X="" I X S X=DIV S Y(1)=$S($D(^DGMT(408.22,D0,0)):^(0),1:"") S X=$P(Y(1),U,12),X=X S DIU=X K Y S X="" X ^DD(408.22,.11,1,1,2.4)
 S X=DE(25),DIC=DIE
 I $D(^DGMT(408.22,DA,0)),$P(^(0),U,11)="" D INC^DGMTDD2

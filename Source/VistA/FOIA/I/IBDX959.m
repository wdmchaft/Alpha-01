IBDX959 ; COMPILED XREF FOR FILE #357.951 ; 10/15/04
 ; 
 S DA=0
A1 ;
 I $D(DISET) K DIKLM S:DIKM1=1 DIKLM=1 G @DIKM1
0 ;
A S DA=$O(^IBD(357.95,DA(1),1,DA)) I DA'>0 S DA=0 G END
1 ;
 S DIKZ(0)=$G(^IBD(357.95,DA(1),1,DA,0))
 S X=$P(DIKZ(0),U,1)
 I X'="" S ^IBD(357.95,DA(1),1,"B",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,1)
 I X'="" S:$P($G(^IBD(357.95,DA(1),1,DA,0)),"^",2)]"" ^IBD(357.95,"AC",DA(1),X,$P(^IBD(357.95,DA(1),1,DA,0),"^",2),DA)=""
 S X=$P(DIKZ(0),U,2)
 I X'="" S:X]"" ^IBD(357.95,"AC",DA(1),$P($G(^IBD(357.95,DA(1),1,DA,0)),"^"),X,DA)=""
 S X=$P(DIKZ(0),U,3)
 I X'="" I X,$P($G(^IBD(357.95,DA(1),1,DA,0)),"^",4)]"" S ^IBD(357.95,"AE",DA(1),X,+$P(^IBD(357.95,DA(1),1,DA,0),"^",10),$P(^(0),"^",4),DA)=""
 S X=$P(DIKZ(0),U,4)
 I X'="" I $P($G(^IBD(357.95,DA(1),1,DA,0)),"^",3),X]"" S ^IBD(357.95,"AE",DA(1),$P(^IBD(357.95,DA(1),1,DA,0),"^",3),+$P(^(0),"^",10),X,DA)=""
 S X=$P(DIKZ(0),U,10)
 I X'="" I $P(^IBD(357.95,DA(1),1,DA,0),"^",3),$P(^(0),"^",4)]"" S ^IBD(357.95,"AE",DA(1),$P(^IBD(357.95,DA(1),1,DA,0),"^",3),+X,$P(^(0),"^",4),DA)=""
 G:'$D(DIKLM) A Q:$D(DISET)
END G ^IBDX9510

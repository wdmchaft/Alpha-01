ORD11 ; COMPILED XREF FOR FILE #101 ; 04/16/99
 ; 
 S DIKZK=2
 S DIKZ(0)=$G(^ORD(101,DA,0))
 S X=$P(DIKZ(0),U,1)
 I X'="" K ^ORD(101,"B",$E(X,1,63),DA)
 S X=$P(DIKZ(0),U,2)
 I X'="" S ORKX=X D UP^ORDD1 K ^ORD(101,"C",$E(X,1,63),DA) S X=ORKX K ORKX
 S X=$P(DIKZ(0),U,2)
 I X'="" D REDO^ORDD101
 S X=$P(DIKZ(0),U,2)
 I X'="" D K12^ORDD1
 S DIKZ(5)=$G(^ORD(101,DA,5))
 S X=$P(DIKZ(5),U,1)
 I X'="" K ^ORD(101,"AE",$E(X,1,30),DA)
 S DIKZ(4)=$G(^ORD(101,DA,4))
 S X=$P(DIKZ(4),U,1)
 I X'="" D REDO^ORDD101
 S DIKZ(99)=$G(^ORD(101,DA,99))
 S X=$P(DIKZ(99),U,1)
 I X'="" D KILL^ORDD101
 S DIKZ(770)=$G(^ORD(101,DA,770))
 S X=$P(DIKZ(770),U,1)
 I X'="" N HL770 S HL770=$G(^ORD(101,DA,770)) K:$P(HL770,"^",3)&($P(HL770,"^",4)) ^ORD(101,"AHL1",X,$P(HL770,"^",3),$P(HL770,"^",4),DA)
 S X=$P(DIKZ(770),U,2)
 I X'="" K ^ORD(101,"AHL2",$E(X,1,30),DA)
 S X=$P(DIKZ(770),U,3)
 I X'="" N HL770 S HL770=$G(^ORD(101,DA,770)) K:$P(HL770,"^")&($P(HL770,"^",4)) ^ORD(101,"AHL1",$P(HL770,"^"),X,$P(HL770,"^",4),DA)
 S X=$P(DIKZ(770),U,4)
 I X'="" N HL770 S HL770=$G(^ORD(101,DA,770)) K:$P(HL770,"^")&($P(HL770,"^",3)) ^ORD(101,"AHL1",$P(HL770,"^"),$P(HL770,"^",3),X,DA)
END G ^ORD12

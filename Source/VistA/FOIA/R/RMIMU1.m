RMIMU1 ;WPB/JLTP ; FUNCTIONAL INDEPENDENCE UTILITIES CONT'D ; 30-AUG-2002
 ;;1.0;FUNCTIONAL INDEPENDENCE;;Apr 15, 2003
OF(IFN,VAL) ; File Other Fields
 N DA,DIE,DR,EDIT,FLD,I,II,LINE,LOC,X,Y
 S DR="" F LINE=1:1 S TEXT=$P($T(FLDS+LINE),";;",2) Q:TEXT=""  D
 .S FLD=$P(TEXT,U),LOC=$P(TEXT,U,2),SS=$P(LOC,","),LOC=$P(LOC,",",2)
 .I $P($G(VAL(SS)),U,LOC)="" Q
 .S EDIT=FLD_"///"_$P(VAL(SS),U,LOC)
 .I ($L(DR)+$L(EDIT))>200 D
 ..S DA=IFN,DIE="^RMIM(783," D ^DIE S DR=""
 .S:DR]"" DR=DR_";" S DR=DR_EDIT
 I DR]"" S DA=IFN,DIE="^RMIM(783," D ^DIE
 K ^RMIM(783,IFN,100) S I=9,II=0
 F  S I=$O(VAL(I)) Q:'I  S II=II+1,^RMIM(783,IFN,100,II,0)=VAL(I)
 S ^RMIM(783,IFN,100,0)="^783.01^"_II_U_II
 Q 1
GF(RMIMR) ; Retrieve Other Fields
 N FLD,GN,GP,LINE,N,P,X
 S IFN=+RMIMR(1)
 F LINE=1:1 S TEXT=$P($T(FLDS+LINE),";;",2) Q:TEXT=""  D
 .S FLD=$P(TEXT,U),P=$P(TEXT,U,2),N=$P(P,","),P=$P(P,",",2)
 .I +FLD=.16 S $P(RMIMR(N),U,12)=$P(^RMIM(783,IFN,0),U,16) Q
 .I +FLD=.17 S $P(RMIMR(N),U,13)=$P(^RMIM(783,IFN,0),U,17) Q
 .I FLD'=+FLD S $P(RMIMR(N),U,P)=$$GET1^DIQ(783,IFN,+FLD) Q
 .S X=$P(^DD(783,+FLD,0),U,4),GN=$P(X,";"),GP=$P(X,";",2)
 .S X=$P($G(^RMIM(783,IFN,GN)),U,GP)
 .I X S $P(RMIMR(N),U,P)=$$FMTE^XLFDT(X,5)
 S I=0,II=$O(RMIMR(""),-1)
 F  S I=$O(^RMIM(783,IFN,100,I)) Q:'I  S II=II+1,RMIMR(II)=^(I,0)
 Q
FLDS ;
 ;;.11^1,11
 ;;.16/^1,12
 ;;.17/^1,13
 ;;1.01/^2,1
 ;;1.02/^2,2
 ;;1.03/^2,3
 ;;1.04/^2,4
 ;;1.05/^2,5
 ;;1.06/^2,6
 ;;1.07/^2,7
 ;;1.08/^2,8
 ;;1.09/^2,9
 ;;2.01/^3,1
 ;;2.03/^3,3
 ;;2.04^3,4
 ;;2.05^3,5
 ;;2.06^3,6
 ;;2.07^3,7
 ;;2.08^3,8
 ;;2.09^3,9
 ;;2.1/^3,10
 ;;2.11/^3,11
 ;;3.01/^4,1
 ;;3.02/^4,2
 ;;3.03/^4,3
 ;;3.04/^4,4
 ;;3.05/^4,5
 ;;3.06/^4,6
 ;;3.07/^4,7
 ;;4.01/^5,1
 ;;4.02/^5,2
 ;;4.03/^5,3
 ;;4.04/^5,4
 ;;4.05/^5,5
 ;;4.06/^5,6
 ;;4.07/^5,7
 ;;4.08/^5,8
 ;;4.09/^5,9
 ;;4.1/^5,10
 ;;4.11/^5,11
 ;;4.12/^5,12
 ;;4.13/^5,13
 ;;4.14/^5,14
 ;;4.15/^5,15
 ;;4.16/^5,16
 ;;4.17/^5,17
 ;;4.18/^5,18
 ;;4.19/^5,19
 ;;4.2/^5,20
 ;;4.21/^5,21
 ;;5.01/^6,1
 ;;5.02/^6,2
 ;;5.03/^6,3
 ;;5.04/^6,4
 ;;5.05/^6,5
 ;;5.06/^6,6
 ;;5.07/^6,7
 ;;5.08/^6,8
 ;;5.09/^6,9
 ;;5.1/^6,10
 ;;5.11/^6,11
 ;;5.12/^6,12
 ;;5.13/^6,13
 ;;5.14/^6,14
 ;;5.15/^6,15
 ;;5.16/^6,16
 ;;5.17/^6,17
 ;;5.18/^6,18
 ;;5.19/^6,19
 ;;5.2/^6,20
 ;;5.21/^6,21
 ;;6.01/^7,1
 ;;6.02/^7,2
 ;;6.03/^7,3
 ;;6.04/^7,4
 ;;6.05/^7,5
 ;;6.06/^7,6
 ;;6.07/^7,7
 ;;6.08/^7,8
 ;;6.09/^7,9
 ;;6.1/^7,10
 ;;6.11/^7,11
 ;;6.12/^7,12
 ;;6.13/^7,13
 ;;6.14/^7,14
 ;;6.15/^7,15
 ;;6.16/^7,16
 ;;6.17/^7,17
 ;;6.18/^7,18
 ;;6.19/^7,19
 ;;6.2/^7,20
 ;;6.21/^7,21
 ;;7.01/^8,1
 ;;7.02/^8,2
 ;;7.03/^8,3
 ;;7.04/^8,4
 ;;7.05/^8,5
 ;;7.06/^8,6
 ;;7.07/^8,7
 ;;7.08/^8,8
 ;;7.09/^8,9
 ;;7.1/^8,10
 ;;7.11/^8,11
 ;;7.12/^8,12
 ;;7.13/^8,13
 ;;7.14/^8,14
 ;;7.15/^8,15
 ;;7.16/^8,16
 ;;7.17/^8,17
 ;;7.18/^8,18
 ;;7.19/^8,19
 ;;7.2/^8,20
 ;;7.21/^8,21
 ;;8.01/^9,1
 ;;8.02/^9,2
 ;;8.03/^9,3
 ;;8.04/^9,4
 ;;8.05/^9,5
 ;;8.06/^9,6
 ;;8.07/^9,7
 ;;8.08/^9,8
 ;;8.09/^9,9
 ;;8.1/^9,10
 ;;8.11/^9,11
 ;;8.12/^9,12
 ;;8.13/^9,13
 ;;8.14/^9,14
 ;;8.15/^9,15
 ;;8.16/^9,16
 ;;8.17/^9,17
 ;;8.18/^9,18
 ;;8.19/^9,19
 ;;8.2/^9,20
 ;;8.21/^9,21
 ;;

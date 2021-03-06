PRCHCRD3 ;WISC/DJM-LINK REPETITIVE ITEM DATA TO P.O.ITEM DATA-AFTER AMENDMENT ;6/24/94  9:28 AM
V ;;5.1;IFCAP;;Oct 20, 2000
 ;Per VHA Directive 10-93-142, this routine should not be modified.
EN3 ; Move Repetitive Item data to file 442, adds FCP to file 441.
 ; Called from OTHER^PRCHAMYD.
 ;
 ; PRCHCCP=FUND CONTROL POINT
 ; PRCHCV=VENDOR
 ; PRCHCPD=P.O. DATE
 ; PRCHCI=ITEM MASTER FILE NUMBER
 ; PRCHPO=P.O. RECORD NUMBER
 ; ITEM0=ITEM NODE 0 DATA
 ; ITEM2=ITEM NODE 2 DATA
 ; ITVEN0=VENDOR NODE 0 FROM FILE 441-VENDOR MULTIPLE
 ;
 N PO0,PO1,PRCHCPD,PRCHCCP,PRCHCI,PRCHCV,PRCHCX,ITEM0,ITEM2,ITVEN0,X,Z
 S PO0=$G(^PRC(442,PRCHPO,0)),PO1=$G(^PRC(442,PRCHPO,1)),PRCHCPD=+$P(PO1,U,15),PRCHCCP=$P($P(PO0,U,3)," ",1),PRCHCV=$P(PO1,U)
 S PRCHCI=$P($G(^PRC(442,PRCHPO,2,IMF1,0)),U,5)
 ;
 S PRCHCX=PRC("SITE")_PRCHCCP D  I $G(^PRC(441,PRCHCI,4,PRCHCX,1,0))="" S ^PRC(441,PRCHCI,4,PRCHCX,1,0)="^"_$P(^DD(441.03,1,0),U,2)_"^0^0"
 .I '$D(^PRC(441,PRCHCI,4)) S ^PRC(441,PRCHCI,4,0)="^"_$P(^DD(441,1,0),U,2)_"^0^0"
 .I '$D(^PRC(441,PRCHCI,4,PRCHCX,0)) S ^(0)=PRCHCX,^PRC(441,PRCHCI,4,"B",PRCHCX,PRCHCX)="",$P(^(0),U,3,4)=PRCHCX_U_($P(^PRC(441,PRCHCI,4,0),U,4)+1)
 ;
 S:'$D(^PRC(441,PRCHCI,4,PRCHCX,1,PRCHPO,0)) ^(0)=PRCHPO,^PRC(441,PRCHCI,4,PRCHCX,1,"AC",9999999-PRCHCPD,PRCHPO)="",$P(^(0),U,3,4)=PRCHPO_U_($P(^PRC(441,PRCHCI,4,PRCHCX,1,0),U,4)+1)
 ;
 I '$D(^PRC(441,PRCHCI,2)) S ^PRC(441,PRCHCI,2,0)="^"_$P(^DD(441,6,0),U,2)_"^0^0"
 I '$D(^PRC(441,PRCHCI,2,PRCHCV,0)) S ^(0)=PRCHCV,^PRC(441,PRCHCI,2,"B",PRCHCV,PRCHCV)="",$P(^(0),U,3,4)=PRCHCV_U_($P(^PRC(441,PRCHCI,2,0),U,4)+1)
 L +^PRC(441,PRCHCI,2,PRCHCV):5 I '$T Q
 S $P(^PRC(441,PRCHCI,0),U,4)=PRCHCV
 ;
 S ITEM0=$G(^PRC(442,PRCHPO,2,IMF1,0))
 S ITEM2=$G(^PRC(442,PRCHPO,2,IMF1,2))
 S ITVEN0=$G(^PRC(441,+PRCHCI,2,PRCHCV,0))
 ;
 ; UNIT OF PURCHASE
 S X=$P(ITEM0,U,3) I X]"" S $P(ITVEN0,U,7)=X
 ;
 ;ACTUAL UNIT COST and DATE OF UNIT PRICE
 S X=$P(ITEM0,U,9) I X]"" S $P(ITVEN0,U,2)=X,$P(ITVEN0,U,6)=$G(DT)
 ;
 ; NATIONAL STOCK NUMBER
 S X=$P(ITEM0,U,13) D:X]""
 .I $P(^PRC(441,+PRCHCI,0),U,5)]"" S Z=$P(^(0),U,5),Z(1)=$P(Z,"-",3,4),Z(2)=$E(Z,4)_$P(Z,"-",2)_$P(Z,"-",3)_$P(Z,"-",4) K ^PRC(441,"BB",Z,+PRCHCI) K:Z(1)]"" ^PRC(441,"BA",Z(1),+PRCHCI) K:Z(2)]"" ^PRC(441,"G",Z(2),+PRCHCI)
 .S Z(1)=$P(X,"-",3,4),Z(2)=$E(X,4)_$P(X,"-",2)_$P(X,"-",3)_$P(X,"-",4)
 .S ^PRC(441,"BB",X,+PRCHCI)="" S:Z(1)]"" ^PRC(441,"BA",Z(1),+PRCHCI)=""
 .S:Z(2)]"" ^PRC(441,"G",Z(2),+PRCHCI)=""
 .S $P(^PRC(441,+PRCHCI,0),U,5)=X
 ;
 ; VENDOR STOCK NUMBER
 S X=$P(ITEM0,U,6) D:X]""
 .I $P(ITVEN0,U,4)]"" K ^PRC(441,"D",$P(ITVEN0,U,4),+PRCHCI,PRCHCV)
 .S $P(ITVEN0,U,4)=X,^PRC(441,"D",X,+PRCHCI,PRCHCV)=""
 ;
 ; CONTRACT NUMBER
 S X=$P(ITEM2,U,2) I X]"" S X=$O(^PRC(440,PRCHCV,4,"B",X,0)) S:X>0 $P(ITVEN0,U,3)=X
 ;
 ; PACKAGING MULTIPLE
 S X=$P(ITEM0,U,12) S:X]"" $P(ITVEN0,U,8)=X
 ;
 ; FEDERAL SUPPLY CLASSIFICATION
 S X=$P(ITEM2,U,3) S:X]"" $P(^PRC(441,+PRCHCI,0),U,3)=X
 ;
 ; MAXIMUM ORDER QUANTITY
 S X=$P(ITEM0,U,14) S:X]"" $P(ITVEN0,U,9)=X
 ;
 ; STOCK KEEPING UNIT
 S X=$P(ITEM0,U,16) S:X]"" $P(^PRC(441,+PRCHCI,3),U,8)=X
 ;
 ; UNIT CONVERSION FACTOR
 S X=$P(ITEM0,U,17) S:X]"" $P(ITVEN0,U,10)=X
 ;
 ; NATIONAL DRUG CODE
 S X=$P(ITEM0,U,15) S:X]"" $P(ITVEN0,U,5)=X
 ;
 ; BOC
 ;S X=+$P(ITEM0,U,4) S:X]"" $P(^PRC(441,PRCHCI,0),U,10)=X
 ;
 ; NOW SAVE ITVEN0
 S ^PRC(441,+PRCHCI,2,PRCHCV,0)=ITVEN0
 L -^PRC(441,PRCHCI,2,PRCHCV)
 Q

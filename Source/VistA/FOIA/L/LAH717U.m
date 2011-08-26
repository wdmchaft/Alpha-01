LAH717U ;SLC/DLG - HITACHI 717 ROUTINE FOR AUTOMATED DATA ;7/20/90  09:10 ;
 ;;5.2;AUTOMATED LAB INSTRUMENTS;;Sep 27, 1994
 ;CROSS LINK BY ID=ACCESSION OR IDE=MACHINE SEQUENCE OR TRAY/CUP
LA1 S:$D(ZTQUEUED) ZTREQ="@" S LANM=$T(+0),TSK=$O(^LAB(62.4,"C",LANM,0)) Q:TSK<1
 Q:'$D(^LA(TSK,"I",0))
 K LATOP D ^LASET Q:'TSK  S X="TRAP^"_LANM,@^%ZOSF("TRAP")
LA2 K TV,Y S TOUT=0,A=2 D IN G QUIT:TOUT,LA2:CTRL'="C",LA2:"NEC"'[$E(IN,3) D QC
 ;S TOUT=0,BAD=0 D IN,QC G QUIT:TOUT
 F I=25:9:255 S V1=$E(Y(2),I,(I+8)),V=$E(V1,1,2) D NUM I V S TEST=+V,V=$E(V1,3,8) D NUM I V]"" S:$D(TC(TEST,1))#2 @TC(TEST,1)=V ;V COULD BE MODIFIED ON THIS LINE
 I "NE"[$E(Y(2),3) S V=$E(Y(2),13,23) D NUM S ID=+V,V=$E(Y(2),4,7) D NUM S IDE=+V,TRAY=$E(Y(2),9),V=$E(Y(2),10,11) D NUM S CUP=+V
 I $E(Y(2),3)="C" S V=$E(Y(2),6,7) D NUM S IDE=+V,(TRAY,CUP,ID)=""
LA3 X LAGEN G LA2:'ISQN ;Can be changed by the cross-link code
 F I=0:0 S I=$O(TV(I)) Q:I<1  S:TV(I,1)]"" ^LAH(LWL,1,ISQN,I)=TV(I,1)
 G LA2
QC ;QC TESTING HERE; S BAD=1 IF DONT STORE
 S Y(A)=IN Q
NUM S X="" F JJ=1:1:$L(V) S:$A(V,JJ)>32 X=X_$E(V,JJ)
 S V=X Q
IN S CNT=^LA(TSK,"I",0)+1 IF '$D(^(CNT)) S TOUT=TOUT+1 Q:TOUT>9  H 5 G IN
 S ^LA(TSK,"I",0)=CNT,IN=^(CNT),TOUT=0
 S:IN["~" CTRL=$P(IN,"~",2),IN=$P(IN,"~",1)
 Q
OUT S CNT=^LA(TSK,"O")+1,^("O")=CNT,^("O",CNT)=OUT
 LOCK ^LA("Q") S Q=^LA("Q")+1,^("Q")=Q,^("Q",Q)=TSK LOCK
 Q
QUIT LOCK ^LA(TSK) H 1 K ^LA(TSK),^LA("LOCK",TSK),^TMP($J),^TMP("LA",$J)
 Q
TRAP D ^LABERR S T=TSK D SET^LAB G @("LA2^"_LANM) ;ERROR TRAP

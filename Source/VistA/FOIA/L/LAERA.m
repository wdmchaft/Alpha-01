LAERA ;SLC/DLG - PHOTON ERA ;7/20/90  08:21 ;
 ;;5.2;AUTOMATED LAB INSTRUMENTS;;Sep 27, 1994
 ;CROSS LINK BY ID OR IDE
LA1 S:$D(ZTQUEUED) ZTREQ="@" S LANM=$T(+0),TSK=$O(^LAB(62.4,"C",LANM,0)) Q:TSK<1
 Q:'$D(^LA(TSK,"I",0))
 K LATOP D ^LASET Q:'TSK  S X="TRAP^"_LANM,@^%ZOSF("TRAP")
LA2 K TV,Y S TOUT=0 D IN G QUIT:TOUT,LA2:$E(IN,1,2)'="RH"
 S TEST=$P(IN,",",3),TEST=$O(^LAB(60,"B",TEST,0)) G LA2:'TEST S I=0
 F J=0:0 S I=$O(TC(I)) Q:I<1  I '$D(TC(I,0)),TC(I,0)'=TEST K TC(I)
LA2A S TOUT=0,BAD=0 D IN G LA2:$E(IN,1,2)="ET",LA2A:$E(IN,1,2)'="RR"
 S V=$P(IN,",",3) D NUM S TRAY=V,V=$P(IN,",",4) D NUM
 S CUP=V,V=$P(IN,",",2) D NUM S ID=V,V=$P(IN,",",9) D NUM
 S I=$O(TC(0)) G LA2:I<1 S @TC(I,1)=V
LA3 X LAGEN G LA2A:'ISQN ;Can be changed by the cross-link code
 S:TV(I,1)]"" ^LAH(LWL,1,ISQN,I)=TV(I,1)
 G LA2A
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

LADEKT7B ;SLC/RWF/DLG - EKTACHEM 700 BI-DIRECTIONAL  ;7/23/90  11:04 ;
 ;;5.2;AUTOMATED LAB INSTRUMENTS;;Sep 27, 1994
 ;CROSS LINK BY ID OR IDE
LA1 S:$D(ZTQUEUED) ZTREQ="@" S LANM=$T(+0),TSK=$O(^LAB(62.4,"C",LANM,0)) Q:TSK<1
 Q:'$D(^LA(TSK,"I",0))
 K LATOP D ^LASET Q:'TSK  S X="TRAP^"_LANM,@^%ZOSF("TRAP"),U="^"
LA2 K TV,TY,Y S TOUT=0,A=1,RMK="" D IN G QUIT:TOUT,LA2:$E(IN,1,6)'?6N D QC
 S LATM=$E(IN,1,6),V=$E(IN,11,25) D NUM S TRAY=+$P(V,"TRAY",2),V=$E(IN,26,40) D NUM S IDE=+V,LAFL=$E(IN,41),LAV6=$E(IN,42),CUP=$A(IN,43)-32,LAMD=$E(IN,44),V=$E(IN,45,49) D NUM S DILU=+X,Y(1)=$E(IN,50,999)
 I Y(1)'["]" F A=2:1 D IN,QC Q:IN["]"
 S IN="" F A1=1:1:A S IN=IN_Y(A1) F I=1:1:$L(IN,"}")-1 S Y=$P(IN,"}",1),IN=$P(IN,"}",2,99),V=$E(Y,2,10),E=$E(Y,12),OV="" D NUM S:"0127"'[E OV=V,V="" S TY($E(Y))=V_"^"_$E(Y,11,22)_$S(OV]"":"^"_OV,1:"")
 F I=0:0 S I=$O(^TMP("LA",$J,I)),V="" Q:I<1  X ^TMP("LA",$J,I,2) I V]"" S @^TMP("LA",$J,I,1)=V
 S ID=IDE
LA3 X LAGEN G LA2:'ISQN ;Can be changed by the cross-link code
 F I=0:0 S I=$O(TV(I)) Q:I<1  S:TV(I,1)]"" ^LAH(LWL,1,ISQN,I)=TV(I,1)
 I $D(RMK),$L(RMK) D RMK^LASET
 G LA2
QC ;QC TESTING HERE; S BAD=1 IF DONT STORE
 S Y(A)=IN Q
NUM S X="" F JJ=1:1:$L(V) S:$A(V,JJ)>32 X=X_$E(V,JJ)
 S V=X Q
IN S:'$D(^LA(TSK,"C",0)) ^LA(TSK,"C")=0,^("C",0)=0 I ^LA(TSK,"C")=^LA(TSK,"C",0) D ^LADKERM3 Q:TOUT
 S CNT=^LA(TSK,"C",0)+1 IF '$D(^(CNT)) S TOUT=TOUT+1 Q:TOUT>9  H 5 G IN
 S ^LA(TSK,"C",0)=CNT,IN=^(CNT),TOUT=0
 S:IN["~" CTRL=$P(IN,"~",2),IN=$P(IN,"~",1)
 Q
OUT S CNT=^LA(TSK,"O")+1,^("O")=CNT,^("O",CNT)=OUT
 LOCK ^LA("Q") S Q=^LA("Q")+1,^("Q")=Q,^("Q",Q)=TSK LOCK
 Q
QUIT G:'$D(^LA(TSK,"I")) Q1 G:^LA(TSK,"I")>^LA(TSK,"I",0) LA2 I $D(^LA(TSK,"P")),^("P")'["QUIT" H 60 G QUIT
 I ^LA(TSK,"O")'=^LA(TSK,"O",0) H 60 G QUIT
Q1 K ^LA("LOCK",TSK) LOCK ^LA(TSK) H 1 K ^LA(TSK,"I"),^("C") L
 Q
TRAP D ^LABERR S T=TSK D SET^LAB G @("LA2^"_LANM) ;ERROR TRAP
 Q

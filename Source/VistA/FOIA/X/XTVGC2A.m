XTVGC2A ;ISC-SF/SEA/JLI - COMPARE SAVED AND CURRENT NAMESPACE DATA ;12/13/93  15:18 ;
 ;;7.3;TOOLKIT;;Apr 25, 1995
ENTRY ;
 F XTTYJ=1:1:8 D
 .K ^TMP($J) S XTTY=$P("OPTIONS;DIC(19,^KEYS;DIC(19.1,^HELP FRAMES;DIC(9.2,^BULLETINS;XMB(3.6,^FUNCTIONS;DD(""FUNC"",^EDIT TEMPLATES;DIE(^SORT TEMPLATES;DIBT(^PRINT TEMPLATES;DIPT(^S;SCRE(",U,XTTYJ)
 .S XTNOD=U_$P(XTTY,";",2),XTTY=$P(XTTY,";"),XTBAS1="^XTV(8991.2,XTVPT,1,XTVD,2," S XTTYI=$O(@(XTBAS1_"""B"","""_XTTY_""",0)")) D  D COMPAR
 ..W !,"*********************************  Checking ",XTTY
 ..I XTTYI>0 S %X=XTBAS1_XTTYI_",1,""B"",",%Y="^TMP($J,""B""," D %XY^%RCR I XTTYJ>5,XTTYJ<9 D
 . . . S XTTYA="" F  S XTTYA=$O(^TMP($J,"B",XTTYA)) Q:XTTYA=""  F XTTYK=0:0 S XTTYK=$O(^TMP($J,"B",XTTYA,XTTYK)) Q:XTTYK'>0  S XTTYM=^(XTTYK) D
 . . . . S XTTYKM=$P($G(@(XTBAS1_XTTYI_",1,"_XTTYK_",1,1,1)")),U,4) I XTTYKM'="" K ^TMP($J,"B",XTTYA,XTTYK) S ^TMP($J,"Z",(XTTYA_" ("_XTTYKM_")"),XTTYK)=XTTYM
 . . . S %X="^TMP($J,""Z"",",%Y="^TMP($J,""B""," D %XY^%RCR K ^TMP($J,"Z")
 . . K ^TMP($J,"XB") S %X=XTNOD_"""B"",",%Y="^TMP($J,""XB""," D %XY^%RCR I XTTYJ>5,XTTYJ<9 D
 . . . S XTTYA="" F  S XTTYA=$O(^TMP($J,"XB",XTTYA)) Q:XTTYA=""  F XTTYK=0:0 S XTTYK=$O(^TMP($J,"XB",XTTYA,XTTYK)) Q:XTTYK'>0  D
 . . . . S XTTYKM=$P($G(@(XTNOD_XTTYK_",0)")),U,4) I XTTYKM'="" K ^TMP($J,"XB",XTTYA,XTTYK) S ^TMP($J,"Z",(XTTYA_" ("_XTTYKM_")"),XTTYK)=""
 . . . S %X="^TMP($J,""Z"",",%Y="^TMP($J,""XB""," D %XY^%RCR K ^TMP($J,"Z")
 K ^TMP($J),XTTYA,XTTYK,XTTYKM,XTTYM
 Q
COMPAR ;
 S:'$D(XTVPK) XTVPK=+^XTV(8991.2,XTVPT,0) S XTNS="" F XTNSI=0:0 S XTNSI=$O(^XTV(8991.19,XTVPK,2,XTNSI)) Q:XTNSI'>0  S XTNS($P(^(XTNSI,0),U))=""
 F  S XTNS=$O(XTNS(XTNS)) Q:XTNS=""  S XTNSL=$L(XTNS),XTNS1=$E(XTNS,1,XTNSL-1)_$C($A($E(XTNS,XTNSL))-1)_"z",XTVAL=XTNS1 D COMPAR1
 S XTVAL="" D COMPAR2
 Q
COMPAR1 ;
 F  S XTVAL=$O(^TMP($J,"XB",XTVAL)) Q:$E(XTVAL,1,XTNSL)'=XTNS  D
 .K XTNUM,^TMP($J,"N") S XTNUMN=0 F XTNUM=0:0 S XTNUM=$O(^TMP($J,"XB",XTVAL,XTNUM)) D:XTNUM'>0  Q:XTNUM'>0  S XTNUM(XTNUM)="",XTNUMN=XTNUMN+1
 ..I XTNUMN>1 W !?2,XTNUMN," ENTRIES OF ",XTVAL," IN B X-REF OF CURRENT ",XTTY," FILE"
 ..F XTNUM=0:0 S XTNUM=$O(XTNUM(XTNUM)) Q:XTNUM'>0  I $D(@(XTNOD_XTNUM_",0)")) S ^TMP($J,"N",($P(^(0),U)_$S(XTTYJ>5&(XTTYJ<9):" ("_$P(^(0),U,4)_")",1:"")),XTNUM)=""
 .. S XTVAL2="" F  S XTVAL2=$O(^TMP($J,"N",XTVAL2)) Q:XTVAL2=""  D COMPAR2
 Q
COMPAR2 ;
 S XTVAL1=""
 K XTN S XTVAL1=$O(^TMP($J,"B",XTVAL1)) I XTVAL1'="" S XTNN=0 F XTN=0:0 S XTN=$O(^TMP($J,"B",XTVAL1,XTN)) Q:XTN'>0  S XTN(XTN)=^(XTN),XTNN=XTNN+1
 Q:XTVAL1=""&(XTVAL2="")
 I XTVAL1'="",XTVAL2=""!(XTVAL2]XTVAL1) D  G COMPAR2 ; XTVAL1 BEFORE XTVAL
 .I XTNN>1 W !?2,XTNN," ENTRIES OF ",XTVAL1," IN B X-REF OF OLD ",XTTY," FILE"
 .K ^TMP($J,"O"),^("OB") F XTN=0:0 S XTN=$O(XTN(XTN)) Q:XTN'>0  I $D(@(XTBAS1_"XTTYI,1,XTN,1,1,1)")) S X=$P(^(1),U)_$S(XTTYJ>5&(XTTYJ<9):" ("_$P(^(1),U,4)_")",1:""),^TMP($J,"O",X,XTN(XTN))=XTN
 .S X="" F  S X=$O(^TMP($J,"O",X)) Q:X=""  F XTNN=0:0 S XTNN=$O(^TMP($J,"O",X,XTNN)) Q:XTNN'>0  S XTIEN=XTNN,XTN=^(XTNN) D
 ..W !!,"DELETED ",$E(XTTY,1,$L(XTTY)-1),":  ",X,"    OLD IEN=",XTN
 ..S XTBAS2=XTBAS1_"XTTYI,1,XTN,1," F I=0:0 S I=$O(@(XTBAS2_I_")")) Q:I'>0  S X1=@(XTBAS2_I_",0)"),X2=@(XTBAS2_I_",1)"),X1=$P(X1,"~IEN~")_XTIEN_$P(X1,"~IEN~",2) W !?5,X1," = ",X2
 .K ^TMP($J,"B",XTVAL1)
 I XTVAL1=""!(XTVAL1]XTVAL2) D  Q  ; XTVAL BEFORE XTVAL1
 .S X="" F  S X=$O(^TMP($J,"N",X)) Q:X=""  F XTNN=0:0 S XTNN=$O(^TMP($J,"N",X,XTNN)) Q:XTNN'>0  S XTIEN=XTNN D  K ^TMP($J,"N",X)
 ..W !!,"ADDED ",$E(XTTY,1,$L(XTTY)-1),":  ",X,"     IEN=",XTIEN
 ..S XTBAS=XTNOD_XTIEN,XTBASI=XTBAS_",",XTBAS=XTBAS_")"
 ..F  S XTBAS=$Q(@XTBAS) S XTVG=XTBAS D MSMQ^XTVGC2 S XTBAS=XTVG Q:XTBAS'[XTBASI  W !?5,XTBAS," = ",@XTBAS
 S XTSEEN=0 D  D CHANGE K ^TMP($J,"B",XTVAL1)
 .I XTNN>1 W !?2,XTNN," ENTRIES OF ",XTVAL1," IN B X-REF OF OLD ",XTTY," FILE"
 .K ^TMP($J,"O"),^("OB") F XTN=0:0 S XTN=$O(XTN(XTN)) Q:XTN'>0  S X=$P(@(XTBAS1_"XTTYI,1,XTN,1,1,1)"),U),^TMP($J,"O",X,XTN(XTN))=XTN
 .S X=""  F  S X=$O(^TMP($J,"O",X)) Q:X=""  F XTNN=0:0 S XTNN=$O(^TMP($J,"O",X,XTNN)) Q:XTNN'>0  S XTIEN=XTNN,XTN=^(XTNN)
 .S X="" F  S X=$O(^TMP($J,"N",X)) Q:X=""  F XTNN=0:0 S XTNN=$O(^TMP($J,"N",X,XTNN)) Q:XTNN'>0  S XTNN(1)=XTNN
 .S XTNN=XTNN(1)
 Q
CHANGE ;
 S XTVG=XTNOD_XTNN_")",XTVG1=XTNOD_XTNN_",",XTVF0=XTBAS1_"XTTYI,1,XTN,1,M,0)"
 F M=1:1 S:XTVG'="" XTVG=$Q(@XTVG) D MSMQ^XTVGC2 S:XTVG'[XTVG1 XTVG="" S XTVGY=$S(XTVG="":"",1:@XTVG) S XTVX=$S('$D(@XTVF0):"",1:^(0)),XTVY=$S(XTVX="":"",1:^(1)) Q:XTVG=""&(XTVX="")  D CHECK^XTVGC2A1 ; Naked is based on @XTVF0
 Q

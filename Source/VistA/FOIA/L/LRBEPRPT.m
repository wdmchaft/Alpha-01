LRBEPRPT ;DALOI/FHS - PRINT CPT CODES FOR TESTS AND PANELS ;03/30/2005
 ;;5.2;LAB SERVICE;**291,359**;Sep 27, 1994
BEBA ;Individual test CPT code look-up
 K ^TMP("LR",$J,"VTO"),^TMP("LR",$J,"TMP")
 S (LRBEPSY,LRBECPT)="" K DIR,X,Y,LRSTOP,LRPG
 S LRBECDT=DT
 S DIR(0)="PO^60:AQEZNM"
 S DIR("S")="I $S('$L($P(^(0),U,3)):0,$P(^(0),U,3)=""N"":0,1:1)"
 D ^DIR K DIR
 I $G(Y)<1 D CLEAN Q
 W @IOF
 S LRBELOP=1,LRBEBY=Y,LRPRT=0
 D LOOP
 I $G(OUT) D  Q
 . W !,+LRBEBY_"  = "_$P(^LAB(60,+LRBEBY,0),U)
 . W !,"**********     ","Invalid data prevents display.",!
 D:'$G(OUT) T1
 D CLEAN
 G BEBA
LOOP ;
 Q:$G(LRBEPSY)!($G(LRSTOP))
 S:'$G(LRSPEC) LRSPEC=99999999 S:'$G(LRBECDT) LRBECDT=DT
 K DIC,LRTEST,LRNAME,LRNLT,T1,LRY,LRBECPT,LRORD
 K LRM,LRMX
 K ^TMP("LR",$J,"VTO"),^TMP("LR",$J,"TMP")
 S LRBECPT=""
 S X=+LRBEBY_U_+LRBEBY,LRNLT=+LRBEBY,LRTEST(1,"P")=LRNLT_U_$$NLT^LRVER1(LRNLT)
 S T1=1,LRTEST(T1)=+LRBEBY_U_^LAB(60,+LRBEBY,0)
 S OUT=0,XX=$P(LRTEST(T1),U,6) D  Q:OUT
 . I XX'="",+$P(XX,";",2)=0 S OUT=1 Q
 . I XX'="" S LRBEY(+LRBEBY,+$P(XX,";",2))=""
 . I $D(^LAB(60,+LRBEBY,2)) D
 . . S I=0 F  S I=$O(^LAB(60,+LRBEBY,2,I)) Q:'I  D
 . . . S K=$P(^LAB(60,+LRBEBY,2,I,0),U,1)
 . . . S XX=$P(^LAB(60,K,0),U,5) I XX'="",+$P(XX,";",2)=0 S OUT=1 Q
 . . . S LRBEY(+LRBEBY,+$P(XX,";",2))=""
 K LRBECPT
 S LRBETST=0,(LRPANEL,LRBEPYS)=""
 S LRBETST=$O(LRBEY(LRBETST)) Q:LRBETST<1!($G(LRSTOP))  D
 . Q:'$G(^LAB(60,LRBETST,12))
 . Q:$L($P(^LAB(60,LRBETST,0),U,5))
 . D PANEL^LRBEBA4
 . I '$O(LRBECPT(LRBETST,0)) Q
 . S LRPANEL="*"
 . S LRBEPSY=1 D DISPLAY
 . S (LRPANEL)=""
 . W !
PANEL ;Display panel test CPT
 I $G(LRBEPSY) K LRBECPT Q
 I $O(LRBECPT(0)) D  Q:$G(LRBELOP)!($G(LRSTOP))
 . S LRBEPYS=1 D DISPLAY
 Q:$S($G(LRBEPO):1,$G(LRBEPYS):1,1:0)
 K LRBECPT
 S LRTST=0,LRBETST=+LRBEBY
NLT ;
 S LRBECDT=DT D PANEL^LRBEBA4
 Q:'$O(LRBECPT(0))
 S LRI=0 F  S LRI=$O(LRBECPT(LRBETST,LRI)) Q:LRI<1!($G(LRSTOP))  D
 . D LN Q:$G(LRSTOP)
 . ;W !,"{"_LRBETST_"="_$P(^LAB(60,LRBETST,0),U)_"} "_$P($G(LRBEBY),U,2)
 . ;W:$D(^LAB(61,LRSPEC,0))#2 "  (SPEC) = "_$E($P(^(0),U),1,30)
 . S LRREQ=$S($P($G(^LAB(60,LRBETST,0)),U,17):"r ",1:" ")
 . W !,LRBETST_" = "_LRREQ_$P($G(LRBEBY),U,2)_"  "  ;_$P(^LAB(60,LRBETST,0),U)
 . W:$D(^LAB(61,LRSPEC,0))#2 " (SPEC) = "_$E($P(^(0),U),1,30)
 . D WRT
 Q
 ;
DISPLAY ;
 Q:$G(LRSTOP)  K LRREQ
 I $O(LRBECPT(0)),'$G(LRPRT) W ! S LRPRT=1
 S LRBETST=0 F  S LRBETST=$O(LRBECPT(LRBETST)) Q:LRBETST<1!($G(LRSTOP))  D
 . D LN Q:$G(LRSTOP)
 . S LRREQ=$S($P($G(^LAB(60,LRBETST,0)),U,17):"r ",1:"")
 . W !,LRBETST_" = "_LRREQ_$P(^LAB(60,LRBETST,0),U) I $D(^LAB(61,LRSPEC,0))#2 W " / "_$P(^(0),U)
 . S LRI=0 F  S LRI=$O(LRBECPT(LRBETST,LRI)) Q:LRI<1  D WRT
 Q
WRT ;
 D LN Q:$G(LRSTOP)
 S LRPRT=1
 S LRCPT=$O(LRBECPT(LRBETST,LRI,0)) Q:'LRCPT!($G(LRSTOP))  D
 . D LN Q:$G(LRSTOP)
 . S XCODE=$$CPT^ICPTCOD(LRCPT)
 . W !?6,$P(XCODE,U,2)_"  "_$P(XCODE,U,3)_"  ["_LRBECPT(LRBETST,LRI,LRCPT)_"]"_$G(LRPANEL)
 . K LRBECPT(LRBETST,LRI,LRCPT),XCODE
 Q
 ;
LN ;Check line spacing
 Q:$Y<(IOSL-4)!($G(LRSTOP))
 I $E(IOST,1)="C" D  Q:LRSTOP
 . N DIR S DIR(0)="E" D ^DIR S LRSTOP=$G(DIRUT)
HDR ;Report header
 W @IOF,! S LRPG=$G(LRPG)+1
 W $$CJ^XLFSTR($G(LRHDR)_"         "_$$FMTE^XLFDT(DT,5)_"    Pg "_LRPG,IOM),!
 Q
 ;
TEST ;List all test with CPT codes
 ;logic follows the Billing Aware CPT logic Hierarchy
  U IO
  I '$G(LRPG) D HDR
 S LRBELOP=1,LRBECDT=DT
  S LRTN="^LAB(60,""B"")"
 F  Q:$G(LRSTOP)  S LRTN=$Q(@LRTN) Q:$QS(LRTN,2)'="B"  D
 . Q:$G(@LRTN)
 . S LRSPEC=999999,LRBEBY=$QS(LRTN,4)
 . Q:$S('$D(^LAB(60,LRBEBY,0))#2:1,'$L($P(^(0),U,3)):1,$P(^(0),U,3)="N":1,1:0)
 . Q:$G(LRSTOP)
 . S LRPRT=0,LRBEPSY=""
 . S LRBEBY=LRBEBY_U_$P(^LAB(60,+LRBEBY,0),U)
 . D LOOP
 . Q:$G(OUT)
 . Q:$G(LRSTOP)
 . D T1
 . K LRBEY,LRBEBY
 . Q:$G(LRSTOP)
 D CLEAN
 Q
 ;
T1 ;
 S LRSPEC=0 F  S LRSPEC=$O(^LAB(60,+LRBEBY,1,LRSPEC)) Q:LRSPEC<1!($G(LRSTOP))  D
 . D LOOP
 . K LRBECPT
 Q:$G(OUT)
 Q:$G(LRSTOP)
  D ATOMIC
 I $G(LRPRT) W !
 K LRBEPSY
 Q
 ;
ATOMIC ; Print Atomic test of panel
 Q:'$O(^LAB(60,+LRBEBY,2,0))
 W !,+LRBEBY_" = "_$P(^LAB(60,+LRBEBY,0),U),!,"**********"
 N LRBEBSY,LRII,LRREQ
 S LRBEBSY=+LRBEBY N LRII S LRII=0
 F  S LRII=$O(^LAB(60,LRBEBSY,2,LRII)) Q:LRII<1!($G(LRSTOP))  D
 . S LRBEBY=+$G(^LAB(60,LRBEBSY,2,LRII,0)) Q:'LRBEBY!($G(LRSTOP))  D
 . . S LRREQ=$S($P($G(^LAB(60,LRBEBY,0)),U,17):"r",1:" ")
 . . I $D(^LAB(60,LRBEBY,0)) S LRBEBY=LRREQ_"["_LRBEBY_"]  "_$P(^(0),U) D
 . . . W ?15,LRBEBY,! D LN
 Q
 ;
ASK ;Present user with a selection of options
 K DIR,Y,LRSTOP,POP,ZTRTN,OUT,OPT,XX
 K LRDEV,ZTDTH,ZTDESC,ZTIO,ZTSAVE
 S OUT=0
 S LRBECDT=DT
 S DIR(0)="SO^1:Single Test Look-up;2:List Panels Only;3:List All Test"
  D ^DIR K DIR
 I Y<1 G CLEAN
 S OPT=Y
 I OPT=1 D  G ASK
 . S LRHDR="*** Single Test code listing (CPT) ***"
 . D BEBA
 I OPT=2 D  G ASK
 . S LRHDR="*** Panel Tests Only (CPT) ***",LRBEPO=1
 . D DEV I $G(POP) D ^%ZISC Q
 . I IO'=IO(0) D  Q
 . . S ZTSAVE("LRBEPO")=""
 . . S ZTRTN="TEST^LRBEPRPT"
 . . D LOAD
 . D TEST
 I OPT=3 D  G ASK
 . D DEV I $G(POP) D ^%ZISC Q
 . S LRHDR="*** All Lab Tests (CPT) ***"
 . I IO'=IO(0) D  Q
 . . S ZTRTN="TEST^LRBEPRPT"
 . . D LOAD
 . D TEST
 Q
 ;
DEV ;Select print device
 N %ZIS,LRMSG,ZTDESC,ZTDTH,ZTIO,ZTSK,ZTSAVE
 S %ZIS="NQ",%ZIS("A")=" Select Print Device: "
 S (LRDEV,%ZIS("B"))="Home" D ^%ZIS
 Q
LOAD ;%ZTLOAD section
 S ZTDTH=$H
 S ZTDESC=$G(LRHDR)
 S (LRDEV,ZTIO)=ION,ZTSAVE("LRHDR")=""
 D ^%ZTLOAD W @IOF,!,$S($G(ZTSK):"Queued to device "_LRDEV,1:"Not Queued"),!
 D ^%ZISC
 D HOME^%ZIS
 Q
 ;
CLEAN ;Clean-up
 W:$D(ZTQUEUED) @IOF
 K DIC,DIR,DIRUT,DTOUT,DUOUT
 K LRBEBY,LRBECPT,LRBECDT,LRBEDT,LRBELOP,LRBEPYS,LRBEYS,LRBENLT,LRBETST,LRBEY,LRCFL
 K LRCPT,LRI,LRIEN,LRM,LRMX,LRNAME,LRNLT,LRNX,LRORD,LRBEPO,LRPANEL,LRPRT
 K LRSPEC,LRSTOP,LRSUB,LRTEST,LRTN,LRTST,LRXX,LRY,S2,T1,X,Y,YY
 K LRHDR,LRDEV,LRPG,POP
 K ^TMP("LR",$J,"VTO"),^TMP("LR",$J,"TMP")
 D ^%ZISC
 Q

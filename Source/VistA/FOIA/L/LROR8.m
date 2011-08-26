LROR8 ;SLC/DCM - FLAG/HOLD ORDERS ;5/1/89  17:46 ;
 ;;5.2;LAB SERVICE;**100,128**;Sep 27, 1994
EN ;;from LROR to FLAG orders
 I ORSTS'=5,ORSTS'=4 W !,"Only PENDING lab orders can be flagged." Q
 S ORSTS=$S(ORSTS=4:5,ORSTS=5:4,ORSTS="":4,1:"") D:ORSTS'="" ST^ORX
 Q
EN1 ;;from LROR to HOLD orders
 I ORGY=0 W !!,"Lab orders cannot be put on HOLD.  Do you want to CANCEL the order",$C(7) S %=2 D YN^DICN Q:%'=1
 D C^LROR3 Q
 Q
EN2 ;Verify unreleased lab orders
 Q  ;Disable verify - now done when released
EN3 ;Verify upon release
 I ORSTS'=11,ORSTS'="" Q
 N LRSTS
 S LRSTS=ORSTS,LRSX=0,LRASK=0,LRORIFN=ORIFN,LREND=0,X=ORPK,LRTST=+X,LROST=$P(X,"^",2),LRSAMP=$P(X,"^",3),LRSPEC=$P(X,"^",4),LRZX(1)=$P(X,"^",5),LRURG=$P(X,"^",6),LRORD=$P(X,"^",7),LRI=1,LRTEST(LRI)=LRTST
 I 'LRORD D GET
 I 'LRTST!('LROST)!('LRSAMP)!('LRSPEC)!('$L(LRZX(1)))!('LRURG)!('$L(LRORD)) W !,"Incomplete data!  This order cannot be released." D READ^ORUTL S OREND=1 D END Q
 S LRTSTNM=$S($D(^LAB(60,LRTST,0)):$P(^(0),"^"),1:"")
 D NOW^%DTC S LRNOW=%
 N GOT,LRNSN,LRODT S GOT=0,LRODT=$P(LROST,".")
 I $D(^LRO(69,"C",LRORD,LRODT)) S LRSN=0 F  S LRSN=$O(^LRO(69,"C",LRORD,LRODT,LRSN)) Q:LRSN<1  D  Q:GOT
 . I $D(^LRO(69,LRODT,1,LRSN,0)),$P(^(0),"^",3)=LRSAMP,$D(^(4,1,0)),+^(0)=LRSPEC,$P($G(^LRO(69,LRODT,1,LRSN,3)),"^") S GOT=1
 I GOT W $C(7),!!,"The specimen for test "_LRTSTNM_", has already been processed by Lab.",!,"Please create a new order, or contact lab to have this test added." S ORSTS="K" D ST^ORX W !?5,LRTSTNM_" Deleted" S OREND=1 D END,READ^ORUTL Q
 I LROST["."&(LRNOW>(LROST+.0002)) D COL I LREND S OREND=1 D END Q
 I LROST'[".",$P(LRNOW,".",1)>LROST D COL I LREND S OREND=1 D END Q
 D DUP I LREND D END Q
S1 S LRSX=LRSX+1 I $D(^XUTL("OR",$J,"LROT",LROST,LRZX(1),LRSAMP,LRSPEC,LRSX)) G S1
 S:'$G(^XUTL("OR",$J,"LROT",LROST,LRZX(1))) ^(LRZX(1))=LRORD
 S ^XUTL("OR",$J,"LROT",LROST,LRZX(1),LRSAMP,LRSPEC,LRSX,1)=LRURG,^XUTL("OR",$J,"LROT",LROST,LRZX(1),LRSAMP,LRSPEC,LRSX,0)=LRORIFN,^XUTL("OR",$J,"LROT",LROST,LRZX(1),LRSAMP,LRSPEC,LRSX)=LRTST
 I $D(ORCARY),'$D(^XUTL("OR",$J,"COM")) M ^XUTL("OR",$J,"COM")=ORCARY
 I '$D(ORCARY),$D(^XUTL("OR",$J,"COM")) M ORCARY=^XUTL("OR",$J,"COM")
 Q:LRSTS=""  S LREXP="",LRZX(6)=LROST\1 D MAX^LRXO1 I LREND S ORSTS="K" D ST^ORX W "  Deleted" K ^XUTL("OR",$J,"LROT",LROST,LRZX(1),LRSAMP,LRSPEC,LRSX) Q
 I $D(^LAB(60,LRTST,3,+$O(^LAB(60,LRTST,3,"B",+LRSAMP,0)),0)) S LREXP=$P(^(0),"^",6) I LREXP D RCOM^LRXO9
 I 'LREXP S LREXP=$P(^LAB(60,LRTST,0),"^",19) I LREXP D RCOM^LRXO9
 ;I $D(^LAB(60,LRTST,6,0)),$O(^(0))'<1 W !,"GENERAL WARD INSTRUCTIONS:" S N1=0 F  S N1=$O(^LAB(60,LRTST,6,N1)) Q:N1<1  W !,"  "_^(N1,0)
 N LRCSX S LRCSX=0,CNT=2,I=0,LRLWC=LRZX(1)
 F  S LRCSX=$O(^XUTL("OR",$J,"COM",LROST,LRZX(1),LRSAMP,LRSPEC,LRCSX)) Q:LRCSX<1  S I=0 D
 . F  S I=$O(^XUTL("OR",$J,"COM",LROST,LRZX(1),LRSAMP,LRSPEC,LRCSX,I)) Q:I<1  I ^(I)'["~For",$O(^(I,0))=LRTST S ORETURN("ORTX",CNT)=^XUTL("OR",$J,"COM",LROST,LRZX(1),LRSAMP,LRSPEC,LRCSX,I),CNT=CNT+1
 D SET2^LROR(+LRTEST(LRI),LRSAMP,LRSPEC,LRURG,LRLWC,LRORD) S ORETURN("ORTX",1)=ORTX(1) K LRLWC
 I $D(ORETURN) S ORIFN=LRORIFN D RETURN^ORX
 ;K ^XUTL("OR",$J,"COM",LROST,LRZX(1),LRSAMP,LRSPEC,LRSX)
 Q
COL S Y=LROST D DD^%DT W !!,"The collection time of "_Y_" has expired",!,"Please enter a new collection time"
 D INIT^LRXO00 I LRZX(1)["I" D  Q
 . D EN^LRORDIM I $G(LREND) W !,"No Collection Time Entered",!! Q
 . S ORETURN("ORSTRT")=LROST D RETURN^ORX Q
 N LRNOW D EN^LRXO5 I $G(LREND) W !,"A new collection time must be entered to release this order",!,"Order not released!" Q
 S ORETURN("ORSTRT")=LROST D RETURN^ORX
 Q
END K LRORIFN,LREND,LRTST,LROST,LRSAMP,LRSPEC,LRZX(1),LRURG,LRORD,LROT,LRZX(6),X,LRI,LRTEST
 Q
DUP ;Check for duplicate in ^XUTL
 N SX
 S SX=0 F  S SX=$O(^XUTL("OR",$J,"LROT",LROST,LRZX(1),LRSAMP,LRSPEC,SX)) Q:SX<1  I LRTST=+(^(SX)) S LREND=1 D:LRORIFN'=+$G(^(SX,0))  Q
 . W !!!,"Duplicate test "_LRTSTNM_", for the same collection time will be deleted",!! D READ^ORUTL
 . S ORIFN=LRORIFN,ORSTS="K" D ST^ORX
 Q
GET ;Get Ord #
 S LRORD=$G(^XUTL("OR",$J,"LROT",LROST,LRZX(1)))
 I 'LRORD S ZTQUEUED=1 D ORDER^LROW2 K ZTQUEUED
 Q

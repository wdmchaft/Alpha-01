XTLKKWL1 ; IHS/ISC STAFF/JC-PART 2 OF LOOKUP CONTROL PROGRAM FOR "AND"ING INVERTED SEARCH ;01/20/95  10:27
 ;;7.3;TOOLKIT;;Apr 25, 1995
NOTSHRT D ^XTLKTOKN
 K XTLKWORD,XTLKDFN,XTLKAWRD,XTLKADFN,XTLKNUSE S XTLKNUSE=0
 D PREPSCH^XTLKKWL2
 I XTLKNWDS=0 W:XTLKSAY=1 "MTLU found no usable words.",! G:'XTLKNUSE NOTSHRTX
 I XTLKNUSE>0 W:XTLKSAY=1 !,"The following word",$S(XTLKNUSE=1:" was",1:"s were")," not used in this search:",! S XTLKWD="" F XTLKQ=0:0 S XTLKWD=$O(XTLKNUSE(XTLKWD)) Q:XTLKWD=""  W:XTLKSAY=1 ?5,XTLKWD,!
NOTSHRTX K XTLKNUSE
 K ^TMP("XTLKHITS",$J) S ^TMP("XTLKHITS",$J)=0 D:XTLKNWDS ^XTLKKSCH
 I ^TMP("XTLKHITS",$J)=0 S Y=-1 D EXIT Q
VERIFY ;
 S:'XTLKASK Y=-1 N XTLKHELP
 I XTLKSAY'=-1 D:XTLKASK ASKONE:^TMP("XTLKHITS",$J)=1,ASKSEL:^TMP("XTLKHITS",$J)>1
 I $D(DTOUT)!($D(DUOUT))!($D(DIROUT)) S Y=-1 G EXIT
 I +Y>0,$D(^DD(+$P(@(XTLKGBL_"0)"),U,2),0,"ACT")) X ^("ACT")
 I +Y>0,$D(@(XTLKREF1_(+Y)_",0)"))#2 S Y=+Y_U_$P(^(0),U,1)
EXIT ;
 K ^TMP($J,"ADFN"),^TMP($J,"AWRD")
 K XTLKWD,XTLKNWDS,XTLKKWCT,XTLKI,XTLKQ,XTLKHLP
 K XTLKREF,XTLKREF0,XTLKREF1,XTLKREF2,XTLKREF3,XTLKREF4
 K XTLKGBL,XTLKT,XTLKL,XTLKASK,XTLKPC
 S X=XTLKXSAV K XTLKXSAV
 Q
 ;
ASKONE ; ASK IF SINGLE HIT IS ACCEPTABLE
 ;;;G:@(XTLKKSCH("GBL")_"0)")'["O" ASKONEX
 S XTLKH=1,XTLKMULT=0
 I XTLKSAY'=-1 D
 .W !
 .D DSPLY S Y=-1
 .N DIR S DIR("?")="^D XHELP^XTLKKWL1",DIR(0)="Y",DIR("A")=" OK",DIR("B")="Y" D ^DIR K DIR
 I $D(DTOUT)!($D(DUOUT))!($D(DIROUT))!(Y=0) S Y=-1 D ASKONEX Q
 I $D(^TMP("XTLKHITS",$J)) S X=$O(^($J,0)) I +X S Y=^(X) I $D(DIC(0))#2,DIC(0)["Z" S Y(0)=@XTLKREF3
ASKONEX K XTLKH,XTLKMULT,XTLKDESC,XTLKF
 Q
 ;
ASKSEL ; ASK FOR SELECTION AMONG HITS
 Q:XTLKSAY=-1
 W !!,"The following ",^TMP("XTLKHITS",$J)," matches were found:",!!
 S XTLKMULT=1,XTLKGRP=1
DSPGRP S XTLKH2=$S((XTLKGRP+4)'>^TMP("XTLKHITS",$J):XTLKGRP+4,1:^TMP("XTLKHITS",$J))
 F XTLKH=XTLKGRP:1:XTLKH2 D DSPLY
 N DIR
 S DIR("A")="Press <RET> or Select 1-"_XTLKH_": "
 S DIR("?")="^D XHELP^XTLKKWL1"
 S DIR(0)="FAO" D ^DIR K DIR W !
 I X?1"^"1N.N K DUOUT
 I $D(DTOUT)!($D(DUOUT))!($D(DIROUT)) S Y=-1 D EXSEL Q
 I X["?" D XHELP G DSPGRP
 I X="" S XTLKGRP=$S((XTLKGRP+5)'>^TMP("XTLKHITS",$J):XTLKGRP+5,1:0) G:XTLKGRP DSPGRP I 'XTLKGRP S Y=-1 D EXSEL Q
 I X="-" S XTLKGRP=$S(XTLKGRP'=1:XTLKGRP-5,1:^TMP("XTLKHITS",$J)-1\5*5+1) G DSPGRP
 I X?1"^"1N.N S XTLKI=($E(X,2,255)-1)\5*5+1 I XTLKI'>^TMP("XTLKHITS",$J) S XTLKGRP=XTLKI G DSPGRP
 I X?1"^"1N.N S XTLKI=($E(X,2,255)-1)\5*5+1 I XTLKI>^TMP("XTLKHITS",$J) S XTLKHELP=$S('$D(XTLKHLP):"W $C(7) D SELQ",1:XTLKHLP) X XTLKHELP G DSPGRP
 G:'$D(^TMP("XTLKHITS",$J,X)) DSPGRP
 S XTLKH=X,Y=^TMP("XTLKHITS",$J,XTLKH) I $D(DIC(0))#2,DIC(0)["Z" S Y(0)=@XTLKREF3
EXSEL K XTLKH,XTLKMULT,XTLKH2,XTLKGRP,XTLKDESC,XTLKF
 Q
 ;
DSPLY ; DISPLAY CODE AND TEXT
 Q:XTLKSAY=-1
 S (Y,XTLKI)=^TMP("XTLKHITS",$J,XTLKH)
 S XTLKREF0=XTLKREF1_XTLKI_",0)"
 I $D(XTLKKSCH("DSPLY")) D @XTLKKSCH("DSPLY") Q
 W:XTLKMULT $J(XTLKH,4),": "
 W $P(@(XTLKREF1_"XTLKI,0)"),"^",1),!
 S XTLKDESC="<display protocol not provided>"
DSPDESC F XTLKPC=0:0 Q:XTLKDESC=""  D GETFRAG W:XTLKMULT ?7 W XTLKF,!
 W !
 K XTLKC,XTLKF
 Q
GETFRAG I $L(XTLKDESC)<72 S XTLKF=XTLKDESC,XTLKDESC="" Q
 F XTLKC=73:-1:1 Q:$E(XTLKDESC,XTLKC)=" "
 S XTLKF=$E(XTLKDESC,1,XTLKC-1),XTLKDESC=$E(XTLKDESC,XTLKC+1,255)
 Q
XHELP ;
 I X["?" S XTLKHELP=$S('$D(XTLKHLP):"W $C(7) D SELQ",1:XTLKHLP) X XTLKHELP
 Q
SELQ W !,"Answer with a number from 1 to ",XTLKH," or"
 W !,"'^' to quit the MTLU search and attempt a FILEMAN search, or"
 W !,"'^^' to quit and return to your menu, or"
 W !,"'^#' if you want to jump to any of the ",^TMP("XTLKHITS",$J)," allowable choices.",!
 Q

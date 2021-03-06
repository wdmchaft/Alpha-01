TIUPRPN2 ;SLC/MJC - Header/Footer for Progress Notes ;25-JAN-2001 08:50:35
 ;;1.0;TEXT INTEGRATION UTILITIES;**44,45,52,100,222**;Jun 20, 1997
FOOTER(TIUFOOT,TIUMISC,TIUCONT1,TIUHDR,TIUROOT) ; Function returns TIUCONT
 ; Also controls Pagination position, writes footer when appropriate.
 ; Requires array TIUFOOT, vars TIUMISC,TIUCONT1. Optional TIUHDR.
 ; IF TIUHDR=1, HEADER WILL NOT BE PRINTED
 N TIUCONT,TIUFLAG,TIUPFNBR,TIUDA
 S TIUCONT=1
 S TIUFLAG=$P(TIUMISC,U),TIUPFNBR=$P(TIUMISC,U,2),TIUDA=$P(TIUMISC,U,3)
 I $E(IOST)="C" G FOOTX:$Y+3<IOSL S TIUCONT=$$STOP G FOOTW
 G FTR:+$G(TIUHDR)
 G:$Y+7<IOSL FOOTX
FTR I (IOT'="HFS")!(IOSL<250) F  Q:$Y+6'<IOSL  W ! ;moves ftr to pg bottom
 N TIUFNUM,TIULINE,LOC,DIV,TIUDIV,TIUDIVI,TIUPRTDT,TIUPRTNM
 S $P(TIULINE,"-",81)=""
 W ?21,$S(+$G(TIUCONT1):"** THIS NOTE CONTINUED ON NEXT PAGE **",1:""),!
 I '+$G(TIUFLAG) W "WORK COPY ========== UNOFFICIAL "
 I  W "- NOT FOR MEDICAL RECORD =========== DO NOT FILE"
 E  W TIULINE
 W !,TIUFOOT("PNMP")
 S LOC=$S($L(TIUROOT):$G(@TIUROOT@(1205,"I")),1:"") ; **100**
 I LOC D
 . S DIV=+$P($G(^SC(LOC,0)),U,15)
 . S TIUDIV=+$O(^TIU(8925.94,"B",DIV,0))
 I $G(TIUDIV),$P($G(^TIU(8925.94,TIUDIV,0)),U,2)]"" S TIUDIVI=$P(^(0),U,2)
 S TIUPRTNM=$S($G(TIUDIVI)]"":TIUDIVI,$G(TIUFOOT("INTNM"))]"":TIUFOOT("INTNM"),1:TIUFOOT("SITE"))
 S TIUPRTDT="Printed:"_$$DATE^TIULS($$NOW^TIULC,"MM/DD/CCYY HR:MIN")
 I $L(TIUPRTNM)<36 D
 . W ?(80-$L(TIUPRTNM)\2),TIUPRTNM,?56,TIUPRTDT
 ELSE  D
 . W ?58,TIUPRTDT,!?(80-$L(TIUPRTNM)\2),TIUPRTNM
 I +$G(TIUFLAG) W !,TIUFOOT("SSN")," ",TIUFOOT("DOB"),?(80-$L(TIUFOOT("LOCP"))\2),TIUFOOT("LOCP") ; *222 only print if NOT WORKCOPY
 I '+$G(TIUFLAG) W !,?(80-$L(TIUFOOT("LOCP"))\2),TIUFOOT("LOCP")
 I +$G(TIUFLAG) W ?(80-$L(TIUPFNBR)),TIUPFNBR
 I '+$G(TIUFLAG) W ?(80-$L(TIUFOOT("PH#"))),TIUFOOT("PH#")
 I +$G(TIUFLAG) W !,TIULINE
 E  W !,"=========================== CONFIDENTIAL INFORMATION ==========================="
 W @IOF
FOOTW I '+$G(TIUHDR),+$G(TIUCONT) D HEADER(.TIUFOOT,TIUFLAG,.TIUPFHDR,TIUCONT1,$G(TIUROOT))
FOOTX ;
 Q TIUCONT
HEADER(TIUFOOT,TIUFLAG,TIUPFHDR,TIUCONT1,TIUROOT) ; Header
 ; Requires array TIUFOOT, vars TIUFLAG,TIUPFHDR,TIUCONT1
 N TIULINE S $P(TIULINE,"-",81)=""
 I $E(IOST)="C" D
 .W @IOF,$C(13),TIULINE,!,TIUFOOT("PNMP")_"  "_TIUFOOT("SSN")
 .W ?(78-$L(TIUPFHDR)),TIUPFHDR,!,TIULINE,!
 .I +$G(TIUCONT1) W $$DATE^TIULS(@TIUROOT@(1301,"I"),"MM/DD/CCYY HR:MIN"),?21,"** CONTINUED FROM PREVIOUS SCREEN **",!
 E  D
 .W !,TIULINE,!,$S('+$G(TIUFLAG):"** WORK COPY - NOT FOR ",1:"")
 .W "MEDICAL RECORD" W:'+$G(TIUFLAG) " **" W ?(80-$L(TIUPFHDR)),TIUPFHDR
 .W !,TIULINE,!
 .I +$G(TIUCONT1) W $$DATE^TIULS(@TIUROOT@(1301,"I"),"MM/DD/CCYY HR:MIN"),?21,"** CONTINUED FROM PREVIOUS PAGE **",!!
 Q
STOP() ;on screen paging check
 ; quits TIUCONT=1 if cont. ELSE quits TIUCONT=0
 N DIR,Y,TIUCONT
 S DIR(0)="E" W:+$G(TIUKID) ! D ^DIR
 S TIUCONT=Y
 Q TIUCONT

LRARCR3B ;DALISC/CKA - ARCHIVEDWKLD REP GENERATOR-PRINT 2 ;
 ;;5.2;LAB SERVICE;**59**;Aug 31, 1995
EN ;CALLED FROM LRARCR3A
STAT ;
 Q:'$D(^TMP("LRAR",$J,"TST/URG"))
 D:(LRIOPAT["A")!($L(LRIOPAT)>1) STAT1
 D:'LREND STAT2
 Q
STAT1 ; Combined patient type totals
 S LRPTYP="A"
 S LRSUBH1="TOTAL TESTS by 'STAT' URGENCY for  ALL PATIENTS: % of GRAND TOTAL"_" ( "_LRSUM_" )"
 I $Y+9>IOSL D PAUSE^LRARCR4 Q:LREND  W @IOF D HDR1^LRARCR4
 W !!!?((80-$L(LRSUBH1))/2),LRSUBH1
 W !?((80-$L(LRSUBH1))/2),$E(LRDSH,1,$L(LRSUBH1))
 I '$D(^TMP("LRAR",$J,"TST/URG",LRPTYP)) W !!,?30,"NONE FOUND" Q
 S LRURG=""
 F  S LRURG=$O(^TMP("LRAR",$J,"TST/URG",LRPTYP,LRURG)) Q:(LRURG="")!(LREND)  S LRURGCNT=^(LRURG) D
 . I $Y+6>IOSL D UP1^LRARCR4 Q:LREND
 . W !!,LRURG,"   =",$J(LRURGCNT,5),"    "
 . W $J($FN($S(LRSUM:LRURGCNT/LRSUM,1:0)*100,"",2),5)_"%"
 . S LRTEST=""
 . F I=0:1 S LRTEST=$O(^TMP("LRAR",$J,"TST/URG",LRPTYP,LRURG,LRTEST)) Q:(LRTEST="")!(LREND)  D
 . . S X=I#2 W:'X !
 . . W ?X*40+1,$E(LRTEST_"      ",1,8)," = "
 . . W $J(^TMP("LRAR",$J,"TST/URG",LRPTYP,LRURG,LRTEST),5),"    "
 . . W $J($FN($S(LRURGCNT:^(LRTEST)/LRURGCNT,1:0)*100,"",2),5)_"%"
 . . I X,$Y+6>IOSL D UP1^LRARCR4 Q:LREND
 Q
STAT2 ; Individual patient type totals
 F LRPTYP="I","O","R" Q:LREND  D
 . S LRSUBH1="TOTAL TESTS by 'STAT' URGENCY for "_$S(LRPTYP="I":"INPATIENTS",LRPTYP="O":"OUTPATIENTS",LRPTYP="R":"OTHER PATIENTS",1:"UNKNOWN PATIENTS")_": % of GRAND TOTAL"_" ( "_LRSUM_" )"
 . I $Y+9>IOSL D PAUSE^LRARCR4 Q:LREND  W @IOF D HDR1^LRARCR4
 . W !!!?((80-$L(LRSUBH1))/2),LRSUBH1
 . W !?((80-$L(LRSUBH1))/2),$E(LRDSH,1,$L(LRSUBH1))
 . I '$D(^TMP("LRAR",$J,"TST/URG",LRPTYP)) W !!,?30,"NONE FOUND" Q
 . S LRURG=""
 . F  S LRURG=$O(^TMP("LRAR",$J,"TST/URG",LRPTYP,LRURG)) Q:(LRURG="")!(LREND)  S LRURGCNT=^(LRURG) D
 . . I $Y+6>IOSL D UP1^LRARCR4 Q:LREND
 . . W !!,LRURG,"   =",$J(LRURGCNT,5),"    "
 . . W $J($FN($S(LRSUM:LRURGCNT/LRSUM,1:0)*100,"",2),5)_"%"
 . . S LRTEST=""
 . . F I=0:1 S LRTEST=$O(^TMP("LRAR",$J,"TST/URG",LRPTYP,LRURG,LRTEST)) Q:(LRTEST="")!(LREND)  D
 . . . S X=I#2 W:'X !
 . . . W ?X*40+1,$E(LRTEST_"      ",1,8)," = "
 . . . W $J(^TMP("LRAR",$J,"TST/URG",LRPTYP,LRURG,LRTEST),5),"    "
 . . . W $J($FN($S(LRURGCNT:^(LRTEST)/LRURGCNT,1:0)*100,"",2),5)_"%"
 . . . I X,$Y+6>IOSL D UP1^LRARCR4 Q:LREND
 Q

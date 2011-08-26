SRSPUT1 ;B'HAM ISC/MAM - SPECIALTY UTILIZATION (CONT) ; 22 MAY 1990  9:45 PM
 ;;3.0; Surgery ;;24 Jun 93
 S Y=SRSD D D^DIQ S SRSDT=Y,Y=SRED D D^DIQ S SREDT=Y
 S X=$P(SRSDT," ") D MON S SRSDT=X_" "_$P(SRSDT," ",2,5)
 S X=$P(SREDT," ") D MON S SREDT=X_" "_$P(SREDT," ",2,5)
 I SRFLG D HDR Q:SRSOUT  D PRINT Q
 D HDR Q:SRSOUT  S SRSP=0 F  S SRSP=$O(^TMP("SR",$J,"SP",SRSP)) Q:SRSP=""!SRSOUT  D PRINT
 Q:SRSOUT  D ^SRSPUT2
 Q
PRINT ; print or info
 I $Y+10>IOSL D HDR Q:SRSOUT
 S SRUTL=^TMP("SR",$J,"SP",SRSP),CASES=$P(SRUTL,"^")
 S SROPT=$P(SRUTL,"^",2),SRPOSS=$P(SRUTL,"^",3),SROT=$P(SRUTL,"^",4),SRUTT=SROPT-SROT
 S SRPERC=$S(SRPOSS:SRUTT/SRPOSS*100,1:"-") S:SRPERC SRPERC=SRPERC+.5\1*1,SRPERC=SRPERC_"%"
 S HOUR=SROPT\60,MIN=SROPT#60 S HOUR=$S(HOUR>1:HOUR_" hrs",HOUR=1:HOUR_" hr",1:"") S MIN=$S(MIN>1:MIN_" mins",MIN=1:MIN_" min",1:"")
 D TIMES S SROPTIME=TIME
 S SROT=$P(SRUTL,"^",4),HOUR=SROT\60,MIN=SROT#60 S HOUR=$S(HOUR>1:HOUR_" hrs",HOUR=1:HOUR_" hr",1:"") S MIN=$S(MIN>1:MIN_" mins",MIN=1:MIN_" min",1:"")
 D TIMES S SROT=TIME
 W !!,$P(SRSP,"("),?28,SRPERC,?54,CASES,?72,SROPTIME,?104,SROT,!! F LINE=1:1:132 W "-"
 Q
TIMES ; put hrs and mins in readable format
 I +HOUR,'+MIN S TIME="   "_HOUR Q
 I +HOUR S TIME=HOUR_" and "_MIN Q
 I +MIN S TIME="   "_MIN Q
 S TIME="     -"
 Q
MON S X=$S(X["JAN":"January",X["FEB":"February",X["MAR":"March",X["APR":"April",X["MAY":"May",X["JUN":"June",X["JUL":"July",X["AUG":"August",X["SEP":"September",X["OCT":"October",X["NOV":"November",1:"December")
 Q
HDR ; print heading
 I $D(ZTQUEUED) D ^SROSTOP I SRHALT S SRSOUT=1 Q
 S PAGE=PAGE+1 W:$Y @IOF W !,?(132-$L(SRINST)\2),SRINST,?122,"PAGE "_PAGE,!,?56,"SURGICAL SERVICE",!,?50,"SURGICAL SPECIALTY UTILIZATION REPORT"
 I SRFLG W !,?(132-$L(SRSP)\2),SRSP
 W !,?(132-$L(SRHDR)\2),SRHDR,!! F LINE=1:1:132 W "="
 W !!,"SURGICAL SPECIALTY",?21,"PERCENT UTILIZATION",?48,"NUMBER OF CASES",?71,"TOTAL OPERATION TIME",?100,"TIME WORKED OUTSIDE NORMAL HRS",!! F LINE=1:1:132 W "="
 Q

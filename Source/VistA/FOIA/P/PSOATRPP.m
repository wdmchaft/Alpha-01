PSOATRPP ;BIR/SJA - INTERNET REFILL REPORT SORTED BY PATIENT ;05/29/07 12:36pm
 ;;7.0;OUTPATIENT PHARMACY;**264**;DEC 1997;Build 19
 ;
 K IOP,%ZIS,POP S PSOION=ION,%ZIS="MQ" D ^%ZIS I POP S IOP=PSOION D ^%ZIS K PSOION S PSOQUIT=1 G END
 I $D(IO("Q")) D  K PSOION,ZTSK S PSOQUIT=1 G END
 . N VAR K IO("Q"),ZTIO,ZTSAVE,ZTDTH,ZTSK S ZTRTN="START^PSOATRPP",ZTDESC="INTERNET REFILL REPORT SORTED BY PATIENT"
 . F VAR="PSODS","PSOED","PSOEDX","PSOREP","PSORMZ","PSOSD","PSOSDX","RDATE" S:$D(@VAR) ZTSAVE(VAR)=""
 . S ZTSAVE("PSODIV*")=""
 . D ^%ZTLOAD W:$D(ZTSK) !,"Report is Queued to print !!"
START U IO
 N DFN,DIV,EOFLAG,LINE,PAGE,PNODE,PSA,PSAB,PSDD,PSO,PSOAB,PSOAFLAG,PSODFN,PSOERR,PSON,PSONAM
 N PSOP6,PSOPAT,PSOP5,PSORXDV,PSORXIN,PSOSD1,PSOT,PSOQUIT,X,Y
 K ^TMP($J,"PSOINT") S PAGE=1,PSOQUIT=0,$P(LINE,"-",$S($G(PSORMZ):130,1:79))=""
 S (PSOERR,PSOAFLAG)=0
 S PSOD=0 F  S PSOD=$O(PSODIV(PSOD)) Q:'PSOD  S ^TMP($J,"PSOINT",PSOD)=""
 S (PSA,PSOD)=0 F  S PSOD=$O(PSODIV(PSOD)) Q:'PSOD  D  Q:$G(PSODIV)="ALL"
 .S ^TMP($J,"PSOINT",PSOD)=""
 .S PSOSD1=PSOSD-1 F  S PSOSD1=$O(^PS(52.43,"AD",PSOSD1)) Q:'PSOSD1  I PSOSD1'<PSOSD,PSOSD1'>PSOED D
 ..S PSA=0 F  S PSA=$O(^PS(52.43,"AD",PSOSD1,PSA)) Q:'PSA  S PSAB=$G(^PS(52.43,PSA,0)) D:$P(PSAB,"^",6)>0
 ...S PSORXIN=$P(PSAB,"^",8),PSODFN=$P($G(^PSRX(PSORXIN,0)),"^",2),PSORXDV=$P($G(^PSRX(PSORXIN,2)),"^",9)
 ...I $G(PSODIV)="ALL"!($$DIV^PSOATRP(PSORXIN,PSORXDV)) D SET
 I PSODS="S" D SUMM G END ;print summary report only
 S DIV=0 F  S DIV=$O(^TMP($J,"PSOINT",DIV)) Q:'DIV!(PSOQUIT)  D  D FO W:$E(IOST)="P" @IOF
 .S (PSO("TOT"),PSO(1),PSO(2))=0
 .S PAGE=1 D HD I $D(^TMP($J,"PSOINT",DIV))'=11 W !!,"NO DATA FOUND TO PRINT FOR THIS RANGE.",! D:$E(IOST)="C"  S PSOERR=1
 ..K DIR S DIR(0)="E",DIR("A")="Press Return to continue" D ^DIR K DIR
 .S PSONAM="" F  S PSONAM=$O(^TMP($J,"PSOINT",DIV,PSONAM)) Q:PSONAM=""!(PSOQUIT)  S PSOPAT=0 D  D FO1
 ..S (PSON,PSORXIN)=0 F  S PSORXIN=$O(^TMP($J,"PSOINT",DIV,PSONAM,PSORXIN)) Q:'PSORXIN!(PSOQUIT)  S PSDD=0 F  S PSDD=$O(^TMP($J,"PSOINT",DIV,PSONAM,PSORXIN,PSDD)) Q:'PSDD!(PSOQUIT)  D
 ...S PSOPAT=PSOPAT+1,PSO("TOT")=PSO("TOT")+1,PSON=PSON+1,PSOAB=$G(^TMP($J,"PSOINT",DIV,PSONAM,PSORXIN,PSDD)) D PRT
END D:$E(IOST)="C"&('$G(PSOQUIT))&('$G(PSOERR))  K ^TMP($J,"PSOINT") W:$E(IOST)="P" @IOF D ^%ZISC S:$D(ZTQUEUED) ZTREQ="@" Q
 .W !!,"** END OF REPORT **"
 .W !! K DIR S DIR(0)="E",DIR("A")="Press Return to continue" D ^DIR K DIR
 ;
HD ;PRINT PAGE HEADING
 W:$G(PAGE)'=1!($E(IOST)="C") @IOF W !,"INTERNET REFILL REPORT BY PATIENT - "_$S(PSODS="D":"Detail",1:"Summary")
 W ?45,$TR(RDATE,"@"," ") W ?$S($G(PSORMZ):120,1:68),"PAGE: "_PAGE
 W !,$S(PSODS="D":"Not Filled - ",1:"")_"For date range "_$G(PSOSDX)_" through "_$G(PSOEDX)_" for "_$P(^PS(59,DIV,0),"^")
 I PSODS="S" W !!,"Patient",?35,"Filled",?48,"Not Filled",?63,"Total"
 E  W !!,"Patient",?28,"Rx #",?42,"Date" W:'$G(PSORMZ) ! W ?$S($G(PSORMZ):56,1:20),"Reason"
 W !,LINE S PAGE=PAGE+1
 Q
PRT ;PRINT REPORT
 I PSODS="S" S PSOT(1)=PSOT(1)+PSO(1),PSOT(2)=PSOT(2)+PSO(2)
 S EOFLAG=0 I ($Y+5)>IOSL D  Q:PSOQUIT
 .I $E(IOST)="C" W ! K DIR S DIR(0)="E",DIR("A")="Press Return to continue,'^' to exit" D ^DIR K DIR S:'Y PSOQUIT=1 I 'PSOQUIT S EOFLAG=1 D HD
 .I $E(IOST)'="C" S EOFLAG=1 D HD
 I PSODS="S" W !,$P(PSOAB,"^",2)_" ("_$P(PSOAB,"^",3)_")",?35,PSO(1),?48,PSO(2),?63,(PSO(1)+PSO(2)) Q
 S PNODE=$G(^PS(52.43,$P(PSOAB,"^"),0))
 S Y=$P(PNODE,"^",5),PSOP5=$E(Y,4,5)_"/"_$E(Y,6,7)_"/"_$E(Y,2,3),PSOP6=$P(PNODE,"^",6),PSO(PSOP6)=PSO(PSOP6)+1
 W !,$S(PSON=1:$E($P(PSOAB,"^",2),1,17)_" ("_$P(PSOAB,"^",3)_")",1:""),?28,$P(PNODE,"^",3),?42,PSOP5 W:'$G(PSORMZ) ! W ?$S($G(PSORMZ):56,1:20),$P(PNODE,"^",10)
 Q
FO I PSODS="S",$D(^TMP($J,"PSOINT",DIV))=11 W !!,"COUNT: ",?35,PSOT(1),?48,PSOT(2),?63,(PSOT(1)+PSOT(2)) G T1
 Q:$D(^TMP($J,"PSOINT",DIV))'=11  D:PSODS="D"
 .W !!,"Total transactions for date range "_$G(PSOSDX)_" through "_$G(PSOEDX)_" = "_PSO("TOT")
T1 I $E(IOST)="C" W ! K DIR S DIR(0)="E",DIR("A")="Press Return to continue" D ^DIR K DIR
 Q
FO1 I $D(^TMP($J,"PSOINT",DIV))=11 W !,"     Total transactions for patient = ",PSOPAT
 Q
SET I PSODS="D",($P(PSAB,"^",6)=1) Q
 S DFN=PSODFN D DEM^VADPT
 S ^TMP($J,"PSOINT",PSORXDV,VADM(1),PSORXIN,PSOSD1)=PSA_"^"_VADM(1)_"^"_VA("BID")
 Q
SUMM ;
 S DIV=0 F  S DIV=$O(^TMP($J,"PSOINT",DIV)) Q:'DIV!(PSOQUIT)  S (PSO(1),PSO(2),PSOT(1),PSOT(2))=0 D  D FO W:$E(IOST)="P" @IOF
 .S PAGE=1 D HD I $D(^TMP($J,"PSOINT",DIV))'=11 W !!,"NO DATA FOUND TO PRINT FOR THIS RANGE.",! D:$E(IOST)="C"  S PSOERR=1
 ..K DIR S DIR(0)="E",DIR("A")="Press Return to continue" D ^DIR K DIR W @IOF
 .S PSONAM="" F  S PSONAM=$O(^TMP($J,"PSOINT",DIV,PSONAM)) Q:PSONAM=""!(PSOQUIT)  S (PSO(1),PSO(2))=0 D  D PRT
 ..S PSORXIN=0 F  S PSORXIN=$O(^TMP($J,"PSOINT",DIV,PSONAM,PSORXIN)) Q:'PSORXIN!(PSOQUIT)  S PSDD=0 F  S PSDD=$O(^TMP($J,"PSOINT",DIV,PSONAM,PSORXIN,PSDD)) Q:'PSDD!(PSOQUIT)  D
 ...S PSOAB=$G(^TMP($J,"PSOINT",DIV,PSONAM,PSORXIN,PSDD))
 ...S PNODE=$G(^PS(52.43,$P(PSOAB,"^"),0)),PSOP6=$P(PNODE,"^",6),PSO(PSOP6)=PSO(PSOP6)+1
 Q

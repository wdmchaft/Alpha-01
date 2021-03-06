GMRGRUT1 ;CISC/RM-ROUTINE UTILITIES (cont.) ;2/21/89
 ;;3.0;Text Generator;;Jan 24, 1996
FITLINE ; THIS UTILITY TAKES A LINE OF TEXT IN GMRGPLN AND A LENGTH IN
 ; GMRGLEN, AND SETS GMRGPLN(0)=THE FIRST N-WORDS OF GMRGPLN THAT
 ; WILL FIT IN LENGTH GMRGLEN, AND GMRGPLN(1)=REST OF GMRGPLN
 ; KILLS GMRGLEN AND RETURNS GMRGPLN,GMRGPLN(0),GMRGPLN(1).
 S GMRG100=0,GMRGPLN(0)="",GMRGPLN(1)=GMRGPLN
 I $L(GMRGPLN)>GMRGLEN F GMRG101=0:0 S GMRG102=$F(GMRGPLN," ",GMRG100) Q:GMRG102'>0!(GMRG102>GMRGLEN)  S GMRG100=GMRG102
 S GMRGPLN(0)=$E(GMRGPLN,1,$S(GMRG100>0:GMRG100-1,1:GMRGLEN)),GMRGPLN(1)=$E(GMRGPLN,$S(GMRG100>0:GMRG100,1:GMRGLEN+1),$L(GMRGPLN))
 I GMRGPLN(0)?1." ",$L(GMRGPLN)>GMRGLEN S GMRGPLN(0)=$E(GMRGPLN,1,GMRGLEN),GMRGPLN(1)=$E(GMRGPLN,GMRGLEN+1,$L(GMRGPLN))
 K GMRG100,GMRG101,GMRG102,GMRGLEN
 Q
STORETXT ; THIS UTILITY TAKES THE FOLLOWING ELEMENTS OF THE GMRGNAR ARRAY
 ;    GMRGNAR=AGGY IEN^O/P FORM.^SPACES LT MARG.^CHILD?^SPACES RT. MARG.
 ;    GMRGNAR(0) = TEXT OF AGGY TERM ^ PATIENT TEXT (IF EXISTS)
 ;    GMRGNAR("LEAD") = LEAD TEXT FOR AGGY TERM
 ;    GMRGNAR("TRAIL") = TRAIL TEXT FOR AGGY TERM
 ; AND STORES THE APPROPRIATE INFORMATION IN THE APPROPRIATE FORMAT
 ; INTO THE UTILITY($J,"GMRGNAR",AGGY CLASS,AGGY IEN,LINE) GLOBAL
 Q:'$D(GMRGNAR)  Q:'$D(^TMP($J,"GMRGNAR",GMRGCLAS,$P(GMRGNAR,"^"),0))  S GMRG01=$P(^(0),"^",2),GMRG14=$P(GMRGNAR,"^",3),GMRG15="" F GMRG02=1:1:GMRG14 S GMRG15=GMRG15_" "
 S GMRGXPRT(0)=$P(GMRGNAR(0),"^",2),GMRGXPRT=$P(GMRGNAR(0),"^"),GMRGXPRT(1)="^^^^1^1" D EN1^GMRGRUT2 S GMRG11=GMRGXPRT K GMRGXPRT
 S GMRG17=$P(GMRGNAR,"^",5),GMRG16=$S((IOM-GMRG14)>0:IOM-GMRG14,1:IOM),GMRG10=$P(GMRGNAR,"^",4),GMRG18=0
 I GMRG10 S GMRG08=" "_$S(GMRG10=1:"",GMRG10=2:"and ",1:""),GMRG04=$S(GMRG10=1:",",1:"."),GMRG05=$S(GMRG01:GMRG01,1:1),GMRG06=$L(^TMP($J,"GMRGNAR",GMRGCLAS,$P(GMRGNAR,"^"),GMRG01)),GMRGPLN=$E(^(GMRG01),GMRGSPC,GMRG06)
 I  S GMRG18=$S($E(GMRGPLN)'="-":0,1:1),GMRGPLN=$E(GMRGPLN,$S(GMRG18!($E(GMRGPLN)=" "):2,1:1),$L(GMRGPLN)) D NOB^GMRGRUT4 G QS
 S:$P(GMRGNAR,"^",2)="B" (GMRGNAR("LEAD"),GMRGNAR("TRAIL"))="" S GMRG18=1
 S (GMRG08,GMRGPLN)="" I 'GMRGSSW,($P(GMRGNAR,"^",2)="B"!($P(GMRGNAR,"^",2)="T")) S GMRG05=GMRG01+1
 E  S:GMRGSSW GMRG08="  ",GMRGPLN=$S(GMRGSSW:$E(^TMP($J,"GMRGNAR",GMRGCLAS,$P(GMRGNAR,"^"),GMRG01),GMRGSPC+1,$L(^TMP($J,"GMRGNAR",GMRGCLAS,$P(GMRGNAR,"^"),GMRG01))),1:""),GMRG18=0 S GMRG05=$S('GMRGSSW:GMRG01+1,GMRG01:GMRG01,1:1)
 S GMRG04="" D NOB^GMRGRUT4
QS K GMRG01,GMRG02,GMRG03,GMRG04,GMRG05,GMRG06,GMRG08,GMRG10,GMRG11,GMRG12,GMRG13,GMRG14,GMRG15,GMRG16,GMRG17,GMRG18,GMRGNAR,GMRGPLN
 Q

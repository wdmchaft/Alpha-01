GMRGRUT4 ;CISC/RM-GMRG ROUTINE UTILITIES ;11/2/89
 ;;3.0;Text Generator;;Jan 24, 1996
EN1 ; CALCULATE HOW DID I GET HERE.  THIS UTILITY NEEDS THE GMRGLVL ARRAY
 ; AND RETURNS THE GMRGHOW ARRAY STARTING WITH THE FIRST TERM TRAVERSED
 ; IN GMRGLVL UP TO THE LAST TERM IN GMRGLVL.
 S GMRG0(0)=+GMRGLVL,GMRG0(1)=+GMRGLVL(GMRG0(0)),GMRG0(2)=+GMRGLVL(+GMRG0(0),GMRG0(1))
 F GMRG1=-1:-1 D HOW1 Q:'GMRG0(0)
 S GMRG1=$O(GMRGHOW("")),GMRG0=1-GMRG1,GMRG1="" F GMRG2=0:0 S GMRG1=$O(GMRGHOW(GMRG1)) Q:GMRG1>0  S GMRGHOW=GMRG1+GMRG0,GMRGHOW(GMRGHOW)=GMRGHOW(GMRG1) K GMRGHOW(GMRG1)
 Q
HOW1 ;
 S GMRG2=$S($D(^TMP($J,"GMRGLVL",GMRG0(0),GMRG0(1),GMRG0(2))):^(GMRG0(2)),1:"")
 I $P(GMRG2,"^",2)="J",$P(GMRG2,"^",3)#2 S GMRG1=GMRG1+1 G HJ
 S GMRG2(0)=$S($D(^TMP($J,"GMRGLVL",GMRG0(0),GMRG0(1),GMRG0(2),0)):^(0),1:""),GMRGHOW(GMRG1)=$P(GMRG2,"^")_"^"_$P(GMRG2(0),"^")_"^"_$P(GMRG2(0),"^",3)
HJ I GMRG0(2)=1,GMRG0(1)=1 S GMRG0(0)=GMRG0(0)-1 Q:'GMRG0(0)  S GMRG0(1)=+GMRGLVL(GMRG0(0)),GMRG0(2)=+GMRGLVL(GMRG0(0),GMRG0(1)) Q
 I GMRG0(2)=1 S GMRG0(1)=GMRG0(1)-1,GMRG0(2)=+GMRGLVL(GMRG0(0),GMRG0(1)) Q
 S GMRG0(2)=GMRG0(2)-1
 Q
EN2 ; TAKES A PATH OF TERMS TRAVERSED IN GMRGHOW ARRAY AND DISPLAYS THEM
 S GMRG0=0,GMRG0(0)=$S($D(IOM):IOM,1:80)
 W @IOF F GMRG1=0:0 S GMRG1=$O(GMRGHOW(GMRG1)) Q:GMRG1'>0  D PRT Q:X="^"
 D EOP
 Q
PRT ;
 S X="" I $Y>(IOSL-6) D EOP Q:X="^"  W @IOF
 I GMRG1'=1 W !?GMRG0,"|",!?GMRG0,"V"
 W ! S GMRGXPRT=$P(GMRGHOW(GMRG1),"^",2),GMRGXPRT(0)=$P(GMRGHOW(GMRG1),"^",3),GMRGXPRT(1)=GMRG0_"^"_GMRG0(0)_"^1^0^0^" D EN1^GMRGRUT2
 I GMRG0'>(GMRG0(0)-20) S GMRG0=GMRG0+2
 Q
EOP ;
 W !!,"Press return to continue or ^ to stop listing " R X:DTIME S:'$T X="^^" S:X="^^" GMRGOUT=1 S:X="^^" X="^"
 Q
NOB ;
 S GMRG11=$S(GMRGNAR("LEAD")=""!'$L(GMRG11):"",1:" ")_$S('GMRG10&(GMRGNAR("LEAD")="")&($E(GMRG11)?1L):$C($A($E(GMRG11))-32),1:$E(GMRG11))_$E(GMRG11,2,$L(GMRG11))_$S(GMRGNAR("TRAIL")=""!'$L(GMRG11)!(GMRGNAR("TRAIL")?1P.E):"",1:" ")
 S:$E(GMRGNAR("LEAD"))?1L&'GMRG10 GMRGNAR("LEAD")=$C($A($E(GMRGNAR("LEAD")))-32)_$E(GMRGNAR("LEAD"),2,$L(GMRGNAR("LEAD")))
 F GMRG02=GMRG05:1 D NOB1 Q:GMRGPLN(0)=""  D NOB2
 Q
NOB1 ;
 S GMRG03=$L(GMRGPLN)
 I $L(GMRGPLN)<245 S GMRGPLN=GMRGPLN_$E(GMRG08,1,245-GMRG03),GMRG08=$E(GMRG08,246-GMRG03,$L(GMRG08)),GMRG03=$L(GMRGPLN)
 I $L(GMRGPLN)<245 S GMRGPLN=GMRGPLN_$E(GMRGNAR("LEAD"),1,245-GMRG03),GMRGNAR("LEAD")=$E(GMRGNAR("LEAD"),246-GMRG03,$L(GMRGNAR("LEAD"))),GMRG03=$L(GMRGPLN)
 I $L(GMRGPLN)<245 S GMRGPLN=GMRGPLN_$E(GMRG11,1,245-GMRG03),GMRG11=$E(GMRG11,246-GMRG03,$L(GMRG11)),GMRG03=$L(GMRGPLN)
 I $L(GMRGPLN)<245 S GMRGPLN=GMRGPLN_$E(GMRGNAR("TRAIL"),1,245-GMRG03),GMRGNAR("TRAIL")=$E(GMRGNAR("TRAIL"),246-GMRG03,$L(GMRGNAR("TRAIL"))),GMRG03=$L(GMRGPLN)
 I $L(GMRGPLN)<245 S GMRGPLN=GMRGPLN_$E(GMRG04,1,245-GMRG03),GMRG04=$E(GMRG04,246-GMRG03,$L(GMRG04))
 S GMRGLEN=GMRG16-GMRG17 D FITLINE^GMRGRUT1 S GMRGPLN=GMRGPLN(1)
 Q
NOB2 ;
 S:GMRG02'=GMRG01 $P(^TMP($J,"GMRGNAR",GMRGCLAS,$P(GMRGNAR,"^"),0),"^",2)=$P(^TMP($J,"GMRGNAR",GMRGCLAS,$P(GMRGNAR,"^"),0),"^",2)+1
 S ^TMP($J,"GMRGNAR",GMRGCLAS,$P(GMRGNAR,"^"),GMRG02)=$S(GMRG18&$L(GMRG15):$E(GMRG15,1,$L(GMRG15)-1)_"-",1:GMRG15)_$S($E(GMRGPLN(0))?1L&GMRG18:$C($A($E(GMRGPLN(0)))-32),1:$E(GMRGPLN(0)))_$E(GMRGPLN(0),2,$L(GMRGPLN(0)))
 S GMRG18=0
 Q

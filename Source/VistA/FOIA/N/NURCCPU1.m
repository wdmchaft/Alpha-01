NURCCPU1 ;HIRMFO/RM/MD-NURSING CARE PLAN UTILITIES (cont.) ;8/16/95
 ;;4.0;NURSING SERVICE;;Apr 25, 1997
EN1 ; ENTRY FOR ACTION TO ENSURE THAT NURSCPE VARIABLE SET IN NCP.
 I '$D(NURSPROB) S NURSPROB=0
 Q:$D(NURSCPE)  S NURSCPE=$O(^NURSC(216.8,"B",GMRGPDA,0)) D:NURSCPE'>0 NEWPL^NURCPP0 I NURSCPE'>0 S GMRGOUT=1
 Q
EN2 ; ENTRY TO PRINT OUTCOME INFORMATION IF PRESENT
 Q:'$P(GMRGSEL,"^",3)
 S NURSTRDT=$O(^NURSC(216.8,NURSCPE,"TARG","AA",$P(GMRGSEL,"^"),0)) G:NURSTRDT'>0 Q2 S NURSTRDA=$O(^(NURSTRDT,0)) G:NURSTRDA'>0 Q2 S NURSTRND=$S($D(^NURSC(216.8,NURSCPE,"TARG",NURSTRDA,0)):^(0),1:"")
 S Y=$P(NURSTRND,"^",5) S NURSTRDT=$S(Y:$E(Y,4,5)_"/"_$E(Y,6,7)_"/"_$E(Y,2,3),1:""),NURSTRMT=$P(NURSTRND,"^",2)
 S GMRGHPRT(1)="68^"_NURSTRDT_$S(NURSTRMT=1:"(M)",NURSTRMT=2:"(DC)",1:"(T)")
Q2 K NURSTRDT,NURSTRDA,NURSTRND,NURSTRMT
 Q
EN3 ; ENTRY TO PRINT EVALUATION INFORMATION IF PRESENT
 Q:'$P(GMRGSEL,"^",3)
 S NURSEVDT=$O(^NURSC(216.8,NURSCPE,"EVAL","AA",$P(GMRGSEL,"^"),0)) G:NURSEVDT'>0 Q3 S NURSEVDA=$O(^(NURSEVDT,0)) G:NURSEVDA'>0 Q3 S NURSEVND=$S($D(^NURSC(216.8,NURSCPE,"EVAL",NURSEVDA,0)):^(0),1:"")
 S Y=$P(NURSEVND,"^") S NURSEVDT=$S(Y:$E(Y,4,5)_"/"_$E(Y,6,7)_"/"_$E(Y,2,3),1:"") S Y=$P(NURSEVND,"^",5) S NURSEVRD=$S(Y:$E(Y,4,5)_"/"_$E(Y,6,7)_"/"_$E(Y,2,3),1:"")
 S GMRGHPRT(1)="67^"_$S(NURSEVRD'="":NURSEVRD,NURSEVDT'="":NURSEVDT,1:"        ")_" ("_$P("E^R^S^U","^",$P(NURSEVND,"^",4)+1)_")"
Q3 K NURSEVDT,NURSEVDA,NURSEVND,NURSEVRD,NURSEVUS,NURSEVST
 Q
EN4 ; ENTRY TO PRINT THE PROBLEM AT THE TOP OF THE SCREEN FOR ALL 
 ; CHILDREN THAT ARE UNDER A PROBLEM.
 Q:'$D(NURSPROB)  Q:'$D(NURSPROB(NURSPROB))  Q:'$P(NURSPROB(NURSPROB),"^")
 S GMRGXPRT=$P(NURSPROB(NURSPROB),"^",2),GMRGXPRT(0)=$S($P(NURSPROB(NURSPROB),"^",3)="":"",$D(^GMR(124.3,GMRGPDA,1,$P(NURSPROB(NURSPROB),"^",3),0)):$P(^(0),"^",2),1:""),GMRGXPRT(1)="^^1^^1" D EN1^GMRGRUT2
 S GMRGPLN=GMRGXPRT F NURSH=1:1 S GMRGLEN=IOM-17 D FITLINE^GMRGRUT1 Q:GMRGPLN(0)=""  S GMRGHPRT(NURSH)=$S(NURSH=1:"PATIENT PROBLEM: ",1:"                 "),GMRGHPRT(NURSH)=GMRGHPRT(NURSH)_GMRGPLN(0),GMRGPLN=GMRGPLN(1)
 K GMRGXPRT,NURSH Q
EN5 ; SCREEN FROM THE ORDERABLE (#.02) SUBFIELD OF THE ORDERS INFO (#4)
 ; FIELD OF THE NURS CARE PLAN (#216.8) FILE)
 S NURS("OK")=0,NURS("IN")=$O(^GMRD(124.25,"B","ORDERABLE",0)),NURS("P")=$P(^NURSC(216.8,DA(1),0),U) G Q5:NURS("IN")=""!(NURS("P")="")!($P(^GMRD(124.2,+Y,0),U,4)'=NURS("IN")),Q5:'$D(^GMR(124.3,NURS("P"),1,"B",+Y))
 F NURS=0:0 S NURS=$O(^GMRD(124.2,"AKID",+Y,NURS)) Q:NURS'>0!NURS("OK")  F NURS(0)=0:0 S NURS(0)=$O(^GMRD(124.2,"AKID",NURS,NURS(0))) Q:NURS(0)'>0  I $D(^NURSC(216.8,D0,"PROB","B",NURS(0))) S NURS("OK")=1 Q
Q5 I NURS("OK")
 K NURS
 Q

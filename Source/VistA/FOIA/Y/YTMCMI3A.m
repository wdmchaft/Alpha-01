YTMCMI3A ;SLC/DKG-TEST PKG: TEST DRIVER; ;5/30/02  15:01
 ;;5.01;MENTAL HEALTH;**76**;Dec 30, 1994
 ;
 I '$D(J) S J=1,YSRP="",B="",YSBEGIN=DT
 I $P(^YTT(601,YSTEST,0),U,6)]"" S YSCH=$P(^(0),U,6),Y=$P(^(0),U,7) D DD^%DT S YSCD=Y I $D(^YTT(601.3,YSCH,0)) S YSCHN=YSCH,YSCH=$P(^(0),U) D CR
NX ;
 I $D(^YTT(601,YSTEST,"Q",J,0))#2=1 S:$P(^(0),U,2)]"" C=$P(^(0),U,2)
 I $D(^YTT(601,YSTEST,"Q",J,"B")) S K=^("B") S:K'="" B=K
 I '$D(^YTT(601,YSTEST,"Q",J,"I",1,0)) G D1
 W @IOF F K=1:1 Q:'$D(^YTT(601,YSTEST,"Q",J,"I",K))  W:'$D(^YTT(601,YSTEST,"Q",J,"I",5)) ! W:$D(^YTT(601,YSTEST,"Q",J,"I",K,0)) !?3,^(0)
 W !!!?3,"PRESS THE SPACE BAR TO CONTINUE."
I2 ;
 D RD I X'=" " G:X="*" ^YTAR2 W " ? " G I2
D1 ;
 W @IOF F K=1:1 Q:'$D(^YTT(601,YSTEST,"Q",J,"T",K))  W:$D(^(K,0)) !!?3,^(0)
 X:B'="" B
D3 ;
 S YZT=$P($H,",",2)
 D RD
 G HOLD:YZT+1>$P($H,",",2)
 G D4:C[X,BK:X="^",^YTAR2:X="*",WHERE:X="?" W " ? " G D3
D4 ;
 S YSRP=YSRP_X S J=J+1 I $D(^YTT(601,YSTEST,"Q",J)) G NX
 S:$D(YSMCMI2P) YSRP=YSRP_YSMCMI2P_YSMCMI2L D ^YTFILE K YSMCMI2P,YSMCMI2L Q
RD ;
 R *X:900 S:'$T X=42 G:X<32 RD S X=$C(X) Q
BK ;
 G:J=1 D1 S J=J-1,X=$L(YSRP),YSRP=$S(X>1:$E(YSRP,1,X-1),X=1:"",1:$E(^YTD(601.4,YSDFN,1,YSENT,J\200),1,199)) G NX
WHERE ;
 W !,YSTESTN,"  QUESTION # ",J,! X:B]"" B G D3
CR ;
 I YSCH="IPAT"!(YSCH="PSYC") S YSTNM=$P($P(^YTT(601,YSTEST,"P"),U),"---",2),YSTNM=$E(YSTNM,1,$L(YSTNM)-1) G IP:YSCH="IPAT",PS:YSCH="PSYC"
 W @IOF,!!!?3,^YTT(601.3,YSCHN,1,1,0)," ",YSCD," ",^YTT(601.3,YSCHN,1,2,0) S YSTX=2
 F  S YSTX=$O(^YTT(601.3,YSCHN,1,YSTX)) Q:'YSTX  W !?3,^(YSTX,0)
 W !! H 5 K YSCH,YSCHN,YSCD,YSTX Q
IP ;
 W @IOF,!!!?3,^YTT(601.3,YSCHN,1,1,0),!?3,^YTT(601.3,YSCHN,1,2,0),YSTNM,",",!?3,^YTT(601.3,YSCHN,1,3,0)," ",YSCD," ",^YTT(601.3,YSCHN,1,4,0),!?3,^YTT(601.3,YSCHN,1,5,0),! H 5 K YSCH,YSCHN,YSCD,YSTX,YSTNM Q
PS ;
 W @IOF,!!!?3,^YTT(601.3,YSCHN,1,1,0),YSTNM,!?3,^YTT(601.3,YSCHN,1,2,0)," ",YSCD,!?3,^YTT(601.3,YSCHN,1,3,0),"  ",^YTT(601.3,YSCHN,1,4,0) H 5 K YSCH,YSCHN,YSCD,YSTX,YSTNM Q
 ;
INP ;
 W !,"Was the MCMI3 taken by ",YSNM," administered as an ",!,"(I)npatient or (O)utpatient? "
 R Y:DTIME S YSTOUT='$T,YSUOUT=Y["^" G:YSTOUT!YSUOUT H^XUS S Y=$TR($E(Y_1),"io","IO") I "IO"'[Y W !,"Answer I for inpatient or O for outpatient",$C(7) G INP
 S YSMCMI2P=Y
EPIS ;
 W !,"Was the duration of the recent Axis I Episode: ",!,"1. Less than one week",!,"2. One to four weeks",!,"3. One to three months",!,"4. Three to twelve months",!,"5. Periodic; one to three years",!,"6. Continuous; one to "
 W "three years",!,"7. Periodic; three to seven years",!,"8. Continuous; three to seven years",!,"9. More than seven years",!,"0. Cannot categorize"
 R !,"Answer: ",Y:DTIME S YSTOUT='$T,YSUOUT=Y["^" G:YSTOUT!YSUOUT H^XUS S Y=$E(Y_"A") I Y'?1N W !,"Enter a number 0-9" G EPIS
 S YSMCMI2L=Y Q
HOLD ;
 W @IOF,#,$C(7)
 R "Please read each question carefully!",X:3 K X G D1

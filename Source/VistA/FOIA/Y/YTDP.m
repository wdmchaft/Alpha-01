YTDP ;SLC/TGA-PRINT & DUPLICATE REPORTS ;11/27/91  15:02
 ;;5.01;MENTAL HEALTH;**37,41,54**;Dec 30, 1994
 ;
 S YSXT="" S:'$D(T1) T1=0 W !!?10,"--- Previous ",$S('T1:"Instruments",1:T1(0)_"(s)")," ---",! I 'YSNT D DIS W !?10,"No Completed ",$S('T1:"Instruments",1:T1(0)_"(s)")," Found",! H 3 G KAR^YTS
 S B=$S(YSNT<11:YSNT,1:YSNT+1\2)
 F K=1:1:B S YSDT=$P(A1(K),U,2) D DAT W !?10,$J(K,3,0),"  ",$P(A1(K),U) W ?22,YSDT I B'=YSNT,$D(A1(B+K)) W ?50,$J(B+K,3,0),"  ",$P(A1(B+K),U) S YSDT=$P(A1(B+K),U,2) D DAT W ?62,YSDT
 D DIS W !!?10,"--- Print ",$S('T1:"Instruments",1:T1(0)_"(s)")," ---",!
DU ;
 W !?10,"Select ",$S('T1:"Instrument",1:T1(0))," # : " R M:DTIME S YSTOUT='$T,YSUOUT=M["^" G KAR^YTS:YSTOUT!YSUOUT,D1:M=""
 I M'?1.3N!(M<1)!(M>YSNT) W:M'["?" "   [Not Valid # or out of Range]" W !!?10,"Type ONLY the NUMBER of the instrument you wish to print",! G DU
 S YSXT=YSXT_$P(A1(M),U,2)_","_$P(A1(M),U,3)_"^" G:$L(YSXT,U)<22 DU
D1 ;
 I YSXT="" G KAR^YTS
RP ;
 W !!,"Type 'Q' to queue to a printer.",! S %ZIS="Q" D ^%ZIS G:POP KAR^YTS
 I $D(IO("Q")),IO=IO(0),IOST'?1"P".E W !,$C(7),"You must QUEUE to a PRINTER!" G RP
 I $D(IO("Q")) K IO("Q") S ZTRTN="RP1^YTDP",ZTSAVE("YS*")="",ZTDESC="YS DUP REPORT" D ^%ZTLOAD W:$D(ZTSK) !!,"Your Task Number is "_ZTSK G KAR^YTS
RP1 ;
 S:$D(ZTQUEUED) ZTREQ="@"
 S YSXTP=1,P0=$S(IOST?1"P".E:1,1:0),YSLFT=0 U IO
RP10 ;
 S YSTEST=$P(YSXT,U,YSXTP) I YSTEST="" D KILL^%ZTLOAD G KAR^YTS
 I YSTEST[",",$P(^YTT(601,$P(YSTEST,",",2),0),U,9)="B" S YSTESTS=YSTEST F  S YSXTP=$G(YSXTP)+1 S:$D(YSXT) YSTEST=$P(YSXT,U,YSXTP) Q:$G(YSTEST)=""!(YSXTP=1)  D CONB
 I $G(YSTEST)="" S YSTEST=YSTESTS K YSTESTS G:'$D(^YTT(601,$P(YSTEST,",",2),"R")) KAR^YTS
CONB I YSTEST["," S (YSHD,YSDT)=$P(YSTEST,",") D DAT S YSHDR=$E(YSHDR,1,61)_" "_YSDT,YSED=$P(YSTEST,","),(YSET,YSTEST)=$P(YSTEST,",",2)
 I '$D(YSTESTS) G:'$D(^YTT(601,YSTEST,"R")) KAR^YTS
 X:^YTT(601,YSTEST,"R")]"" ^("R")
 G:$D(YSFORM) RP21 I $P(^YTT(601,YSTEST,0),U,6)]"" S YSCH=$P(^(0),U,6),Y=$P(^(0),U,7) D DD^%DT S YSCD=Y I $D(^YTT(601.3,YSCH,0)) S YSCHN=YSCH,YSCH=$P(^(0),U) D CR G:YSLFT RP21
 G:'P0 RP11 I IOSL-$Y<7 D DTA^YTREPT W !!!
 E  F I=1:1:IOSL-$Y-5 W !
 W "Not valid unless signed: Reviewed by ................................",!,"Printed by: ",$P(^VA(200,DUZ,0),U),"   "
 S YSORD=$P(^YTD(601.2,YSDFN,1,YSET,1,YSED,0),U,3)
 W ! I YSORD,$D(^VA(200,YSORD,0)) W "Ordered by: ",$P(^(0),U)
RP11 ;
 I $D(^YTD(601.2,YSDFN,1,YSET,1,YSED,"R",0)),$P(^(0),U,4)>0 D:'P0 SCR1 G:YSUOUT RP21 D ^YTDP1 G:YSLFT RP21
 I $P(^YTT(601,YSTEST,0),U,9)="T" D:'P0 SCR1 G:YSLFT RP21 D:'$D(YSNOITEM) IR^YTREPT D:$D(YSNOITEM) @YSNOITEM
RP21 ;
 K J,R,S,X,YSFORM,YSMX,YSNOITEM ;Q:YSTOUT!YSUOUT  I $D(P0) D:'P0 SCR1
 S:YSTOUT!YSUOUT YSXT="" S YSXTP=YSXTP+1 G RP10
DAT ;
 S YSDT=$$FMTE^XLFDT(YSDT,"5ZD") Q
DIS ;
 Q:'$D(^YTD(601.4,YSDFN,1,"B"))  W !!?10,"Discontinued Instruments Exist",! Q
CR ;
 S P1=$S(P0:"I IOSL-$Y<15",1:"I IOSL-$Y<9")
 X P1 D CK:$T Q:YSLFT  I YSCH="IPAT"!(YSCH="PSYC") S YSTNM=$P($P(^YTT(601,YSTEST,"P"),U),"---",2),YSTNM=$E(YSTNM,1,$L(YSTNM)-1) G IP:YSCH="IPAT",PS:YSCH="PSYC"
 W !!!?3,^YTT(601.3,YSCHN,1,1,0)," ",YSCD," ",^YTT(601.3,YSCHN,1,2,0) S YSTX=2
NL ;
 S YSTX=$O(^YTT(601.3,YSCHN,1,YSTX)) Q:'YSTX  W !?3,^(YSTX,0) G NL
IP ;
 W !!!?3,^YTT(601.3,YSCHN,1,1,0) W !?3,^YTT(601.3,YSCHN,1,2,0),YSTNM,",",!?3,^YTT(601.3,YSCHN,1,3,0)," ",YSCD," ",^YTT(601.3,YSCHN,1,4,0),!?3,^YTT(601.3,YSCHN,1,5,0) K YSCH,YSCHN,YSCD,YSTX,YSTNM Q
PS ;
 W !!!?3,^YTT(601.3,YSCHN,1,1,0),YSTNM W !?3,^YTT(601.3,YSCHN,1,2,0)," ",YSCD,!?3,^YTT(601.3,YSCHN,1,3,0),"  ",^YTT(601.3,YSCHN,1,4,0) K YSCH,YSCHN,YSCD,YSTX,YSTNM Q
SCR1 ;
 ;  Added 5/6/94 LJA
 N A,B,B1,C,D,E,E1,F,F1,G,G1,H,I,J,J1,J2,J3,J4,K,L,L1,L2,M,N
 N N1,N2,N3,N4,P,P0,P1,P3,R,R1,S,S1,T,T1,T2,TT,V,V1,V2,V3
 N V4,V5,V6,W,X,X0,X1,X2,X3,X4,X7,X8,X9,Y,Y1,Y2,Z,Z1,Z3
 ;
 Q:YSLFT  F I0=1:1:(IOSL-$Y-2) W !
 N DTOUT,DUOUT,DIRUT
 S DIR(0)="E" D ^DIR K DIR S YSTOUT=$D(DTOUT),YSUOUT=$D(DUOUT),YSLFT=YSTOUT
 W @IOF Q
CK ;
 D SCR1:'P0 Q:YSUOUT!YSTOUT  D:P0 DTA^YTREPT Q

DIO2 ;SFISC/GFT,TKW-PRINT ;9:17 AM  24 Feb 2000
 ;;22.0;VA FileMan;**32**;Mar 30, 1999
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 S (DISTP,DILCT)=0
XDY I $D(DIBTPGM) D @("EN"_DIBTPGM),ENRLS^DIOZ(+$P(DIBTPGM,"^DISZ",2)) Q
 X DY(DN) G XDY:DN
 Q
 ;
SEARCH S DISEARCH=1 ; Protect switch SO-2/24/2000
SCR S DIO("SCR")=1,DE=0 I '$D(DIS(0)) G OR
 X DIS(0) Q:'$T  G PASS:'$D(DIS(1))
OR S DE=DE+1 I '$D(DIS(DE)) Q
 X DIS(DE) E  G OR
PASS S:'$D(DPQ) DIPASS=1
O F DLP=0:1:DX Q:'DN  X $S($D(DPQ):DX(DLP),1:^UTILITY($J,99,DLP))
 Q
 ;
N W !
T I $X,IOT'="MT" W !
 I '$D(DIOT(2)),DN,$D(IOSL),$S('$D(DIWF):1,$P(DIWF,"B",2):$P(DIWF,"B",2),1:1)+$Y'<IOSL,$D(^UTILITY($J,1))#2,^(1)?1U1P1E.E X ^(1)
 S DISTP=DISTP+1,DILCT=DILCT+1 D:'(DISTP#100) CSTP
 Q
 ;
CSTP I $G(IOT)="SPL"!($G(IOT)="HFS") I '$D(DPQ),$$ROUEXIST^DILIBF("XUPARAM"),DILCT>$$KSP^XUPARAM("SPOOL LINES") D  Q
 . S DIFMSTOP=1,DN=0 S:$D(ZTQUEUED) ZTSTOP=1
 . W !,"*** JOB STOPPED BECAUSE MAXIMUM SPOOL LINES HAS BEEN EXCEEDED ***",!! Q
 I '$D(ZTQUEUED) K DISTOP Q
 Q:$G(DISTOP)=0  S:$G(DISTOP)="" DISTOP=1
 I DISTOP'=1 X DISTOP K:'$T DISTOP S DISTOP=$T Q:'$T
 Q:'$$S^%ZTLOAD
 W:$G(IO)]"" !,"*** TASK "_ZTSK_" STOPPED BY USER - DURING "_$S($D(DPQ):"SORT",1:"PRINT")_" EXECUTION ***",!! S ZTSTOP=1,DN=0 Q
 ;
DT I $G(DDXPDATE) D DT^DDXP4 W DDXPY K DDXPY Q
 I $G(DUZ("LANG"))>1,Y W $$OUT^DIALOGU(Y,"DD") Q
 I Y W $P("JAN^FEB^MAR^APR^MAY^JUN^JUL^AUG^SEP^OCT^NOV^DEC",U,$E(Y,4,5))_" " W:Y#100 $J(Y#100\1,2)_"," W Y\10000+1700 W:Y#1 "  "_$E(Y_0,9,10)_":"_$E(Y_"000",11,12) Q
 W Y Q
 ;
C S DQ(C)=Y
S S Q(C)=Y*Y+Q(C) S:L(C)>Y L(C)=Y S:H(C)<Y H(C)=Y
P S N(C)=N(C)+1
A S S(C)=S(C)+Y Q
D I Y=DITTO(C) S Y="" Q
 S DITTO(C)=Y Q
 ;
CP S C="" F  S C=$O(CP(C)) Q:C=""  G DQ:'$D(DQ(C))
 S CP=CP+1 F  S C=$O(CP(C)),A="" Q:C=""  F  S A=$O(CP(A)) S CP(C,A)=DQ(C)*DQ(A)+CP(C,A) Q:A=C
DQ K DQ Q
 ;
H F DI=DI:1:DN I $D(^UTILITY($J,"H",DI)) X ^UTILITY($J,"H",DI) W:$X&($G(DIAR)'=4)&($G(DIAR)'=6) !
 Q
 ;
M X $S($D(DPQ):DX(DIXX),1:^UTILITY($J,99,DIXX))

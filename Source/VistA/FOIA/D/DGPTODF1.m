DGPTODF1 ;ALB/MTC - PTF DRG FREQUENCY REPORT ; 9/14/01 9:58am
 ;;5.3;Registration;**375**;Aug 13, 1993
 I "DB"[DGS,$D(^UTILITY($J,"DGPTFR","D")) D FD^DGPTODF2
 E  I $D(^UTILITY($J,"DGPTFR","SB")) D FD^DGPTODF2  ;added line DG*5.3*375
 I DGS'="D" S I=0 F I1=0:0 S I=$O(^UTILITY($J,"DGPTFR","FS",I)) Q:I']""  S S=^(I) F D=0:0 S D=$O(^UTILITY($J,"DGPTFR","FS",I,D)) Q:D'>0!(D>600)  S D1=^(D),^UTILITY($J,"DGPTFR","FS",I,(999999-$P(D1,U,2)),D)=D1 K ^UTILITY($J,"DGPTFR","FS",I,D)
START D QUIT,MCT:$D(^UTILITY($J,"DGPTFR","FD")),ST:$D(^UTILITY($J,"DGPTFR","FS")) W @IOF
QUIT K %,B,C,C1,C2,C3,C4,C5,D,D1,D2,D3,DGCPG,DGFLAG,DGTCH,E,F1,F2,F3,F4,F5,G,G1,G2,G3,I,I1,J,M1,M2,M3,M4,M5,P,P1,P3,S,S1,T,T1,T2,T3,X,X2,Y,Z,^UTILITY($J,"DGTC"),^UTILITY($J,"BOK") Q
MCT S P=0,DGFLAG="Medical Center",P3="DRG" D INIT,TINIT,COV^DGPTODF2,HEAD^DGPTODF2 F D=0:0 S D=$O(^UTILITY($J,"DGPTFR","FD",D)) Q:D'>0  S D1=999999-D F D2=0:0 S D2=$O(^UTILITY($J,"DGPTFR","FD",D,D2)) Q:D2'>0  S D3=^(D2),P1=D2 D TDOL,PRINT
 D TOT,PP,TP^DGUTL
 Q
ST S P=0,(DGFLAG,P3)="Service" D TINIT,INIT,COV^DGPTODF2
 S G=0 F G1=0:0 S G=$O(^UTILITY($J,"DGPTFR","FS",G)) Q:G']""  S (G2,P1)=^(G) D TOT:C1,STOT:C1,INIT,HEAD^DGPTODF2 F D=0:0 S D=$O(^UTILITY($J,"DGPTFR","FS",G,D)) Q:D'>0  S D1=999999-D,G3=G2 D ST1
 S G3=G2 D STOT,TOT S DGFLAG="Medical Center" D HEAD^DGPTODF2,LAST S C1=F1,C2=F2,C3=F3,C4=F4,C5=F5,DGFLAG="Medical Center",^UTILITY($J,"DGTC","MEDICAL CENTER",P)="" D TOT,PP,TP^DGUTL
BS S P=0,(DGFLAG,P3)="Specialty" D MI,INIT,TINIT,COV^DGPTODF2 S G=0 F G1=0:0 S G=$O(^UTILITY($J,"DGPTFR","FB",G)) Q:G']""  S G2=^(G) D CON:F1,MT:F1,INIT,TINIT,HEAD^DGPTODF2,BT
 D CON,MT,MCON,PP,TP^DGUTL
 Q
BT F Z=0:0 S Z=$O(^UTILITY($J,"DGPTFR","FB",G,Z)) Q:Z'>0  S (B,P1)=^(Z) D TOT:C1,STOT:C1,INIT W !!!?15,B,!! F D=0:0 S D=$O(^UTILITY($J,"DGPTFR","FB",G,Z,D)) Q:D'>0  S D1=999999-D D BT1
 D STOT,TOT Q
BT1 F D2=0:0 S D2=$O(^UTILITY($J,"DGPTFR","FB",G,Z,D,D2)) Q:D2'>0  S D3=^(D2) D TDOL,PRINT
 Q
ST1 F D2=0:0 S D2=$O(^UTILITY($J,"DGPTFR","FS",G,D,D2)) Q:D2'>0  S D3=^(D2) D TDOL,PRINT
 Q
INIT S (C1,C2,C3,C4,C5,T,T1)=0 Q
TINIT S (F1,F2,F3,F4,F5)=0 Q
MI S (M1,M2,M3,M4,M5)=0 Q
CON S C1=F1,C2=F2,C3=F3,C4=F4,C5=F5,DGFLAG="Service" D TOT S DGFLAG="Specialty" Q
MCON S C1=M1,C2=M2,C3=M3,C4=M4,C5=M5,DGFLAG="Medical Center" D TOT Q
TDOL S T=D1+$J($S($P(D3,U,3):$P(D3,U,11)/$P(D3,U,3),1:0),0,4)*$P(D3,U,6)+($P(D3,U,10)*DG1DAWW)+($P(D3,U,9)*DGHIWW),T=$S(T=0:$P(D3,U,6),1:T),T1=T*DGWWCST S:'$D(^UTILITY($J,"DGTC",P1)) ^(P1,P)="" Q
PRINT D HEAD^DGPTODF2:$Y>(IOSL-11)
 W !?10,$J(D2,3),$J($P(D3,U,3),5),$J($P(D3,U,4),6),$J($P(D3,U,5),6),$J($P(D3,U,6),9),$J($P(D3,U,7),7),$J($P(D3,U,10),9),$J(D1,16),$J(+D3,11),$J(+D3/D1,12,2) S X=T,X2=2 D COMMA^%DTC W $J(X,18)   ;S X=T1,X2=2 D COMMA^%DTC W $J(X,18)
 S C1=C1+D1,C3=+D3+C3,C4=C4+T,C5=C5+T1,C2=C2+$P(D3,U,10)
 Q
TOT W ! F E=1:1:132 W $S(DGFLAG["Serv":"-",1:"=")
TOT1 W !?10,"Total for ",DGFLAG,?46,$J(C2,9),$J(C1,16),$J(C3,11),$J(C3/C1,12,2) S X=C4,X2=2 D COMMA^%DTC W $J(X,18) W $J(C4/C1,18,2)   ;S X=C5,X2="2$" D COMMA^%DTC W $J(X,18)
 Q
STOT S F1=F1+C1,F2=F2+C2,F3=F3+C3,F4=F4+C4,F5=F5+C5,^UTILITY($J,"BOK",G3)=C1_U_C2_U_C3_U_C4_U_C5 Q
MT S M1=M1+F1,M2=M2+F2,M3=M3+F3,M4=M4+F4,M5=M5+F5 Q
LAST S D=0 F D1=0:0 S D=$O(^UTILITY($J,"BOK",D)) Q:D']""  F E=1:1:5 S @("C"_E)=$P(^UTILITY($J,"BOK",D),U,E) S DGFLAG=D D TOT1:E=5
 Q
 Q
PP S %=$S($Y>(IOSL-11):($Y+2),1:IOSL-11) F E=$Y:1:% W ! I E=(%-1) D DIS^DGPTOD1 W !!?62,"-",P,"-"
 Q

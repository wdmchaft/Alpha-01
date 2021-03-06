YSESD ;SLC/DCM-DRIVER TO RUN THE DECISION SUPPORT ALGORIGHM CHOSEN ; 7/14/89  12:44 ;08/11/93 09:38
 ;;5.01;MENTAL HEALTH;;Dec 30, 1994
 ;DECISION ASSISTANT SYSTEM (VERSION 1.0) FOR MENTAL HEALTH PACKAGE - DWIGHT MCDANIEL / REGION 5 ISC, SLC
 ;
 ; Called by routines YSESA, YSESR
E G EN
K S A5AI="" F A5AJ=1:1 S A5AI=$O(A5AVAR(A5AI)) Q:A5AI=""  K @A5AI
 K A5AVAR Q
PUSH S:'$D(A5ASP) A5ASP=0 S A5ASP=A5ASP+1,ESPATH(A5ASP)=A5AND Q
POP Q:A5ASP=0  K ESPATH(A5ASP) S A5ASP=A5ASP-1 Q
CHOOSE I A5AV?1"$"1A.E1"$" S A5AVAR(A5AX)=@A5AX
 F A5AJ=1:1 S A5AT=$P(A5ARES,",",A5AJ) Q:A5AT=""  D TEST I @A5ACND S A5AX=A5AT Q
 K A5AT,A5ACND,A5ANODE,A5AT1 Q
TEST S A5ANODE=$P(A5AT,"::",2),A5AT=$P(A5AT,"::"),A5ACND=""
 F A5AI=2:1 S A5AT1=$P(A5AT,A5AV,A5AI) Q:A5AT1=""  S A5ACND=A5ACND_$S(A5AV?2"$":A5AX,1:@A5AX)_A5AT1
 I A5ACND["$" F A5AI=1:1 S A5AT1=$P(A5ACND,"$",2) Q:A5AT1=""  S A5AT=$L($P(A5ACND,"$"))+$L(A5AT1)+3,A5ACND=$P(A5ACND,"$")_@(A5AT1)_$E(A5ACND,A5AT,150)
 Q
NALG S ESP=A5AND D ^YSESN S A5AND=ESPATH(A5ASP) D POP S GN2=$O(@(ESDBP1_Q_A5AND_Q_",0)")) Q
RSTART S A5AI="" F A5AJ=1:1 S A5AI=$O(A5AVAR(A5AI)) Q:A5AI=""  K @A5AI
 D K K ESPATH S A5ASP=0 G:$D(A5AFL) END S DIC=A5AS(0),ESDBP=A5AS(1),A5ASYS=A5ASYS(0),A5AGN=A5AGN(0) G EN
EN S A5AVARC=0,A5AS(0)=DIC,A5AS(1)=ESDBP,A5AGN(0)=GN,A5ASYS(0)=A5ASYS,ESDBP=ESDBP_GN_",1,",ESDBP1=ESDBP_"""B"",",TB=($L(A5ASYS)\2)+7
 S XCODE="F ESI=1:1 W $S($P(ANS,"","",ESI)]""[0]"":$P(ANS,"","",ESI),1:"""") Q:$P(ANS,"","",ESI+1)=""""  W $S($P(ANS,"","",ESI)]""0"":""/"",1:"""") "
MEN S TB=(80-($L(A5ASYS)+14))\2,TC=(80-($L(A5ASYS)+20))\2 W @IOF,!!,$E(STR,1,TC),?TB,A5ASYS_" MAJOR SYMPTOM",?79-TC,$E(STR,1,TC),!,"**",?77,"**",!,"** "
 S ESI="" F ESJ=0:1 S ESI=$O(@(ESDBP1_Q_ESI_Q_")")) Q:ESI=""  W ESI,?41 W:ESJ#2 ?77,"**",!,"** "
 W ?77,"**",!,STR,!!
F S DIC=ESDBP,DIC(0)="AEMQZ",DIC("A")="Select the Major Symptom Algorithm: " D ^DIC I Y=-1 S (A5AI,A5AJ)="" G END
 S GN1=$P(Y,U),A5AND=@(ESDBP_GN1_",1)"),ESDBP=ESDBP_GN1_",2,",ESDBP1=ESDBP_"""B"",",GN2=$O(@(ESDBP1_Q_A5AND_Q_",0)")),A5ALG=$P(Y,U,2)
 ;
LOOP ; Called by routine YSESA
 S:$D(@(ESDBP_GN2_",2)")) A5ARES=^(2),ANS=$P(A5ARES,U),A5ARES=$P(A5ARES,U,2) I '$D(^(2)) D NRSP^YSESH G RSTART
 Q:A5AND=0  D PUSH S (A5AV,ESORDR)="" W !!!!! S PROMPT=0 F ESI=0:0 S PROMPT=$O(@(ESDBP_GN2_",1,"_PROMPT_")")) Q:PROMPT']""  W @("^("_PROMPT_",0)"),!
 I GN2=-1 D NNODE^YSESH G RSTART
 I ANS?1A.E1"$".E1"$" S A5AV=$E(ANS,$F(ANS,"$")-1,99),ANS=$P(ANS,A5AV)
 I A5ARES="" D NNODE^YSESH G RSTART
 I $S(A5ARES["[0]":1,A5ARES["[D]":1,1:0) W !!?18,$C(7),"<<  CONCLUSION AND FINDINGS  >>",!!?23,"PRESS RETURN TO EXIT" R A5ANS:DTIME G:A5AX=""!(A5ARES["[0]") RSTART G L1
 W !!?10,"Answer with "_$S(A5AV["$":"",1:"(") X XCODE W $S(A5AV["$":"",1:")")_": " R:A5AV="" A5AX:DTIME I A5AV["$" S A5AX=$P(A5AV,"$",2) R:A5AX]"" @A5AX R:A5AX="" A5AX:DTIME I "?"'[A5AX,"^"'[A5AX D CHOOSE
L1 I A5AX["?"!(A5AX="") W !!,"   Type in prompted response or Type '^' to return to",!,"    previous prompt, or type '^^' to leave algorithm.",!!! G LOOP
 I A5AX["^" D POP G:A5ASP=0!(A5AX="^^") RSTART S A5AND=ESPATH(A5ASP),GN2=$O(@(ESDBP1_Q_A5AND_Q_",0)")) D POP D:A5AND["<"&(A5AND[">") NALG G LOOP
 I A5AND[">"&(A5AND[">") D ^YSESN G LOOP
 F ESI=1:1 S ESP=$P(A5ARES,",",ESI) Q:ESP=""  I $P(ESP,"::")[A5AX S ESP=$P(ESP,"::",2) Q
 I ESP="" W $C(7),!!,"******  NOT A VALID LISTED RESPONSE  ******" G LOOP
 I $D(@(ESDBP_GN2_",3)")) X ^(3)
 I $E(ESP)["<"&($E(ESP,$L(ESP))[">") S A5AND="<"_A5ALG_">" D PUSH,^YSESN G:GN1>0 LOOP D NNODE^YSESH G RSTART
 I $E(ESP)="{"&($E(ESP,$L(ESP))="}") S A5AND="{"_A5ASYS_"<"_A5ALG_"["_A5AND_"]>}" D PUSH,^YSESN G:GN1>0 LOOP D NNODE^YSESH G RSTART
 S A5AND=$P($P(ESP,"[",2),"]") I A5AND=""!(A5AND=" ") D NNODE^YSESH G RSTART
 S GN2=$O(@(ESDBP1_Q_A5AND_Q_",0)")) I GN1=""!(A5AND=-1) D NNODE^YSESH G RSTART
 G LOOP
END K T,GN,GN1,ESP,EST,A5ANS,A5AV,A5ASP,A5AND,ESORDR,ESPATH,A5AVARC,A5AGN,A5AS Q
ULC F ESI=1:1 S ESI1=$E(EST,ESI) Q:ESI1=""  I ESI1?1L S ESI1=$C($A(ESI1)-32),EST=$E(EST,1,ESI-1)_ESI1_$E(EST,ESI+1,$L(ANS))
 S ESI="" K ESI1 Q

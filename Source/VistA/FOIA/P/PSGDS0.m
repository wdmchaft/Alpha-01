PSGDS0 ;BIR/CML3-GATHER INFO FOR DISCHARGE SUMMARY ;25 Feb 99 / 9:30 AM
 ;;5.0; INPATIENT MEDICATIONS ;**4,8,24,58**;16 DEC 97
 ;
 ;Reference to ^PS(55 is supported by DBIA 2191
 ;Reference to ^PSDRUG is supported by DBIA 2192
 ;
GOD ; gather order data
 S N=0,ND=$G(^PS(55,PSGP,5,PSJJORD,0)),ND2=$G(^(2)),SI=$P($G(^(6)),"^"),DRG=$G(^(.2)),DO=$P(DRG,"^",2) ; I $D(^PSDRUG(+DRG,8)),$P(^(8),"^",5) S DRG=$P(^(8),"^",5),N=1
 ; S WS=$S(DRG&PSGAPWD:$D(^PSI(58.1,"D",+DRG,PSGAPWD)),1:0),DRG=$G(^PSDRUG(+DRG,0))
 S SD=$P(ND2,"^",2),FD=$P(ND2,"^",4),ND2=$P(ND2,"^"),RTE=+$P(ND,"^",3),ST=$P(ND,"^",9),RTE=$E($$ENMRN^PSGMI(RTE),1,5),SPH="",NF=""
 S ND=$P(ND,"^",7),ND=$$ENSTN^PSGMI(ND) F X="SD","FD" S @X=$E($$ENDTC^PSGMI(@X),1,5)
 S (CNT,DRGN,DDRG)=""
 F JJ=0:0 S JJ=$O(^PS(55,PSGP,5,PSJJORD,1,JJ)) Q:'JJ  D
 .S X=$G(^PS(55,PSGP,5,PSJJORD,1,JJ,0))
 .I $P(X,U,3),($P(X,U,3)<PSGDT) Q
 .S CNT=CNT+1,DDRG=JJ
 .S SPH=SPH_$P($G(^PSDRUG(+X,0)),U,3)
 S DDRG=$S('CNT:"NO",CNT>1:"MULTIPLE",1:$G(^PS(55,PSGP,5,PSJJORD,1,DDRG,0)))
 ;I $P(DDRG,U,2) S DO=$P(DDRG,U,2)
 ;S DRGN=$S(DRGN:$$ENPDN^PSGMI(+DRG),$P($G(^PSDRUG(+DDRG,8)),"^",5):$P(^PSDRUG($P(^(8),"^",5),0),"^"),1:$P(^PSDRUG(+DDRG,0),"^"))
 S DRGN=$S('DDRG:$$ENPDN^PSGMI(+DRG),$P($G(^PSDRUG(+DDRG,8)),"^",5):$P(^PSDRUG($P(^(8),"^",5),0),"^"),1:$P(^PSDRUG(+DDRG,0),"^"))
 S:DDRG SPH=$S($P($G(^PSDRUG(+DDRG,8)),"^",5):$P(^PSDRUG($P(^(8),"^",5),0),"^",3),1:SPH)
 S UC="" F Q=0:0 S Q=$O(^PS(55,PSGP,5,PSJJORD,1,Q)) Q:'Q  S DD=$G(^(Q,0)) I DD,$S('$P(DD,"^",3):1,1:$P(DD,"^",3)>DT) S UC=UC+($P($G(^PSDRUG(+DD,660)),"^",6)*$S('$P(DD,"^",2):1,1:$P(DD,"^",2)))
 ;
 ;
 S Y=SI S:Y]"" Y=$$ENSET^PSGSICHK(Y) S ^TMP("PSG",$J,PSGAPWDN,PN,DRGN_"^"_PSJJORD)=DO_"^"_RTE_"^"_ST_"^"_ND2_"^"_SD_"^"_FD_"^"_SPH_"^"_N_"^"_NF_"^"_ND_"^"_UC_"^"_+DDRG S:Y]"" ^(DRGN_"^"_PSJJORD,1)=Y Q
 ;
PAT ;
 D PSJAC2^PSJAC(1),NOW^%DTC S PSGDT=%,PN=$P(PSGP(0),"^")_"^"_PSGP I PSJSEL("SELECT")="P" S PSGAPWDN=$S(PSJPWDN]"":PSJPWDN,1:"Outpatient")
 F STRT=PSGDT:0 S STRT=$O(^PS(55,PSGP,5,"AUS",STRT)) Q:'STRT  F PSJJORD=0:0 S PSJJORD=$O(^PS(55,PSGP,5,"AUS",STRT,PSJJORD)) Q:('PSJJORD)!(PSGBLANK=1)  D GOD
 Q:'$D(^TMP("PSG",$J,PSGAPWDN,PN))  K VASD,^UTILITY("VASD",$J) S DFN=PSGP,(PSGOD,SC)="" D SDA^VADPT I $D(^UTILITY("VASD",$J,1,"E")),$D(^("I")) S SC=$P(^("E"),"^",2),PSGOD=$$ENDTC^PSGMI(+^("I"))
 K VAEL S ELIG="" D ELIG^VADPT I $D(VAEL) S ELIG=$S(VAEL(3)["^":VAEL(3),1:"^")_"^"_VAEL(4)_"^"_VAEL(6)
 S ^TMP("PSG",$J,PSGAPWDN,PN)=$P(PSJPSEX,U,2)_U_$E($P(PSJPDOB,U,2),1,10)_";"_PSJPAGE_U_$P(PSJPSSN,U,2)_U_PSJPDX_U_$S(PSJPRB]"":PSJPRB,1:"*NF*")_U_$E($P(PSJPAD,U,2),1,10)_U_$E($P(PSJPTD,U,2),1,10)_U_$E(PSGOD,1,8)_U_SC_U_+PSJPWT,^(PN,0)=ELIG
 Q
 ;
GDT ;
 K %DT S %DT="EFTX",Y=-1,%DT(0)=$S(N["R":PSGDT,1:STT) F  W !!,"Enter ",N," date: " R X:DTIME W:'$T $C(7) S:'$T X="^" Q:"^"[X  D DTM^PSGDS:X?1."?",^%DT Q:Y>0
 I X'="^" S:N["R" STT=$S(Y'>0:PSGDT,Y#1:+$E(Y,1,12),1:Y+.0001) S:N["O" STP=$S(Y'>0:9999999,Y#1:+$E(Y,1,12),1:Y+.24)
 K %DT Q
 ;
 ;
EN ; entry point
 S X="" I '(PSGBLANK) I (PSJSEL("SELECT")'="P") D NOW^%DTC S PSGDT=% F N="START","STOP" D GDT Q:X="^"
 Q:X="^"  K ZTSAVE S:PSJSEL("SELECT")'="P" (ZTSAVE("STT"),ZTSAVE("STP"))="" F X="PSGP","PSJSEL(","PSGAPWD","PSGAPWG","PSGAPWDN","PSGAPWGN","PSGBLANK","PSGPAT(","PSGPTMP","PPAGE" S ZTSAVE(X)=""
 W !,"...this may take a few minutes...(you should QUEUE this report)..."
 S PSGTIR="ENQ^PSGDS0",ZTDESC="DISCHARGE SUMMARY" D ENDEV^PSGTI Q:POP!$D(IO("Q"))
 ;
ENQ ; queued entry point
 K ^TMP("PSG",$J) S PSJACNWP=1 N RBP S RBP=$S($D(PSJSEL("RBP")):PSJSEL("RBP"),1:"P") D @("P"_PSJSEL("SELECT")) D:'PSGBLANK ^PSGDSP D ^%ZISC
 K %DT,AM,DRGN,LQ,N,SC,SPH,UC,VASD,^TMP("PSG",$J),^UTILITY("VASD",$J) Q
 ;
PG ;
 F PSGAPWD=0:0 S PSGAPWD=$O(^PS(57.5,"AC",PSGAPWG,PSGAPWD)) Q:'PSGAPWD  I $D(^DIC(42,PSGAPWD,0)),$P(^(0),"^")]"" S PSGAPWDN=$P(^(0),"^") D PW  Q:$G(NP)="^"
 Q
 ;
PW ;
 I $D(PSJSEL("TM")) S TM="" F  S TM=$O(PSJSEL("TM",TM)) Q:TM=""  S PSGPATM(TM)=TM
 S PSGP=0
 F  S PSGP=$O(^DPT("CN",PSGAPWDN,PSGP)) Q:'PSGP  D  Q:$G(NP)="^"
 .I PSGBLANK=1 D EN^PSGDSP1 Q
 .S LQ=0,Q=STT-.000001 F  Q:LQ  S Q=$O(^PS(55,PSGP,5,"AUS",Q)) Q:'Q  D
 ..F QQ=0:0 S QQ=$O(^PS(55,PSGP,5,"AUS",Q,QQ)) Q:'QQ  I $P($G(^PS(55,PSGP,5,QQ,2)),"^",2)'>STP S RB=$G(^DPT(PSGP,.101)),TM="zz" D  S LQ=1 Q
 ..I '$D(PSGATM) D SET Q
 ..S:RB TM=$O(^PS(57.7,"AWRT",PSGAPWD,RB,0)) S:'TM TM="zz" I $D(PSGPATM("ALL"))!$D(PSGPATM(TM)) D SET Q
 I $D(^TMP("PSGDS",$J)) N PSGX S PSGX="^TMP(""PSGDS"",$J)" F  S PSGX=$Q(@PSGX) Q:PSGX'[("""PSGDS"""_","_$J)  S PSGP=$G(@PSGX) D PAT Q:$G(X)?1"^"."^"
 Q
 ;
SET ;
 S:TM'["zz" TM=$G(^PS(57.7,$G(PSGAPWD),1,TM,0)) I $G(RB)="" S RB="z"
 I RBP="P" D ^PSJAC S ^TMP("PSGDS",$J,TM,PSGP(0))=PSGP Q
 I RBP="R" S ^TMP("PSGDS",$J,TM,RB)=PSGP
 Q
 ;
PP ;
 N PAT S PAT="" F  S PAT=$O(PSGPAT(PAT)) Q:PAT=""  S PSGP=$G(PSGPAT(PAT)) D @$S(PSGBLANK=1:"EN^PSGDSP1",1:"PAT")  Q:$G(NP)="^"
 Q

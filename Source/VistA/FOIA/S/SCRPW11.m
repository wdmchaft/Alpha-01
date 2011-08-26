SCRPW11 ;RENO/KEITH - Patient Activity by Appointment Frequency ; 15 Jul 98  02:38PM
 ;;5.3;Scheduling;**139,144,562**;AUG 13, 1993;Build 7
 D TITL^SCRPW50("Patient Activity by Appointment Frequency") N SDDIV Q:'$$DIVA^SCRPW17(.SDDIV)
DTR D SUBT^SCRPW50("*** Date Range Selection ***")
FDT W ! S %DT="AEPX",%DT("A")="Beginning date: ",%DT(0)="-TODAY" D ^%DT G:X=U!($D(DTOUT)) EXIT G:X="" EXIT
 G:Y<1 FDT S SDBDAY=Y X ^DD("DD") S SDPBDA=Y
LDT W ! S %DT("A")="   Ending date: " D ^%DT G:X=U!($D(DTOUT)) EXIT G:X="" EXIT
 I Y<SDBDAY W !!,$C(7),"Ending date must be after beginning date!" G LDT
 G:Y<1 LDT S SDEDAY=Y X ^DD("DD") S SDPEDA=Y
 D SUBT^SCRPW50("*** Report Format Selection ***")
 W ! S DIR(0)="N^1:999:0",DIR("?")="Enter the minimum number of appointments for a patient to be included in this report",DIR("A")="Minimum appointment frequency" D ^DIR G:Y'>0 EXIT S SDMIN=Y
 K DIR S DIR(0)="S^R:RANGE OF STOP CODES;S:SELECTED STOP CODES;C:CLINIC GROUP",DIR("A")="Limit clinics by",DIR("?")="Output will be limited to primary stop codes or clinic group as specified."
 D ^DIR G:$D(DTOUT)!$D(DUOUT) EXIT S SDFMT=Y,SDOUT=0 D @(SDFMT) G:SDOUT EXIT
 D SUBT^SCRPW50("*** Output Order Selection ***")
 K DIR S DIR(0)="S^A:ALPHABETIC;V:VISIT FREQUENCY",DIR("A")="Specify output order" D ^DIR G:$D(DTOUT)!$D(DUOUT) EXIT S SDORD=Y
 N ZTSAVE F V="SD(","SDDIV","SDDIV(","SDFMT","SDMIN","SDBDAY","SDEDAY","SDPBDA","SDPEDA","SDBCS","SDECS","SDORD","SDMC","SDMD" S ZTSAVE(V)=""
 W !!,"This report requires 132 column output.",! D EN^XUTMDEVQ("START^SCRPW11","PT. ACTIVITY BY APPT. FREQUENCY",.ZTSAVE) G EXIT
 ;
START K ^TMP("SCRPW",$J) S (SDOUT,SDSTOP)=0,SDMD=$O(SDDIV(0)),SDMD=$O(SDDIV(SDMD)) S:$P(SDDIV,U,2)="ALL DIVISIONS" SDMD=1
 I SDFMT="C" S SDCG=$O(SD(0)),SDCL=0 F  S SDCL=$O(^SC("ASCRPW",SDCG,SDCL)) Q:'SDCL  S SDCL0=$G(^SC(SDCL,0)) I $$DIV() D LOOP Q:SDOUT
 G:SDOUT EXIT
 I SDFMT'="C" S SDCL=0 F  S SDCL=$O(^SC(SDCL)) Q:SDCL'>0  S SDCL0=$G(^SC(SDCL,0)) I $$DIV() D LOOP Q:SDOUT
 G:SDOUT EXIT
 S SDIV="" F  S SDIV=$O(^TMP("SCRPW",$J,SDIV)) Q:SDIV=""!SDOUT  S DFN=0 F  S DFN=$O(^TMP("SCRPW",$J,SDIV,0,DFN)) Q:DFN'>0  S SDSTOP=SDSTOP+1 D:SDSTOP#1000=0 STOP Q:SDOUT  D FAPP,ORDR
 S SDII=1,SDT(SDII)="<*>  PATIENT ACTIVITY BY APPOINTMENT FREQUENCY  <*>" S:SDFMT="R" SDII=SDII+1,SDT(SDII)="IN CLINICS WITH PRIMARY STOP CODES "_SDBCS_" TO "_SDECS
 I SDFMT="S" S SDI=0 F  S SDI=$O(SD(SDI)) Q:'SDI  S SDII=SDII+1,SDT(SDII)="IN CLINICS WITH PRIMARY STOP CODE: "_SDI
 I SDFMT="C" S SDII=SDII+1,SDT(SDII)="IN CLINIC GROUP: "_SD(SDCG)
 S SDII=SDII+1,SDT(SDII)="FOR PATIENTS WITH AT LEAST "_SDMIN_" APPOINTMENTS TO THESE CLINICS",SDII=SDII+1
 D NOW^%DTC S Y=% X ^DD("DD") S SDPNOW=$P(Y,":",1,2),SDPAGE=1,SDLINE="",$P(SDLINE,"-",133)=""
 S SDIV="" F  S SDIV=$O(SDDIV(SDIV)) Q:'SDIV  S SDIV(SDDIV(SDIV))=SDIV
 I 'SDDIV,$P(SDDIV,U,2)'="ALL DIVISIONS" S SDIV($P(SDDIV,U,2))=$$PRIM^VASITE()
 I $P(SDDIV,U,2)="ALL DIVISIONS" S SDI=0 F  S SDI=$O(^TMP("SCRPW",$J,SDI)) Q:'SDI  S SDX=$P($G(^DG(40.8,SDI,0)),U) S:$L(SDX) SDIV(SDX)=SDI
 D:$E(IOST)="C" DISP0^SCRPW23 S (SDI,SDIV)="" F  S SDIV=$O(^TMP("SCRPW",$J,SDIV)) Q:SDI!(SDIV="")  S:$O(^TMP("SCRPW",$J,SDIV,2,(SDMIN-1))) SDI=1
 I 'SDI S SDIV=0 D DHDR^SCRPW40(SDII,.SDT),HDR G:SDOUT EXIT S SDX="No activity found that meets report criteria!" W !!?(IOM-$L(SDX)\2),SDX G EXIT
 S SDIVN="" F  S SDIVN=$O(SDIV(SDIVN)) Q:SDIVN=""!SDOUT  S SDIV=SDIV(SDIVN) D DPRT(.SDIV)
 S SDI=0,SDI=$O(^TMP("SCRPW",$J,SDI)),SDMD=$O(^TMP("SCRPW",$J,SDI))
 G:SDOUT EXIT I SDMD S SDIV=0 D DPRT(.SDIV)
 I $E(IOST)="C",'SDOUT N DIR S DIR(0)="E" D ^DIR
 G EXIT
 ;
DPRT(SDIV) ;Print report for a division
 D DHDR^SCRPW40(SDII,.SDT) I '$O(^TMP("SCRPW",$J,SDIV,2,(SDMIN-1))) S SDX="No activity found for this division within report parameters!" D HDR Q:SDOUT  W !!?(IOM-$L(SDX)\2),SDX Q
 D HDR Q:SDOUT
 S SDFREQ=999999999 F  S SDFREQ=$O(^TMP("SCRPW",$J,SDIV,2,SDFREQ),-1) Q:SDFREQ'>0!(SDFREQ<SDMIN)!SDOUT  S DFN=0 F  S DFN=$O(^TMP("SCRPW",$J,SDIV,2,SDFREQ,DFN)) Q:DFN'>0!SDOUT  D:SDORD="V" PRT D:SDORD="A" ALPH
 I SDORD="A" S SDNAM="" F  S SDNAM=$O(^TMP("SCRPW",$J,SDIV,3,SDNAM)) Q:SDNAM=""!SDOUT  S DFN=0 F  S DFN=$O(^TMP("SCRPW",$J,SDIV,3,SDNAM,DFN)) Q:'DFN!SDOUT  S SDFREQ=^TMP("SCRPW",$J,SDIV,3,SDNAM,DFN) D PRT
 Q
 ;
EXIT D END^SCRPW50 K SD,SDFMT,SDBCS,SDBDAY,SDCL,SDCL0,SDCLCS,SDCLPT,SDCS,SDDAY,DFN,SDECS,SDEDAY,SDFREQ,SDMIN,SDPTAP0,SDPTCLT,SDPTCSF,SDI,SDOUT,DIC,DTOUT,DUOUT,%,%H,%I,%DT,SDDIV,SDII,SDMC,SDMD,X,SDIVN,SDSTOP,SDX
 D KVA^VADPT K SDCG,SDORD,SDNAM,SDCLNA,SDPTCS,SDT,SDBDAY,SDEDAY,SDRBDA,SDREDA,SDPBDA,SDPEDA,SDPNOW,SDLINE,SDPAGE,DGPGM,DGVAR,DIR,POP,Y,V,ZTSAVE,^TMP("SCRPW",$J) Q
 ;
STOP ;Check for stop task request
 S:$G(ZTQUEUED) (SDOUT,ZTSTOP)=$S($$S^%ZTLOAD:1,1:0) Q
 ;
R K DIR S DIR(0)="N^101:999:0",DIR("?")="Specify a range of Clinic Stop codes defined for clinics to be returned in this report",DIR("A")="Start with CLINIC STOP"
 W ! D ^DIR S:$D(DTOUT)!$D(DUOUT)!($G(Y)<1) SDOUT=1 Q:SDOUT  S SDBCS=Y,DIR(0)="N^"_Y_":999:0",DIR("A")="End with CLINIC STOP" D ^DIR S:$D(DTOUT)!$D(DUOUT)!($G(Y)<1) SDOUT=1 Q:SDOUT  S SDECS=Y Q
 ;
S K SD,DIC S DIC="^DIC(40.7,",DIC(0)="AEMQZ" F  D S1 Q:$G(Y)<1
 S:$D(DTOUT)!$D(DUOUT)!'$D(SD) SDOUT=1 Q
 ;
S1 D ^DIC I Y>0 S SD($P(Y(0),U,2))=""
 Q
 ;
C K SD,DIC S DIC="^SD(409.67,",DIC(0)="AEMQ" W ! D ^DIC I $D(DTOUT)!$D(DUOUT)!($G(Y)<1) S SDOUT=1 Q
 S SD(+Y)=$P(Y,U,2) Q
 ;
DIV() ;Check division
 Q:'SDDIV 1  Q $D(SDDIV(+$P(SDCL0,U,15)))
 ;
LOOP S SDCLCS=$P(SDCL0,U,7),SDCLCS=$P($G(^DIC(40.7,+SDCLCS,0)),U,2),SDIV=$P(SDCL0,U,15) S:'SDIV SDIV=$$PRIM^VASITE()
 I SDFMT="R" Q:SDCLCS<SDBCS!(SDCLCS>SDECS)
 I SDFMT="S" Q:'$D(SD(+SDCLCS))
L1 S SDDAY=SDBDAY F  S SDDAY=$O(^SC(SDCL,"S",SDDAY)) Q:(SDDAY'>0!(SDDAY>SDEDAY))!SDOUT  S SDCLPT=0 F  S SDCLPT=$O(^SC(SDCL,"S",SDDAY,1,SDCLPT)) Q:SDCLPT'>0!SDOUT  S DFN=+^SC(SDCL,"S",SDDAY,1,SDCLPT,0) D EVAL
 Q
 ;
EVAL S SDSTOP=SDSTOP+1 I SDSTOP#1000=0 D STOP Q:SDOUT
 ;SD*562 if DFN missing in appt sub-file of file #44 delete record
 I '$D(DFN)!(DFN="")!(DFN=0) D  Q
 .S DA(2)=SDCL,DA(1)=SDDAY,DA=SDCLPT
 .S DIK="^SC("_DA(2)_",""S"","_DA(1)_",1," D ^DIK
 .K DA,DIK
 S SDPTAP0=^DPT(DFN,"S",SDDAY,0) Q:$P(SDPTAP0,U,2)["C"!($P(SDPTAP0,U,2)["N")  D EV1(SDIV) D:SDMD EV1(0)
 Q
 ;
EV1(SDIV) S ^TMP("SCRPW",$J,SDIV,0,DFN)=$G(^TMP("SCRPW",$J,SDIV,0,DFN))+1,^TMP("SCRPW",$J,SDIV,0,DFN,SDCLCS,$P(SDCL0,U))=$G(^TMP("SCRPW",$J,SDIV,0,DFN,SDCLCS,$P(SDCL0,U)))+1
 S ^TMP("SCRPW",$J,SDIV,0,DFN,SDCLCS)=$G(^TMP("SCRPW",$J,SDIV,0,DFN,SDCLCS))+1 Q
 ;
FAPP S SDDAY=DT F  S SDDAY=$O(^DPT(DFN,"S",SDDAY)) Q:SDDAY'>0  S SDPTAP0=^DPT(DFN,"S",SDDAY,0),SDCL=+SDPTAP0,SDCL0=^SC(SDCL,0),SDCLCS=$P(SDCL0,U,7),SDCLCS=$P(^DIC(40.7,SDCLCS,0),U,2) D FAP1
 Q
 ;
FAP1 I SDFMT="R",$P(SDPTAP0,U,2)'["C",SDCLCS'<SDBCS,SDCLCS'>SDECS S ^TMP("SCRPW",$J,SDIV,1,DFN,SDDAY,$P(SDCL0,U))=SDCLCS
 I SDFMT="S",$P(SDPTAP0,U,2)'["C",$D(SD(+SDCLCS)) S ^TMP("SCRPW",$J,SDIV,1,DFN,SDDAY,$P(SDCL0,U))=SDCLCS
 Q
 ;
ORDR S SDFREQ=^TMP("SCRPW",$J,SDIV,0,DFN),^TMP("SCRPW",$J,SDIV,2,SDFREQ,DFN)="",SDPTCS=0
 F  S SDPTCS=$O(^TMP("SCRPW",$J,SDIV,0,DFN,SDPTCS)) Q:SDPTCS'>0  S SDPTCSF=^TMP("SCRPW",$J,SDIV,0,DFN,SDPTCS),^TMP("SCRPW",$J,SDIV,2,SDFREQ,DFN,SDPTCSF,SDPTCS)=""
 Q
 ;
HDR ;Print report header
 D STOP Q:SDOUT
 I $E(IOST)="C",SDPAGE>1 N DIR S DIR(0)="E" D ^DIR S SDOUT=Y'=1 Q:SDOUT
 W:SDPAGE>1!($E(IOST)="C") $$XY^SCRPW50(IOF,1,0) W:$X $$XY^SCRPW50("",0,0) W SDLINE S SDI=0 F  S SDI=$O(SDT(SDI)) Q:'SDI  W !?(132-$L(SDT(SDI))\2),SDT(SDI)
 W !,SDLINE,!,"For date range: ",SDPBDA," to ",SDPEDA,!,"Date printed: ",SDPNOW,?(126-$L(SDPAGE)),"Page: ",SDPAGE,!,SDLINE S SDPAGE=SDPAGE+1 Q
 ;
PRT D:$Y>(IOSL-7) HDR Q:SDOUT  D ^VADPT W !!,"Number of appts.: ",SDFREQ,?24,"Patient: ",$E(VADM(1),1,30),?65,"SSN: ",$P(VADM(2),U,2)
 S SDPTCSF=99999999 F  S SDPTCSF=$O(^TMP("SCRPW",$J,SDIV,2,SDFREQ,DFN,SDPTCSF),-1) Q:SDPTCSF'>0!SDOUT  S SDCLCS=0 F  S SDCLCS=$O(^TMP("SCRPW",$J,SDIV,2,SDFREQ,DFN,SDPTCSF,SDCLCS)) Q:SDCLCS'>0!SDOUT  D PRT1
 I $D(^TMP("SCRPW",$J,SDIV,1,DFN)) D:$Y>(IOSL-5) HDR Q:SDOUT  W !?44,"FUTURE APPOINTMENTS:" S SDDAY=0 F  S SDDAY=$O(^TMP("SCRPW",$J,SDIV,1,DFN,SDDAY)) Q:SDDAY'>0!SDOUT  D PRT2
 Q
 ;
PRT1 D:$Y>(IOSL-6) HDR Q:SDOUT  S SDCS=0,SDCS=$O(^DIC(40.7,"C",SDCLCS,SDCS)),SDCS=$P(^DIC(40.7,SDCS,0),U) W !?29,SDPTCSF," appointments to ",SDCS,"  (",SDCLCS,")"
 S SDCLNA="" F  S SDCLNA=$O(^TMP("SCRPW",$J,SDIV,0,DFN,SDCLCS,SDCLNA)) Q:SDCLNA']""  D:$Y>(IOSL-5) HDR Q:SDOUT  S SDPTCLT=^TMP("SCRPW",$J,SDIV,0,DFN,SDCLCS,SDCLNA) W !?34,SDPTCLT,"  ",SDCLNA," appointment",$S(SDPTCLT=1:"",1:"s")
 Q
 ;
PRT2 S SDCLNA="" F  S SDCLNA=$O(^TMP("SCRPW",$J,SDIV,1,DFN,SDDAY,SDCLNA)) Q:SDCLNA']""  D:$Y>(IOSL-4) HDR Q:SDOUT  S SDCLCS=^TMP("SCRPW",$J,SDIV,1,DFN,SDDAY,SDCLNA),Y=SDDAY X ^DD("DD") W !?44,Y,"  ",SDCLNA,"  (",SDCLCS,")"
 Q
 ;
ALPH D ^VADPT S SDNAM=VADM(1),^TMP("SCRPW",$J,SDIV,3,SDNAM,DFN)=SDFREQ Q

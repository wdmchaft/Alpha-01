IBAMTC2 ;ALB/CJM - INTEGRATED BILLING, CLEANUP OF UNCLOSED EVENTS, UNPASSED CHARGES ; 04-APRIL-1992
 ;;2.0;INTEGRATED BILLING;**132,176**;21-MAR-94
 ;;Per VHA Directive 10-93-142, this routine should not be modified.
 ;
MAIN ;
 N IBAGE,IBFREQ,IBCHG,DFN,IBN,IBND,IBSL,IBDISC,DIE,DR,DA,IBQUIT,IBPASS,IBOLD,IBDATE,IBDUZ S IBDUZ=$G(DUZ)
 D NOW^%DTC S IBDATE=X
 S IBAGE=44,IBFREQ=15 ; age of unpassed charges to report, frequency
 ; loop through all incomplete entries in file 350
 N IBFLLTC
 S IBN="" F  S IBN=$O(^IB("AC",1,IBN)) Q:'IBN  S IBND=$G(^IB(IBN,0)) D
 .Q:($P(IBND,"^",5)'=1)!($P(IBND,"^",16)']"")
 .I $P(IBND,"^",16)=IBN S IBFLLTC="" D  Q:IBFLLTC="L"
 ..;
 ..N IBDISC,IBSL,VAIN,VAINDT,IBLDT D DISC Q:+IBDISC=0
 ..S DFN=$P(IBND,"^",2),VAINDT=IBDISC D INP^VADPT S IBFLLTC=$P($$TREATSP^IBAECU2($P($G(^DIC(45.7,+VAIN(3),0)),U,2)),"^",1)
 ..S IBLDT=$$LASTMJ^IBAECU() I IBLDT>0,$E(IBDISC,1,5)<$E(IBLDT,1,5),IBFLLTC="L" D CLOSE
 .I $P(IBND,"^",16)=IBN D
 ..D EVENT
 .E  D CHARGE
 Q
EVENT ; closes events if the patient was discharged
 S (IBPASS,IBQUIT)=0
 D DISC I IBDISC D CLOSE D:'IBQUIT FNDCHGS,PASS:IBCHG,BULLET1^IBAMTC3
 Q
DISC ; gets the discharge date
 S IBDISC="",IBSL=$P(IBND,"^",4)
 I $P(IBSL,":")=405 S IBDISC=$P(IBSL,":",2) S:IBDISC]"" IBDISC=$P($G(^DGPM(IBDISC,0)),"^",17)
 S:IBDISC IBDISC=($P($G(^DGPM(IBDISC,0)),"^")\1)
 Q
CLOSE ;
 S IBQUIT=1
 L +^IB(IBN):3 I $T D
 .S IBQUIT=0
 .S DIE="^IB(",DA=IBN,DR=".05////2"
 .D ^DIE L -^IB(IBN)
 Q
FNDCHGS ;
 N I S IBCHG="" F I=1:1 S IBCHG=$O(^IB("ACT",IBN,IBCHG)) Q:'IBCHG  S IBCHG(I)=IBCHG
 S IBCHG=(I-1)
 Q
PASS ; pass the charges if they appear correct, complete, and can be locked
 S IBPASS=0
 N IBI,IBNOS,IBADMIT S DFN=$P(IBND,"^",2),IBADMIT=($P(IBND,"^",17)\1)
 Q:+$$MVT^DGPMOBS($P(IBSL,":",2))
 I IBDISC=$P(IBND,"^",17) Q:$P(IBND,"^",18)'=IBDISC
 E  S X1=$P(IBND,"^",18),X2=1 D C^%DTC Q:X'=IBDISC
 S IBPASS=1 F IBI=1:1:IBCHG L +^IB(IBCHG(IBI)):1 S IBPASS=$T Q:'IBPASS  I ($P($G(^IB(IBCHG(IBI),0)),"^",15)>IBDISC)!($P($G(^IB(IBCHG(IBI),0)),"^",14)<IBADMIT) S IBPASS=0 Q
 I IBPASS N IBN F IBI=1:1:IBCHG S IBNOS=IBCHG(IBI),IBY=1 D FILER^IBAUTL5 D:IBY<1 ^IBAERR1
 F IBI=1:1:IBCHG L -^IB(IBCHG(IBI))
 Q
 ;
CHARGE ; if the charge is old send a bulletin
 N IBWHEN S IBWHEN=$P($G(^IB(IBN,1)),"^",2)
 S X2=IBWHEN,X1=IBDATE D ^%DTC
 S IBOLD=(+$FN(X,"T")) I IBOLD>IBAGE,X#IBFREQ=0 D BULLET2^IBAMTC3
 Q

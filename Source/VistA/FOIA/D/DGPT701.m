DGPT701 ;ALB/MTC - Process 701 Transaction ;10/06/1999
 ;;5.3;Registration;**64,164,251,415,729**;Aug 13, 1993;Build 59
 ; 10/06/1999 ACS - Removed Place of Disposition codes M,Y,Z from the list of
 ; invalid codes.
 ;
EN ;
 Q
SET ;
 S DGPTSTR=$G(^TMP("AEDIT",$J,"N701",DGPTAL7))
 D PARSE^DGPT701P
DTE ;
 S (X,DGPTDDS)=$$FMDT^DGPT101($E(DGPTDDTD,1,6))_"."_$E(DGPTDDTD,7,10)
 S %DT="XT" D ^%DT I Y<0 S DGPTERC=705 D ERR G:DGPTEDFL EXIT
 I Y>0 D DD^%DT S DGPTDTD=$E(Y,5,6)_"-"_$E(Y,1,3)_"-"_$E(Y,9,12)_" "_$S($P(Y,"@",2)]"":$E($P(Y,"@",2),1,5),1:"00:00")
 S X1=DGPTNOW,X2=+DGPTDDS D ^%DTC I X<0 S DGPTERC=740 D ERR G:DGPTEDFL EXIT
 S X1=+DGPTDDS,X2=+DGPTDTS D ^%DTC S DGPTELP=X I X<0 S DGPTERC=737 D ERR G:DGPTEDFL EXIT
CHECK ;
TSPEC ; CHECK TREATING SPECIALTY CODE
 N DGPTDSP1
 I DGPTDSP'?2AN S DGPTERC=706 D ERR G:DGPTEDFL EXIT G DISPTY
 S DGPTSP1=$E(DGPTDSP,1),DGPTSP2=$E(DGPTDSP,2),DGPTERC=0
 D CHECK^DGPTAE02 I DGPTERC S DGPTERC=706 D ERR G:DGPTEDFL EXIT G DISPTY
 ;-- Active treating specialty edit check
 I $E(DGPTDSP,1)=0!($E(DGPTDSP,1)=" ") S DGPTDSP=$E(DGPTDSP,2)
 ; DGPTDSP  := ptf code (alpha-numeric) value (file:42.4,field:7)
 ; DGPTDSP1 := dinum value (ien, file:42.4,field:.001)
 S DGPTDSP1=+$O(^DIC(42.4,"C",DGPTDSP,0))
 ;-- If not active treat spec, set flag to print error msg during
 ;-- PTF Close-out Error display at WRER^DGPTAEE
 I '$$ACTIVE^DGACT(42.4,DGPTDSP1,DGPTDDS) S DGPTERC=706,DGPTSER(DGPTDDS_701)=1 D ERR G:DGPTEDFL EXIT
 ;
DISPTY ;
 I (DGPTDTY<1)!(DGPTDTY>7) S DGPTERC=707 D ERR G:DGPTEDFL EXIT G OPCAR
 S DGPTERC=0 D DISPTY^DGPTAE02 I DGPTERC D ERR G:DGPTEDFL EXIT
OPCAR ;
 I "13 "'[DGPTDOP S DGPTERC=708 D ERR G:DGPTEDFL EXIT G VA
 I DGPTDOP'=" " S DGPTERC=0 D OP^DGPTAE02 I DGPTERC D ERR G:DGPTEDFL EXIT
VA ;
 I "12 "'[DGPTDVA S DGPTERC=709 D ERR G:DGPTEDFL EXIT
 ;
VAOP ;-- check for inconsistencies between opcare and va aspices
 I DGPTDVA=2,DGPTDOP=1 D  G:DGPTEDFL EXIT
 . S DGPTERC=708 D ERR
 . S DGPTERC=709 D ERR
CDR ;
 I DGPTDLR'?6" "&(DGPTDLR'?." "6N) S DGPTERC=775 D ERR G:DGPTEDFL EXIT
POD ;
 ;I "68EIMNOQSVWYZ"[DGPTDPD S DGPTERC=710 D ERR G:DGPTEDFL EXIT G RECF
 I "68EINOQSVW"[DGPTDPD S DGPTERC=710 D ERR G:DGPTEDFL EXIT G RECF
 S DGPTERC=0 D POD^DGPTAE02 I DGPTERC D ERR G:DGPTEDFL EXIT
RECF ;
 I DGPTDVA'=1!(DGPTDRF="      ") G ASIH
 I DGPTDRF[" " S DGPTDRF=$P(DGPTDRF," ",1)
 I DGPTDRF="" S DGPTERC=711 D ERR G:DGPTEDFL EXIT
ASIH ;
 I DGPTDAS'="   ",DGPTDAS'?2E1N S DGPTERC=712 D ERR G:DGPTEDFL EXIT
 ;
LEAVE ;
 S DGPTERC=0 D LEAVE^DGPTAE02 D:DGPTERC ERR G:DGPTEDFL EXIT
SC ;
 I DGPTDSC'="   "&(DGPTDSC'?3N) S DGPTERC=730 D ERR G:DGPTEDFL EXIT G CP
 S DGPTDSC=+DGPTDSC
CP ;
 S DGPTERC=0 D CANDP^DGPTAE02 I DGPTERC D ERR G:DGPTEDFL EXIT
DIAG ;
 S DGPTERC=0 D ^DGPT70DX I DGPTERC D ERR G:DGPTEDFL EXIT
OVER ; Pass FY92 edits for earlier data
 I DGPTDDS'>2911001 G ONED
LEG ; LEGIONNAIRE'S DISEASE
 S DGPTERC=0 D LEG^DGPTAE02 I DGPTERC D ERR G:DGPTEDFL EXIT
SUI ; Suicide indicator
 S DGPTERC=0 D SUI^DGPTAE02 I DGPTERC D ERR G:DGPTEDFL EXIT
DRUG ;
 S DGPTERC=0 D DRUG^DGPTAE02 I DGPTERC D ERR G:DGPTEDFL EXIT
AXES ; Psych axises
 I '$P($G(^DIC(42.4,+DGPTDSP1,0)),U,4) S (DGPT70X4,DGPT7X51,DGPT7X52)=" " G ONED
 S DGPTERC=0 D AXIV^DGPTAE02 I DGPTERC D ERR G:DGPTEDFL EXIT
 S DGPTERC=0 D AXV1^DGPTAE02 I DGPTERC D ERR G:DGPTEDFL EXIT
 S DGPTERC=0 D AXV2^DGPTAE02 I DGPTERC D ERR G:DGPTEDFL EXIT
ONED ;
 I (DGPTDDXO=" ")&('$D(^TMP("AEDIT",$J,"N702"))&'$D(^TMP("AEDIT",$J,"N703"))) S DGPTERC=718 D ERR G:DGPTEDFL EXIT
 I (DGPTDDXO="X")&($D(^TMP("AEDIT",$J,"N072"))) S DGPTERC=719 D ERR G:DGPTEDFL EXIT
EXIT ;
 Q
ERR ;
 D WRTERR^DGPTAE(DGPTERC,"N701",DGPTAL7)
 S ERROR=1
 Q

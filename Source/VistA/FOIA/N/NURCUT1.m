NURCUT1 ;HIRMFO/RM-UTILITIES FOR CLINICAL NURSING ;APR 4, 1994
 ;;4.0;NURSING SERVICE;**18**;Apr 25, 1997
ALLERGY(DFN,ARRAY) ; PROCEDURE RETURNS PT A/AR INFO
 ; INPUT: DFN=PTR TO PATIENT
 ;        ARRAY=RESULTS RETURNED IN ARRAY (PASSED BY REF)
 ; OUTPUT:  ARRAY(X)=A
 ;            WHERE X IS 1..NUMBER OF ALLERGIES
 ;                  A IS FREE TEXT ALLERGY (MECHANISM)
 N GMRAL,CTR,LPV K ARRAY
 S X="GMRADPT" X ^%ZOSF("TEST") I $T D EN1^GMRADPT S ARRAY=GMRAL
 I $O(GMRAL(0)) D
 .   S CTR=0
 .   S LPV=0 F  S LPV=$O(GMRAL(LPV)) Q:LPV'>0  D
 .   .   S CTR=CTR+1
 .   .   S ARRAY(CTR)=$P(GMRAL(LPV),"^",2)_$P(" (allergy)^ (adv rxn)","^",$F("01",$P(GMRAL(LPV),"^",5))-1)
 .   .   Q
 .   Q
 Q
DX(DFN,DATE) ; PROCEDURE RETURNS PT DX INFO
 ;  INPUT: DFN=PTR TO PATIENT
 ;         DATE=DATE TO FIND CURRENT DX FOR
 ; OUTPUT:
 Q

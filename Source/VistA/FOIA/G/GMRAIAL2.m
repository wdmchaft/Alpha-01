GMRAIAL2 ;BPOIFO/JG - BUILD HL7 ORU^R01 MESSAGE FOR ALLERGIES - PART 2 ; 17 Mar 2006  11:55 AM
 ;;4.0;Adverse Reaction Tracking;**22,23,34**;Mar 29, 1996
 ; Creates HL7 V2.4 ORU^R01 message for allergy updates & assessments
 ;
 ; This routine uses the following IAs:
 ;   #4248 - VDEFEL calls        (controlled)
 ;   #3630 - VAFCQRY calls       (controlled)
 ;   #4531 - ZERO^PSN50P41       (supported)
 ;   #2574 - $$CLASS2^PSNAPIS    (supported)
 ;
 ; This routine is called as a subroutine by GMRAIAL1
 ;
 Q
 ;
ENTRY ; Entry point from GMRAIAL1
 ;
 ; Skip to OBX8 if doing an assessment
 G OBX8:ALTYPE=2
 ;
 ; OBX 1 - Reactant
OBX1 S S=S+1,OUTX=1_HLFS_"CE"_HLFS_"AGENT"_HLFS
 S $P(OUTX,HLFS,5)=$$HL7RC^GMRAIAL1($P(ALRDATA,U,2)),$P(OUTX,HLFS,11)=RSLTSTA
 S X=$P(SITEPARM,U,6)_HLCM_$P(SITEPARM,U,5)_HLCM_"L",$P(OUTX,HLFS,15)=X
 S OUTX="OBX"_HLFS_OUTX D SAVE^GMRAIAL1
 ;
 ; OBX2 - Allergy Type
OBX2 S S=S+1,OUTX=1_HLFS_"CE"_HLFS_"ALLERGY TYPE"_HLFS
 S X=$P(ALRDATA,U,20),VAL=X_HLCM F I=1:1:$L(X) D
 . S Y=$E(X,I),Y=$S(Y="D":"DRUG",Y="F":"FOOD",Y="O":"OTHER",1:"")
 . S VAL=VAL_Y S:I<$L(X) VAL=VAL_","
 S VAL=VAL_HLCM_"L",$P(OUTX,HLFS,5)=VAL,$P(OUTX,HLFS,11)=RSLTSTA
 S X=$P(SITEPARM,U,6)_HLCM_$P(SITEPARM,U,5)_HLCM_"L",$P(OUTX,HLFS,15)=X
 S OUTX="OBX"_HLFS_OUTX D SAVE^GMRAIAL1
 ;
 ; OBX3 - GMR Allergy
 ;        ALLERGIES FROM VA DRUG CLASS HAVE SPECIAL FORMAT FOR
 ;        OBX-5
OBX3 S S=S+1,OUTX=1_HLFS_"CE"_HLFS_"GMR ALLERGY"_HLFS
 S VAL="",X=$P(ALRDATA,U,3) G OBX4:X=""
 I X'["50.605" D
 . S VAL=$P($G(@("^"_$P(X,";",2)_$P(X,";",1)_",0)")),U)
 . I VAL="" D ERR^VDEFREQ("No data in GMR Allergy file "_$P(X,";",2)_$P(X,";")_",0)") S ZTSTOP=1 Q
 . I X["PSDRUG" S GMRAVUID=$P(X,";",1)_U_$P(SITEPARM,U,6)_"_"_50
 . E  D
 . . S GMRAFILE=+$P($P(X,";",2),"(",2),GMRAIENS=+X_","
 . . S GMRAVUID=$$GETVUID^GMRAIAL1(GMRAFILE,.01,GMRAIENS,1)
 . S VAL=$P(GMRAVUID,U)_HLCM_VAL_HLCM_$P(GMRAVUID,U,2)
 I X["50.605" D
 . S GMRAIENS=$P(X,";"),Y=$$CLASS2^PSNAPIS(GMRAIENS)
 . S GMRAVUID=$$GETVUID^GMRAIAL1(50.605,.01,GMRAIENS_",",1)
 . S VAL=$G(VAL)_$P(GMRAVUID,U)_HLCM_$P(Y,U,2)_HLCM_$P(GMRAVUID,U,2)_HLCM_$P(Y,U)_HLCM_$P(Y,U,2)_HLCM_$P(SITEPARM,U,6)_"_50.605"
 G RETURN:ZTSTOP
 S $P(VAL,HLCM,2)=$$HL7RC^GMRAIAL1($P(VAL,HLCM,2)),$P(OUTX,HLFS,5)=VAL,$P(OUTX,HLFS,11)=RSLTSTA ;34
 S OUTX="OBX"_HLFS_OUTX D SAVE^GMRAIAL1
 ;
 ; OBX4 - List of drug ingredients
OBX4 G OBX5:'$D(^GMR(120.8,KEY,2))
 S S=S+1,OUTX=1_HLFS_"CE"_HLFS_"DRUG INGREDIENTS"_HLFS
 S IEN1=0,VAL="" F  S IEN1=$O(^GMR(120.8,KEY,2,IEN1)) Q:'+IEN1  D
 . S Y=^GMR(120.8,KEY,2,IEN1,0),GMRAVUID=$$GETVUID^GMRAIAL1(50.416,.01,Y_",",1)
 . D ZERO^PSN50P41(Y,,,"GMRAING") ;34 Gets zero node of ingredient entry from 50.416
 . S X=$P(GMRAVUID,U)_HLCM_$$HL7RC^GMRAIAL1($P(^TMP($J,"GMRAING",Y,.01),U))_HLCM_$P(GMRAVUID,U,2),VAL=VAL_X_HLRP ;34
 S VAL=$E(VAL,1,$L(VAL)-1),$P(OUTX,HLFS,5)=VAL,$P(OUTX,HLFS,11)=RSLTSTA
 S OUTX="OBX"_HLFS_OUTX D SAVE^GMRAIAL1
 ;
 ; OBX5 - Drug class
OBX5 G OBX6:'$D(^GMR(120.8,KEY,3))
 S S=S+1,OUTX=1_HLFS_"CE"_HLFS_"DRUG CLASSES"_HLFS
 S IEN1=0,VAL="" F  S IEN1=$O(^GMR(120.8,KEY,3,IEN1)) Q:'+IEN1  D
 . S GMRAIENS=^GMR(120.8,KEY,3,IEN1,0),X=$$CLASS2^PSNAPIS(GMRAIENS)
 . S GMRAVUID=$$GETVUID^GMRAIAL1(50.605,.01,GMRAIENS_",",1)
 . S VAL=$G(VAL)_$P(GMRAVUID,U)_HLCM_$$HL7RC^GMRAIAL1($P(X,U,2))_HLCM_$P(GMRAVUID,U,2)_HLCM_$P(X,U)_HLCM_$$HL7RC^GMRAIAL1($P(X,U,2))_HLCM_$P(SITEPARM,U,6)_"_50.605"_HLRP ;34
 S VAL=$E(VAL,1,$L(VAL)-1),$P(OUTX,HLFS,5)=VAL,$P(OUTX,HLFS,11)=RSLTSTA
 S OUTX="OBX"_HLFS_OUTX D SAVE^GMRAIAL1
 ;
 ; OBX6 - MECHANISM
OBX6 S X=$P(ALRDATA,U,14) G OBX7:X=""
 S S=S+1,OUTX=1_HLFS_"CE"_HLFS_"MECHANISM"_HLFS
 S GMRAVUID=$$GETVUID^GMRAIAL1(120.8,17,X)
 S VAL=$P(GMRAVUID,U)_HLCM_$S(X="A":"ALLERGY",X="P":"PHARMACOLOGIC",X="U":"UNKNOWN",1:"")_HLCM_$P(GMRAVUID,U,2)
 S $P(OUTX,HLFS,5)=VAL,$P(OUTX,HLFS,11)=RSLTSTA
 S OUTX="OBX"_HLFS_OUTX D SAVE^GMRAIAL1
 ;
 ; OBX7 - Reaction
OBX7 S IEN1=0 F  S IEN1=$O(^GMR(120.8,KEY,10,IEN1)) Q:'+IEN1  D  Q:ZTSTOP
 . S S=S+1,OUTX=1_HLFS_"CE"_HLFS_"REACTION"_HLFS
 . S X=^GMR(120.8,KEY,10,IEN1,0),GMRAVUID=""
 . S:$P(X,U,2)'="" GMRAVUID="^L"
 . S:$P(X,U,2)="" $P(X,U,2)=$P($G(^GMRD(120.83,+X,0)),U)
 . S:GMRAVUID'="^L" GMRAVUID=$$GETVUID^GMRAIAL1(120.83,.01,+X_",",1)
 . S VAL=$P(GMRAVUID,U)_HLCM_$$HL7RC^GMRAIAL1($P(X,U,2))_HLCM_$P(GMRAVUID,U,2) ;34
 . S $P(OUTX,HLFS,5)=VAL,$P(OUTX,HLFS,11)=RSLTSTA
 . S $P(OUTX,HLFS,14)=$$TS^VDEFEL($P(X,U,4))
 . S XX=$$XCN200^VDEFEL($P(X,U,3)) ;34
 . F II=2:1:7 S $P(XX,SEPC,II)=$$HL7RC^GMRAIAL1($P(XX,SEPC,II)) ;34
 . S $P(OUTX,HLFS,16)=XX ;34
 . S OUTX="OBX"_HLFS_OUTX D SAVE^GMRAIAL1
 ;
 ; Skip assessment if allergy update
 G OBX9:ALTYPE=1
 ;
 ; OBX8 - Assessment
OBX8 S S=S+1,OUTX=1_HLFS_"CE"_HLFS_"ASSESSMENT"_HLFS
 I +ENTERR=1 S VAL=HLCM_"ENTERED IN ERROR"_HLCM G OBX8A ;34
 S X=+$P(ALRDATA,U,2),GMRAVUID=$$GETVUID^GMRAIAL1(120.86,1,X)
 S VAL=$P(GMRAVUID,U)_HLCM_$S(X=0:"NO KNOWN ALLERGIES",X=1:"YES",1:"")
 S VAL=VAL_HLCM_$P(GMRAVUID,U,2)
OBX8A S $P(OUTX,HLFS,5)=VAL,$P(OUTX,HLFS,11)=$E("FW",1+ENTERR) ;34
 I '+ENTERR S $P(OUTX,HLFS,14)=$$TS^VDEFEL($P(ALRDATA,U,4))
 I '+ENTERR D  ;Block added in 34
 . S XX=$$XCN200^VDEFEL($P(ALRDATA,U,3))
 . F II=2:1:7 S $P(XX,SEPC,II)=$$HL7RC^GMRAIAL1($P(XX,SEPC,II))
 . S $P(OUTX,HLFS,16)=XX
 S OUTX="OBX"_HLFS_OUTX D SAVE^GMRAIAL1
 ;
 ; Done if assessment
 G RETURN:ALTYPE=2
 ;
 ; OBX9 - Comments
 ; One OBX for each comment
OBX9 S IEN=0 F  S IEN=$O(^GMR(120.8,KEY,26,IEN)) Q:'+IEN  D
 . S ALRDATA=$G(^GMR(120.8,KEY,26,IEN,0)) G RETURN:ALRDATA=""
 . ; Set up the static fields
 . S X1=1_HLFS_"TX"_HLFS_"COMMENT"_HLFS
 . S XX=$$XCN200^VDEFEL($P(ALRDATA,U,2)) ;34
 . F II=2:1:7 S $P(XX,SEPC,II)=$$HL7RC^GMRAIAL1($P(XX,SEPC,II)) ;34
 . S $P(X1,HLFS,11)=RSLTSTA,$P(X1,HLFS,16)=XX ;34
 . S $P(X1,HLFS,19)=$$TS^VDEFEL($P(ALRDATA,U)),X=$P(ALRDATA,U,3)
 . S GMRAVUID=$$GETVUID^GMRAIAL1(120.826,1.5,X)
 . S $P(X,HLCM,2)=$S(X="V":"VERIFIED",X="O":"OBSERVED",X="E":"ERRORED",1:"")
 . S $P(X,HLCM)=+GMRAVUID,$P(X,HLCM,3)=$P(GMRAVUID,U,2)
 . S $P(X1,HLFS,17)=X
 . ;
 . ; A comment may be more than one line
 . S IEN1=0,VAL="" F  S IEN1=$O(^GMR(120.8,KEY,26,IEN,2,IEN1)) Q:'+IEN1  D
 . . S X=$$HL7RC^GMRAIAL1(^GMR(120.8,KEY,26,IEN,2,IEN1,0)),VAL=VAL_X_HLRP
 . S:$E(VAL,$L(VAL))=HLRP VAL=$E(VAL,1,$L(VAL)-1)
 . S OUTX=X1,$P(OUTX,HLFS,5)=VAL
 . S S=S+1,OUTX="OBX"_HLFS_OUTX D SAVE^GMRAIAL1
 ;
 ; Return to GMRAIAL1
RETURN Q

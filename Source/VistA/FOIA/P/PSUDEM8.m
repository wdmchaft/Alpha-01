PSUDEM8 ;BIR/DAM - ICD9 Codes for Inpatient PTF Record Extract ; 20 DEC 2001
 ;;4.0;PHARMACY BENEFITS MANAGEMENT;;MARCH, 2005
 ;
 ;DBIA's
 ; Reference to file 45 supported by DBIA 3511
 ; Reference to file 80 supported by DBIA 10082
 ;
EN ;EN  CALLED FROM PSUDEM7
 D PTFIEN
 Q
 ;
PTFIEN ;$O through ^XTMP("PSU_"_PSUJOB,"PSUIPV" to get all the PTF IEN's
 ;
 S PSUC=0
 F  S PSUC=$O(^XTMP("PSU_"_PSUJOB,"PSUIPV",PSUC)) Q:'PSUC  D
 .D PTF70     ;gather ICD9 data on ^DGPT(D0,70 node
 .D PTFM      ;gather ICD9 data on ^DGPT(D0,"M","AC" node
 .D FIN K ^XTMP("PSU_"_PSUJOB,"PSUTMP3")
 .D EN^PSUDEM9    ;gather CPT data on 2 separate ^DGPT nodes
 Q
 ;
PTF70 ;Find all ICD9 pointers present on ^DGPT(D0,70 node
 ;
 N PSU1,PSU2,PSU3,PSU4,PSU5,PSU6,PSU7,PSU8,PSU9,PSU10,PSU11
 S PSU1=$P($G(^DGPT(PSUC,70)),U,10) S:PSU1="" PSU1="NULL"  ;Ptr 1
 S PSU2=$P($G(^DGPT(PSUC,70)),U,16) S:PSU2="" PSU2="NULL"  ;Ptr 2
 S PSU3=$P($G(^DGPT(PSUC,70)),U,17) S:PSU3="" PSU3="NULL"  ;Ptr 3
 S PSU4=$P($G(^DGPT(PSUC,70)),U,18) S:PSU4="" PSU4="NULL"  ;Ptr 4
 S PSU5=$P($G(^DGPT(PSUC,70)),U,19) S:PSU5="" PSU5="NULL"  ;Ptr 5
 S PSU6=$P($G(^DGPT(PSUC,70)),U,20) S:PSU6="" PSU6="NULL"  ;Ptr 6
 S PSU7=$P($G(^DGPT(PSUC,70)),U,21) S:PSU7="" PSU7="NULL"  ;Ptr 7
 S PSU8=$P($G(^DGPT(PSUC,70)),U,22) S:PSU8="" PSU8="NULL"  ;Ptr 8
 S PSU9=$P($G(^DGPT(PSUC,70)),U,23) S:PSU9="" PSU9="NULL"  ;Ptr 9
 S PSU10=$P($G(^DGPT(PSUC,70)),U,24) S:PSU10="" PSU10="NULL"  ;Ptr 10
 S PSU11=$P($G(^DGPT(PSUC,70)),U,11) S:PSU11="" PSU11="NULL"  ;Ptr 11
 D ICD91
 Q
 ;
ICD91 ;Find ICD9 codes from pointer on ^DGPT(D0,70 node and place in
 ;an array
 ;
 N PSUID1,PSUID2,PSUID3,PSUID4,PSUID5,PSUID6,PSUID7,PSUID8,PSUID9
 N PSUID10,PSUID11
 S:PSU1'["N" PSUID1=$P($G(^ICD9(PSU1,0)),U) D
 .I $D(PSUID1) S ^XTMP("PSU_"_PSUJOB,"PSUTMP3",PSUC,1,PSUID1)=""   ;1ST ICD9 CODE
 S:PSU2'["N" PSUID2=$P($G(^ICD9(PSU2,0)),U) D
 .I $D(PSUID2) S ^XTMP("PSU_"_PSUJOB,"PSUTMP3",PSUC,2,PSUID2)=""   ;2ND ICD9 CODE
 S:PSU3'["N" PSUID3=$P($G(^ICD9(PSU3,0)),U) D
 .I $D(PSUID3) S ^XTMP("PSU_"_PSUJOB,"PSUTMP3",PSUC,3,PSUID3)=""   ;3rd ICD9 CODE
 S:PSU4'["N" PSUID4=$P($G(^ICD9(PSU4,0)),U) D
 .I $D(PSUID4) S ^XTMP("PSU_"_PSUJOB,"PSUTMP3",PSUC,4,PSUID4)=""   ;4th ICD9 CODE
 S:PSU5'["N" PSUID5=$P($G(^ICD9(PSU5,0)),U) D
 .I $D(PSUID5) S ^XTMP("PSU_"_PSUJOB,"PSUTMP3",PSUC,5,PSUID5)=""   ;5th ICD9 CODE
 S:PSU6'["N" PSUID6=$P($G(^ICD9(PSU6,0)),U) D
 .I $D(PSUID6) S ^XTMP("PSU_"_PSUJOB,"PSUTMP3",PSUC,6,PSUID6)=""   ;6th ICD9 CODE
 S:PSU7'["N" PSUID7=$P($G(^ICD9(PSU7,0)),U) D
 .I $D(PSUID7) S ^XTMP("PSU_"_PSUJOB,"PSUTMP3",PSUC,7,PSUID7)=""   ;7th ICD9 CODE
 S:PSU8'["N" PSUID8=$P($G(^ICD9(PSU8,0)),U) D
 .I $D(PSUID8) S ^XTMP("PSU_"_PSUJOB,"PSUTMP3",PSUC,8,PSUID8)=""    ;8th ICD9 CODE
 S:PSU9'["N" PSUID9=$P($G(^ICD9(PSU9,0)),U) D
 .I $D(PSUID9) S ^XTMP("PSU_"_PSUJOB,"PSUTMP3",PSUC,9,PSUID9)=""   ;9th ICD9 CODE
 S:PSU10'["N" PSUID10=$P($G(^ICD9(PSU10,0)),U) D
 .I $D(PSUID10) S ^XTMP("PSU_"_PSUJOB,"PSUTMP3",PSUC,10,PSUID10)=""  ;10th ICD9 CODE
 S:PSU11'["N" PSUID11=$P($G(^ICD9(PSU11,0)),U) D
 .I $D(PSUID11) S ^XTMP("PSU_"_PSUJOB,"PSUTMP3",PSUC,11,PSUID11)=""  ;11th ICD9 CODE
 Q
 ;
PTFM ;
 S PSUCD=0
 S I=12
 F  S PSUCD=$O(^DGPT(PSUC,"M","AC",PSUCD)) Q:'PSUCD  D
 .I PSUCD="" S PSUCD="N"
 .N PSUIDT
 .S PSUIDT=$P($G(^ICD9(PSUCD,0)),U) D
 ..S ^XTMP("PSU_"_PSUJOB,"PSUTMP3",PSUC,I,PSUIDT)=""
 ..D DEL
 ..S I=I+1
 Q
 ;
DEL ;Delete duplicates
 ;
 F N=1:1:10 I $D(^XTMP("PSU_"_PSUJOB,"PSUTMP3",PSUC,N,PSUIDT)) D
 .K ^XTMP("PSU_"_PSUJOB,"PSUTMP3",PSUC,I,PSUIDT)
 Q
 ;
FIN ;$O through array, and set codes into the Inpatient Record 
 ;global ^XTMP("PSU_"_PSUJOB,"PSUIPV"
 ;
 S T=0,N=8
 F  S T=$O(^XTMP("PSU_"_PSUJOB,"PSUTMP3",PSUC,T)) Q:'T  Q:N=29  D
 .S PSUIDF=0
 .F  S PSUIDF=$O(^XTMP("PSU_"_PSUJOB,"PSUTMP3",PSUC,T,PSUIDF)) Q:'PSUIDF  D
 ..S $P(^XTMP("PSU_"_PSUJOB,"PSUIPV",PSUC),U,N)=PSUIDF
 ..S N=N+1
 ;
 F N=8:1:28 I '$P(^XTMP("PSU_"_PSUJOB,"PSUIPV",PSUC),U,N) D
 .S $P(^XTMP("PSU_"_PSUJOB,"PSUIPV",PSUC),U,N)=""    ;Set unfilled pieces to null
 Q

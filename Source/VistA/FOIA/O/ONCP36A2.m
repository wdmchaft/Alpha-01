ONCP36A2 ;HINES OIFO/GWB-POST-INSTALL ROUTINE FOR PATCH ONC*2.11*36
 ;;2.11;ONCOLOGY;**36**;Mar 07, 1995
 ;
 D  Q
 .D  Q
 ..I TOP=67619 D  D SPP Q
 ...I SPP>9,SPP<18 S FORDS=18
 ...I SPPAF>9,SPP<18 S FORDSAF=18
 ...I SPP=40 S FORDS=10
 ...I SPPAF=40 S FORDSAF=10
 ...S SUB=0 F  S SUB=$O(SUBTX(SUB)) Q:SUB'>0  D  D SUBTX
 ....S FORDSUB=""
 ....I $P(SUBTX(SUB),U,1)>9,$P(SUBTX(SUB),U,1)<18 S FORDSUB=18 Q
 ....I $P(SUBTX(SUB),U,1)=40 S FORDSUB=10 Q
 ..
 ..I TOP>67619,TOP<67630 D  D SPP Q
 ...I SPP=10 S FORDS=9
 ...I SPPAF=10 S FORDSAF=9
 ...S SUB=0 F  S SUB=$O(SUBTX(SUB)) Q:SUB'>0  D  D SUBTX
 ....S FORDSUB=""
 ....I $P(SUBTX(SUB),U,1)=10 S FORDSUB=9 Q
 ..
 ..I TOP>67669,TOP<67680 D  D SPP Q
 ...I SPP=60,RR=1 S FORDS=28
 ...I SPPAF=60,RR=1 S FORDSAF=28
 ...I SPP=60,RR=2 S FORDS=29
 ...I SPPAF=60,RR=2 S FORDSAF=29
 ...S SUB=0 F  S SUB=$O(SUBTX(SUB)) Q:SUB'>0  D  D SUBTX
 ....S FORDSUB=""
 ....I $P(SUBTX(SUB),U,1)=60,$P(SUBTX(SUB),U,2)=1 S FORDSUB=28 Q
 ....I $P(SUBTX(SUB),U,1)=60,$P(SUBTX(SUB),U,2)=2 S FORDSUB=29 Q
 ..
 ..I TOP>67699,TOP<67730 D  D SPP Q
 ...I SPP>20,SPP<33 S FORDS=3
 ...I SPPAF>20,SPPAF<33 S FORDSAF=3
 ...I SPP>40,SPP<44 S FORDS=10
 ...I SPPAF>40,SPPAF<44 S FORDSAF=10
 ...I (SPP=50)!(SPP=60) S FORDS=18
 ...I (SPPAF=50)!(SPPAF=60) S FORDSAF=18
 ...S SUB=0 F  S SUB=$O(SUBTX(SUB)) Q:SUB'>0  D  D SUBTX
 ....S FORDSUB=""
 ....I $P(SUBTX(SUB),U,1)>20,$P(SUBTX(SUB),U,1)<33 S FORDSUB=3 Q
 ....I $P(SUBTX(SUB),U,1)>40,$P(SUBTX(SUB),U,1)<44 S FORDSUB=10 Q
 ....I $P(SUBTX(SUB),U,1)=50 S FORDSUB=18 Q
 ....I $P(SUBTX(SUB),U,1)=60 S FORDSUB=18 Q
 ..
 ..I TOP=67739 D  D SPP Q
 ...I SPP=10 S FORDS=16
 ...I SPPAF=10 S FORDSAF=16
 ...I SPP=11 S FORDS=17
 ...I SPPAF=11 S FORDSAF=17
 ...I SPP=12 S FORDS=18
 ...I SPPAF=12 S FORDSAF=18
 ...S SUB=0 F  S SUB=$O(SUBTX(SUB)) Q:SUB'>0  D  D SUBTX
 ....S FORDSUB=""
 ....I $P(SUBTX(SUB),U,1)=10 S FORDSUB=16 Q
 ....I $P(SUBTX(SUB),U,1)=11 S FORDSUB=17 Q
 ....I $P(SUBTX(SUB),U,1)=12 S FORDSUB=18 Q
 ..
 ..I TOP>67769,TOP<67780 D  D SPP Q
 ...I SPP=10 S FORDS=2
 ...I SPPAF=10 S FORDSAF=2
 ...I SPP=30 S FORDS=5
 ...I SPPAF=30 S FORDSAF=5
 ...I SPP=31 S FORDS=6
 ...I SPPAF=31 S FORDSAF=6
 ...I SPP=32 S FORDS=7
 ...I SPPAF=32 S FORDSAF=7
 ...I SPP=40 S FORDS=8
 ...I SPPAF=40 S FORDSAF=8
 ...I SPP=41 S FORDS=9
 ...I SPPAF=41 S FORDSAF=9
 ...I SPP=42 S FORDS=10
 ...I SPPAF=42 S FORDSAF=10
 ...I SPP=50 S FORDS=11
 ...I SPPAF=50 S FORDSAF=11
 ...I SPP=51 S FORDS=12
 ...I SPPAF=51 S FORDSAF=12
 ...I SPP=52 S FORDS=13
 ...I SPPAF=52 S FORDSAF=13
 ...I SPP=60 S FORDS=14
 ...I SPPAF=60 S FORDSAF=14
 ...I SPP=61 S FORDS=15
 ...I SPPAF=61 S FORDSAF=15
 ...I SPP=62 S FORDS=16
 ...I SPPAF=62 S FORDSAF=16
 ...I SPP=90 S FORDS=17
 ...I SPPAF=90 S FORDSAF=17
 ...I SPP=99 S FORDS=18
 ...I SPPAF=99 S FORDSAF=18
 ...S SUB=0 F  S SUB=$O(SUBTX(SUB)) Q:SUB'>0  D  D SUBTX
 ....S FORDSUB=""
 ....I $P(SUBTX(SUB),U,1)=10 S FORDSUB=2 Q
 ....I $P(SUBTX(SUB),U,1)=30 S FORDSUB=5 Q
 ....I $P(SUBTX(SUB),U,1)=31 S FORDSUB=6 Q
 ....I $P(SUBTX(SUB),U,1)=32 S FORDSUB=7 Q
 ....I $P(SUBTX(SUB),U,1)=40 S FORDSUB=8 Q
 ....I $P(SUBTX(SUB),U,1)=41 S FORDSUB=9 Q
 ....I $P(SUBTX(SUB),U,1)=42 S FORDSUB=10 Q
 ....I $P(SUBTX(SUB),U,1)=50 S FORDSUB=11 Q
 ....I $P(SUBTX(SUB),U,1)=51 S FORDSUB=12 Q
 ....I $P(SUBTX(SUB),U,1)=52 S FORDSUB=13 Q
 ....I $P(SUBTX(SUB),U,1)=60 S FORDSUB=14 Q
 ....I $P(SUBTX(SUB),U,1)=61 S FORDSUB=15 Q
 ....I $P(SUBTX(SUB),U,1)=62 S FORDSUB=16 Q
 ....I $P(SUBTX(SUB),U,1)=90 S FORDSUB=17 Q
 ....I $P(SUBTX(SUB),U,1)=99 S FORDSUB=18 Q
 ..
 ..I TOP>67759,TOP<67769 D  D SPP Q
 ...I SPP'="" S FORDS=1
 ...I SPPAF'="" S FORDSAF=1
 ...S SUB=0 F  S SUB=$O(SUBTX(SUB)) Q:SUB'>0  D  D SUBTX
 ....I $P(SUBTX(SUB),U,1)'="" S FORDSUB=1
 ..
 ..I TOP=67809 D  D SPP Q
 ...I SPP'="" S FORDS=1
 ...I SPPAF'="" S FORDSAF=1
 ...S SUB=0 F  S SUB=$O(SUBTX(SUB)) Q:SUB'>0  D  D SUBTX
 ....I $P(SUBTX(SUB),U,1)'="" S FORDSUB=1
 ..D SPP Q
 ;
SPP I FORDS'="" S $P(^ONCO(165.5,IEN,3.1),U,29)=FORDS
 E  S $P(^ONCO(165.5,IEN,3.1),U,29)=SPPPNT
 I FORDSAF'="" S $P(^ONCO(165.5,IEN,3.1),U,30)=FORDSAF
 E   S $P(^ONCO(165.5,IEN,3.1),U,30)=SPPAFPNT
 Q
 ;
SUBTX S:FORDSUB'="" $P(^ONCO(165.5,IEN,4,SUB,0),U,4)=FORDSUB
 Q

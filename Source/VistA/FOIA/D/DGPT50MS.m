DGPT50MS ;ALB/MTC - 501 Edits Cont ; 16 NOV 92
 ;;5.3;Registration;**142,729**;Aug 13, 1993;Build 59
 ;
 ; Edits for legionnaire's, suicide, drug and psych indicators
 ;
LEG ;
 ;I ((+DGPTMD1=482.8)!(+DGPTMD2=482.8)!(+DGPTMD3=482.8)!(+DGPTMD3=482.8)!(+DGPTMD4=482.8)!(+DGPTMD5=482.8))&("12"'[DGPTMLG) S DGPTERC=531 Q
 Q
SUI ;
 N I,DGINACT
 S DGPTMSX=0 F I=1:1:5 I ($E(@("DGPTMD"_I),1,3)="E95")&("012345678"[$E(@("DGPTMD"_I),4)) S DGPTMSX=1 Q:DGPTMSX
 Q:'DGPTMSX
 I '$D(DGSCDT) D DC
 S DGINACT=$$GET1^DIQ(45.88,"2,",.03,"I")
 I DGINACT]"",$D(DGSCDT) Q:DGSCDT>DGINACT
 I "123"'[DGPTMSU S DGPTERC=532 Q
 Q
DRUG ;
 N I,DGINACT
 S DGPTMSX=0 F I=1:1:5 I ($E(@("DGPTMD"_I),1,4)="304.")&("013456"[$E(@("DGPTMD"_I),5))&("0123"[$E(@("DGPTMD"_I),6)) S DGPTMSX=1 Q:DGPTMSX
 G:DGPTMSX DRG1
 S DGPTMSX=0 F I=1:1:5 I ($E(@("DGPTMD"_I),1,4)="305.")&("234579"[$E(@("DGPTMD"_I),5))&("0123"[$E(@("DGPTMD"_I),6)) S DGPTMSX=1 Q:DGPTMSX
DRG1 ;
 Q:'DGPTMSX
 I '$D(DGSCDT) D DC
 S DGINACT=$$GET1^DIQ(45.88,"4,",.03,"I")
 I DGINACT]"",$D(DGSCDT) Q:DGSCDT>DGINACT
 I DGPTMDG'?1A3N S DGPTERC=533 Q
 I $E(DGPTMDG,1)'="A" S DGPTERC=533 Q
 I ($E(DGPTMDG,2,4))<1!($E(DGPTMDG,2,4)>18) S DGPTERC=533 Q
 Q
AXIV ;
 N I
 S DGPTMSX=0 F I=1:1:5 I ($E(@("DGPTMD"_I),1,3)'<290)&($E(@("DGPTMD"_I),1,3)<320) S DGPTMSX=1 Q:DGPTMSX
 Q:'DGPTMSX
 I "0123456"'[DGPTMXIV S DGPTERC=534 Q
 Q
AXV1 ;
 Q:'DGPTMSX
 I (DGPTMXV1<1)!(DGPTMXV1>90) S DGPTERC=535 Q
 Q
AXV2 ;
 Q:'DGPTMSX
 Q:DGPTMXV2="  "
 I (DGPTMXV2<1)!(DGPTMXV2>90) S DGPTERC=535 Q
 Q
SRVC ;
 I " 12"'[DGPT50SR S DGPTERC=530
 Q
DC ;find discharge date
 S DGSCDT=$S('$D(^DGPT(PTF,70)):DT,^(70):+^(70),1:DT)
 Q

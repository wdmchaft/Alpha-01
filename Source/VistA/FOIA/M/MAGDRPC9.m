MAGDRPC9 ;WOIFO/EdM - Imaging RPCs ; 05 Sep 2008 12:55 PM
 ;;3.0;IMAGING;**50,54,53**;Mar 19, 2002;Build 1719;Apr 28, 2010
 ;; Per VHA Directive 2004-038, this routine should not be modified.
 ;; +---------------------------------------------------------------+
 ;; | Property of the US Government.                                |
 ;; | No permission to copy or redistribute this software is given. |
 ;; | Use of unreleased versions of this software requires the user |
 ;; | to execute a written test agreement with the VistA Imaging    |
 ;; | Development Office of the Department of Veterans Affairs,     |
 ;; | telephone (301) 734-0100.                                     |
 ;; | The Food and Drug Administration classifies this software as  |
 ;; | a medical device.  As such, it may not be changed in any way. |
 ;; | Modifications to this software may result in an adulterated   |
 ;; | medical device under 21CFR820, the use of which is considered |
 ;; | to be a violation of US Federal Statutes.                     |
 ;; +---------------------------------------------------------------+
 ;;
 Q
 ;
UIDROOT(OUT) ; RPC = MAG DICOM GET UID ROOT
 S OUT=$G(^MAGD(2006.15,1,"UID ROOT"))
 Q
 ;
NEWUID(OUT,OLD,NEW,IMAGE) ; RPC = MAG NEW SOP INSTANCE UID
 N D0,L,X
 S IMAGE=+$G(IMAGE),OLD=$G(OLD)
 S:$G(NEW)="" NEW=OLD
 S D0=0 S:OLD'="" D0=$O(^MAG(2005,"P",OLD,""))
 I IMAGE,D0,IMAGE'=D0 S OUT="-1,UID cannot belong to multiple images ("_IMAGE_"/"_D0_")" Q
 I IMAGE,'D0 S D0=IMAGE
 I 'D0 S OUT="-2,Cannot find image with UID "_OLD Q
 S OUT=$P($G(^MAG(2005,D0,"SOP")),"^",2) Q:OUT'=""
 S L=$L(NEW,".")-1,X=$P(NEW,".",L+1),L=$P(NEW,".",1,L)_"."
 L +^MAG(2005,"P"):1E9 ; Background process MUST wait
 S OUT="" F  D  Q:OUT'=""
 . S OUT=L_X
 . I $L(OUT)>64 S OUT="-3,Cannot use "_NEW_" to create valid UID" Q
 . I $D(^MAG(2005,"P",OUT)) S OUT="",X=X+1 Q
 . S $P(^MAG(2005,D0,"SOP"),"^",2)=OUT
 . S ^MAG(2005,"P",OUT,D0)=1
 . Q
 L -^MAG(2005,"P")
 Q
 ;
QRNEWUID(IMAGE) ; Get updated UID for Query/Retrieve
 N DATE,DH,FAIL,I,OLD,OUT,NEW,LASTUID,NEXTUID,TIME,X,Y
 S IMAGE=+$G(IMAGE)
 S NEW=$P($G(^MAG(2005,IMAGE,"SOP")),"^",2)
 Q:NEW'="" NEW  ; There already is a new UID
 ; Generate the next UID using date/time and counter
 L +^MAGDICOM(2006.563,1,"MACHINE ID"):1E9 ; Background process must wait
 S LASTUID=$G(^MAGDICOM(2006.563,1,"LAST UID"))
 ; Can't use D NOW^XLFDT to set DH because it is incorrect at midnight
 S DH=$H,X=$$HTFM^XLFDT(DH,1),DATE=X+17000000
 S X=$P(DH,",",2) D
 . N H,M,S
 . S H=X\3600,M=X\60#60,S=X#60
 . S TIME=H*100+M*100+S
 . Q
 S NEXTUID=$G(^MAGD(2006.15,1,"UID ROOT"))
 I NEXTUID="" S $EC=",13:No UID Root defined - Run INIT^MAGDRUID," ; Fatal Error
 ; UID type = 7, Machine ID = 0
 S NEXTUID=NEXTUID_".1.7."_(+$G(DUZ(2)))_".0."_DATE_"."_TIME_".0"
 ; Remove leading 0s from UID components
 F I=1:1:$L(NEXTUID,".") S $P(NEXTUID,".",I)=+$P(NEXTUID,".",I)
 I $P(NEXTUID,".",1,10)=$P(LASTUID,".",1,10) D
 . S NEXTUID=LASTUID
 . S $P(NEXTUID,".",11)=$P(NEXTUID,".",11)+1
 . Q
 S ^MAGDICOM(2006.563,1,"LAST UID")=NEXTUID
 L -^MAGDICOM(2006.563,1,"MACHINE ID")
 S OLD=$P($G(^MAG(2005,IMAGE,"PACS")),"^",1)
 D NEWUID(.OUT,OLD,NEXTUID,IMAGE) ; Store the new UID with the image
 Q OUT
 ;
NEXT(OUT,SEED,DIR) ; RPC = MAG RAD GET NEXT RPT BY DATE
 N D2,DFN,EXAMDATE,NAME
 ;
 ; ^RADPT(DFN,"DT",D1,"P",D2,0) = Data, $P(17) = pointer to report
 ; ^RADPT("AR",9999999.9999-D1,DFN,D1)="" ; IA # 65
 ;
 ; OUT = report_pointer ^ ExamDate ^ Patient ^ D2
 ;
 S SEED=$G(SEED),DIR=$S($G(DIR)<0:-1,1:1) ; default is ascending order
 S EXAMDATE=$P(SEED,"^",1),DFN=$P(SEED,"^",2),D2=$P(SEED,"^",3)
 S OUT=0 F  D  Q:OUT
 . I EXAMDATE="" S EXAMDATE=$O(^RADPT("AR",""),DIR),DFN="" ; IA # 65
 . I EXAMDATE="" S OUT=-1 Q
 . I DFN="" S DFN=$O(^RADPT("AR",EXAMDATE,""),DIR) ; IA # 65
 . I DFN="" S EXAMDATE=$O(^RADPT("AR",EXAMDATE),DIR),D2="" Q  ; IA # 65
 . S:'D2 D2=$S(DIR>0:0,1:" ")
 . S D2=$O(^RADPT(DFN,"DT",9999999.9999-EXAMDATE,"P",D2),DIR) ; IA # 1172
 . I 'D2 D  Q
 . . S DFN=$O(^RADPT("AR",EXAMDATE,DFN),DIR),D2="" ; IA # 65
 . . I 'DFN D
 . . . S EXAMDATE=$O(^RADPT("AR",EXAMDATE),DIR),DFN="" ; IA # 65
 . . . I EXAMDATE="" S OUT=-1
 . . . Q
 . . Q
 . S OUT=$P($G(^RADPT(DFN,"DT",9999999.9999-EXAMDATE,"P",D2,0)),"^",17) ; IA # 1172
 . S:OUT OUT=OUT_"^"_EXAMDATE_"^"_DFN_"^"_D2
 . Q
 Q
 ;
NXTPTRPT(OUT,DFN,RARPT1,DIR) ; RPC = MAG RAD GET NEXT RPT BY PT
 S DFN=$G(DFN)
 I 'DFN S OUT="-1,Patient DFN not passed" Q
 I '$D(^RARPT("C",DFN)) S OUT="-2,Patient does not have any radiology reports" Q  ; IA # 2442
 S RARPT1=$G(RARPT1),DIR=$S($G(DIR)<0:-1,1:1) ; default is ascending order
 S OUT=$O(^RARPT("C",DFN,RARPT1),DIR) ; IA # 2442
 Q
 ;
GETICN(OUT,DFN) ; RPC = MAG DICOM GET ICN
 S OUT=$$GETICN^MPIF001(DFN)
 Q
 ;
CLEAN ; Overflow from MAGDRPC4
 N STUID
 S S0=$P(SENT(I),"^",1),S1=$P(SENT(I),"^",2)
 Q:'$D(^MAGDOUTP(2006.574,S0,1,S1))
 L +^MAGDOUTP(2006.574,S0,1,0):1E9 ; Background process MUST wait
 S X=$G(^MAGDOUTP(2006.574,S0,0)),LOC=$P(X,"^",4),PRI=+$P(X,"^",5)
 S STS=$P($G(^MAGDOUTP(2006.574,S0,1,S1,0)),"^",2)
 K ^MAGDOUTP(2006.574,S0,1,S1)
 I LOC'="",STS'="" K ^MAGDOUTP(2006.574,"STS",LOC,PRI,STS,S0,S1)
 S X=$G(^MAGDOUTP(2006.574,S0,1,0))
 S $P(X,"^",4)=$P(X,"^",4)-1
 S ^MAGDOUTP(2006.574,S0,1,0)=X
 L -^MAGDOUTP(2006.574,S0,1,0)
 Q:$O(^MAGDOUTP(2006.574,S0,1,0))
 L +^MAGDOUTP(2006.574,0):1E9 ; Background process MUST wait
 S STUID=$G(^MAGDOUTP(2006.574,S0,2))
 K ^MAGDOUTP(2006.574,S0)
 K:STUID'="" ^MAGDOUTP(2006.574,"STUDY",STUID)
 S X=$G(^MAGDOUTP(2006.574,0))
 S $P(X,"^",4)=$P(X,"^",4)-1
 S ^MAGDOUTP(2006.574,0)=X
 L -^MAGDOUTP(2006.574,0)
 Q
 ;
IENLOOK ; Overflow from MAGDRPC4
 ; lookup image by the IEN
 N ACNUMB,D0,DFN,GROUPIEN,MODIFIER,P,PROCNAME,STUDYDAT,X,Y
 S NUMBER=+$P(NUMBER,"`",2)
 ; patient safety checks
 D CHK^MAGGSQI(.X,NUMBER) I +$G(X(0))'=1 D  Q
 . S OUT(1)="-9,"_$P(X(0),"^",2,999)
 . Q
 S GROUPIEN=$P($G(^MAG(2005,NUMBER,0)),"^",10)
 I GROUPIEN D CHK^MAGGSQI(.X,GROUPIEN) I +$G(X(0))'=1 D  Q
 . S OUT(1)="-10,Group #"_GROUPIEN_": "_$P(X(0),"^",2,999)
 . Q
 ;
 S X=$G(^MAG(2005,NUMBER,2)),P=$P(X,"^",6),D0=$P(X,"^",7)
 I 'P!'D0 D  ; get parent from group
 . S:GROUPIEN X=$G(^MAG(2005,GROUPIEN,2)),P=$P(X,"^",6),D0=$P(X,"^",7)
 . Q
 ;
 S OUT(2)=P_"^"_D0_"^"_NUMBER_"^" ; result w/o Accession Number
 I 'P!'D0 S OUT(1)="-6,Warning - Parent file entry is not present - no Accession Number."
 E  I P=74 D
 . N DATETIME,I,INFO,PROC,RADPT0,RADPT1,RADPT2,RADPT3,RARPT0
 . S RARPT0=$G(^RARPT(D0,0)) ; IA # 1171
 . I $T(ACCRPT^RAAPI)'="" D  ; requires RA*5.0*47
 . . S X=$$ACCRPT^RAAPI(D0,.INFO)
 . . I X<0 S OUT(1)="-11,Radiology Problem: "_X Q
 . . S ACNUMB=INFO(1)
 . . Q
 . E  S ACNUMB=$P(RARPT0,"^",1)
 . S RADPT1=$P(RARPT0,"^",2),DATETIME=$P(RARPT0,"^",3)
 . S RADPT2=9999999.9999-DATETIME,RADPT3=1
 . S RADPT0=$G(^RADPT(RADPT1,"DT",RADPT2,"P",RADPT3,0))
 . S PROCNAME=$$GET1^DIQ(71,$P(RADPT0,"^",2),.01)
 . S STUDYDAT=17000000+(DATETIME\1)
 . S MODIFIER=""
 . S I=0 F  S I=$O(^RADPT(RADPT1,"DT",RADPT2,"P",RADPT3,"M",I)) Q:'I  D
 . . S X=^RADPT(RADPT1,"DT",RADPT2,"P",RADPT3,"M",I,0)
 . . S:I>1 MODIFIER=MODIFIER_", " S MODIFIER=MODIFIER_$$GET1^DIQ(71.2,X,.01)
 . . Q
 . S X=P_"^"_D0_"^"_NUMBER_"^"_ACNUMB_"^"_STUDYDAT_"^"_PROCNAME_"^"_MODIFIER
 . S OUT(1)=1,OUT(2)=X
 . Q
 E  I P=8925 D
 . N GMRCIEN
 . ; get pointer from TIU to consult request
 . S X=$$GET1^DIQ(8925,D0,1405,"I") ; IA ???
 . I $P(X,";",2)="GMR(123," D
 . . S GMRCIEN=$P(X,";"),ACNUMB="GMRC-"_GMRCIEN
 . . S STUDYDAT=17000000+($$GET1^DIQ(123,GMRCIEN,.01,"I")\1)
 . . S PROCNAME=$$GET1^DIQ(123,GMRCIEN,1) ; TO SERVICE
 . . S MODIFIER=$$GET1^DIQ(123,GMRCIEN,4) ; PROCEDURE
 . . S X=P_"^"_D0_"^"_NUMBER_"^"_ACNUMB_"^"_STUDYDAT_"^"_PROCNAME_"^"_MODIFIER
 . . S OUT(1)=1,OUT(2)=X
 . . Q
 . E  S OUT(1)="-8,Problem with parent file "_P_", internal entry number "_D0_" - no Accession Number."
 . Q
 E  S OUT(1)="-7,Parent file "_P_" not yet supported - no Accession Number."
 Q

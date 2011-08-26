YTAPI ;ALB/ASF- PSYCH TEST API ;2/27/04  15:44
 ;;5.01;MENTAL HEALTH;**53,71,76,77**;Dec 30, 1994
PARSE(YS) ; -- array parsing
 S DFN=$G(YS("DFN"))
 S YSCODE=$G(YS("CODE"))
 S:YSCODE?1N.N YSCODE=$P($G(^YTT(601,YSCODE,0),"ERROR"),U)
 S YSADATE=$G(YS("ADATE")) S X=YSADATE,%DT="T" D ^%DT S YSADATE=Y
 S YSSCALE=$G(YS("SCALE"))
 S YSBEG=$G(YS("BEGIN")) S:YSBEG="" YSBEG="01/01/1970" S X=YSBEG,%DT="T" D ^%DT S YSBEG=Y ;ASF 1/30/04
 S YSEND=$G(YS("END")) S:YSEND="" YSEND="01/01/2099" S X=YSEND,%DT="T" D ^%DT S YSEND=Y ;ASF 1/30/04
 S YSLIMIT=$G(YS("LIMIT"),999)
 S YSSTAFF=$G(YS("STAFF"))
 S R1=$G(YS("R1"))
 S R2=$G(YS("R2"))
 S R3=$G(YS("R3"))
 K %DT
 Q
LISTALL(YSDATA,YS) ;
 N N,N2,N4,YSAA,I,II,DFN,YSCODE,YSADATE,YSSCALE,YSBED,YSEND,YSJJ,YSLIMIT
 N IFN,R3,SFN1,SFN2,YSBEG,YSCK,YSDFN,YSED,YSIFN,YSINUM,YSITEM,YSN2,YSNODE,YSPRIV,YSQT,YSR,YSSONE,YSSTAFF,YSTYPE
 D PARSE(.YS)
 I DFN'>0!('$D(^DPT(DFN))) S YSDATA(1)="[ERROR]",YSDATA(2)="BAD DFN" Q
 K YSDATA S YSDATA(1)="[DATA]"
 S N=0 F  S N=$O(^YTD(601.2,DFN,1,N)) Q:N'>0  D
 . I $P(^YTT(601,N,0),U,9)="I"  QUIT
 . I $D(^YTT(601,N)) S N2=YSBEG-.1 F  S N2=$O(^YTD(601.2,DFN,1,N,1,N2)) Q:N2'>0!(N2>YSEND)  D
 .. S X=^YTT(601,N,0),N4=$P(X,U)
 .. I N4="MMPI",$D(^YTD(601.2,DFN,1,N,1,N2,99)),^(99)="MMPIR" S N4="MMPR"
 .. S YSPRIV="P" S:$P(X,U,10)="Y" YSPRIV="E" S:$P(X,U,9)="I" YSPRIV="E" ;ASF 4/18/01
 .. S YSAA(9999999-N2,N4)=YSPRIV_U_N ;ASF 9/9/03
 .. Q
 I YSCODE="GAF" D GAF
 I YSCODE="ASI" D ASI ;ASF 9/9/03
 S I=0,N=1 F  S I=$O(YSAA(I)) Q:I'>0  S II="" F  S II=$O(YSAA(I,II)) Q:II=""  D SET(9999999-I_U_$$FMTE^XLFDT(9999999-I,"5ZD")_U_II_U_YSAA(I,II)) ;ASF 4/18/01
 Q
GAF ;
 N YSJJ,YSDD,X,Y,YSX,YSN
 S YSDD=9999999-YSEND-.00001
 F YSJJ=1:1:YSLIMIT S YSDD=$O(^YSD(627.8,"AX5",DFN,YSDD)) Q:YSDD'>0!(YSDD>(9999999-YSBEG))  D
  . S YSN=0 F  S YSN=$O(^YSD(627.8,"AX5",DFN,YSDD,YSN)) Q:YSN'>0  D
 .. S YSX=$P($G(^YSD(627.8,YSN,60)),U,3)_"^^"_$$EXTERNAL^DILFD(627.8,.04,"",$P($G(^YSD(627.8,YSN,0)),U,4))_U_$G(^YSD(627.8,YSN,80,1,0))
 .. S YSAA(YSDD,"GAF")=9999999-YSDD_"^GAF^"_YSX
 Q
ASI ;
 Q:'$D(^YSTX(604,"C",DFN))
 S IFN="A" F YSJJ=1:1:YSLIMIT S IFN=$O(^YSTX(604,"C",DFN,IFN),-1) Q:IFN'>0  D
 . Q:'$D(^YSTX(604,IFN,.5))  ; no sig
 . S N2=$P($G(^YSTX(604,IFN,0)),U,5)
 . I N2>YSEND!(N2<YSBEG) Q  ;not in range
 . S YSSONE="^^^"
 . S:YSSCALE=1 YSSONE="^Medical^"_$$GET1^DIQ(604,IFN_",",8.12)_U_$$GET1^DIQ(604,IFN_",",.61)
 . S:YSSCALE=2 YSSONE="^Employment^"_$$GET1^DIQ(604,IFN_",",9.34)_U_$$GET1^DIQ(604,IFN_",",.62)
 . S:YSSCALE=3 YSSONE="^Alcohol^"_$$GET1^DIQ(604,IFN_",",11.18)_U_$$GET1^DIQ(604,IFN_",",.63)
 . S:YSSCALE=4 YSSONE="^Drug^"_$$GET1^DIQ(604,IFN_",",11.185)_U_$$GET1^DIQ(604,IFN_",",.635)
 . S:YSSCALE=5 YSSONE="^Legal^"_$$GET1^DIQ(604,IFN_",",14.34)_U_$$GET1^DIQ(604,IFN_",",.64)
 . S:YSSCALE=6 YSSONE="^Family^"_$$GET1^DIQ(604,IFN_",",18.29)_U_$$GET1^DIQ(604,IFN_",",.65)
 . S:YSSCALE=7 YSSONE="^Psychiatric^"_$$GET1^DIQ(604,IFN_",",19.33)_U_$$GET1^DIQ(604,IFN_",",.66)
 . S YSAA(9999999-N2,"ASI")=N2_U_$$FMTE^XLFDT(N2,"5ZD")_YSSONE_U_IFN
 Q
SET(X) ;
 S N=N+1
 S YSDATA(N)=X
 Q
LISTONE(YSDATA,YS) ;
 N YSLIMIT,YSJJ,YSSONE,S,R,N,YSN2,N4,I,II,DFN,YSCODE,YSADATE,YSSCALE,YSBED,YSEND,YSAA
 N IFN,R1,R2,R3,SFN1,SFN2,YSBEG,YSCK,YSDFN,YSED,YSIFN,YSINUM,YSITEM,YSN2,YSNODE,YSPRIV,YSQT,YSR,YSSTAFF,YSTYPE,YSET
 D PARSE(.YS)
 K YSDATA
 I DFN'>0!('$D(^DPT(DFN))) S YSDATA(1)="[ERROR]",YSDATA(2)="BAD DFN" Q
 I '$D(^YTT(601,"B",YSCODE))&(YSCODE'="ASI")&(YSCODE'="GAF") S YSDATA(1)="[ERROR]",YSDATA(2)="INCORRECT TEST CODE" Q
 S YSET=-1 S:YSCODE'="ASI"&(YSCODE'="GAF") YSET=$O(^YTT(601,"B",YSCODE,""))
 S YSDATA(1)="[DATA]"
 I $D(^YTT(601,YSET)) S YSN2=YSEND+.1 F YSJJ=1:1:YSLIMIT S YSN2=$O(^YTD(601.2,DFN,1,YSET,1,YSN2),-1) Q:YSN2'>0!(YSN2<YSBEG)  D
 . K YSSONE S YSSONE=""
 . D PRIV^YTAPI2
 . I YSSCALE'=""&(YSPRIV=1) D
 .. S YSADATE=YSN2
 .. D SCOR1^YTAPI2
 .. D SF^YTAPI2
 .. S YSSCALE=$G(YS("SCALE"))
 . S:$D(YSSCALE)&(YSSCALE'="") YSSONE=$S($D(YSSONE(YSSCALE)):U_$P(YSSONE(YSSCALE),U,2,99),1:"")
 . S YSAA(9999999-YSN2,YSCODE)=YSN2_YSSONE
 . Q
 I YSCODE="ASI" D ASI
 I YSCODE="GAF" D GAF
 S I=0,N=1 F  S I=$O(YSAA(I)) Q:I'>0  S II="" F  S II=$O(YSAA(I,II)) Q:II=""  D
 . S X=$P(YSAA(I,II),U)_U_$$FMTE^XLFDT($P(YSAA(I,II),U),"5ZD")_U_II
 . S:$P(YSAA(I,II),U,2)'="" X=X_U_$P(YSAA(I,II),U,2,9)
 . D SET(X)
 Q

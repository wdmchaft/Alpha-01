YSPP8 ;ALB/ASF-PP PSYCH TESTS ;12/4/91  13:57 ;
 ;;5.01;MENTAL HEALTH;**37**;Dec 30, 1994
 ;
 S YSFHDR="Problem List, Psychological Tests and Interviews <<section 9>>" D ENHD^YSFORM
 ;
ENCE ; Called indirectly from YSCEN31
 ;
 I '$D(^PTX(YSDFN,"HX")) W !,"HISTORY 1: ** NONE ON FILE **"
 I $D(^MR(YSDFN,"PE"))>1 W !,"PHYSICAL: " S M="PE",YSDOT=0 D DT0
 I '$D(^MR(YSDFN,"PE")) W !,"PHYSICAL: ** NONE ON FILE **"
 W !,"PROBLEM LIST : ",$S('$D(^YS(615,YSDFN,"PL")):"** NOT ON FILE **",1:"ON FILE")
 I $D(^GMR(121,"AC",YSDFN)) W !,"PROGRESS NOTES: LAST " S YSDOT="",YSDOT=$O(^GMR(121,"AC",YSDFN,YSDOT)) S YSDOT=9999999-YSDOT D YSDOT
 I '$D(^GMR(121,"AC",YSDFN)) W !,"PROGRESS NOTES: ** NONE ON FILE **"
 I '$D(^YTD(601.2,YSDFN,1,"B")) W !,"NO PSYCHOLOGICAL TESTS OR INTERVIEWS HAVE BEEN COMPLETED",! G END
 E  D TLST
END ;
 K M,N,YSDOT Q:$D(YSNOFORM)  D ENFT^YSFORM:YST,WAIT1^YSUTL:'YST Q
TLST ;
 W !!,"Psychological Tests/Interviews:",! K AI,N
 S N=0 F  S N=$O(^YTD(601.2,YSDFN,1,N)) Q:'N!('$D(^YTT(601,N)))  S N(2)=0 F  S N(2)=$O(^YTD(601.2,YSDFN,1,N,1,N(2))) Q:'N(2)  S X=^YTT(601,N,0),N(4)=$P(X,U) S AI(N(4),N(2))=""
 S N(3)=0,N="" F  S N=$O(AI(N)) Q:N=""  S N(2)=0 F  S N(2)=$O(AI(N,N(2))) Q:'N(2)  W ?(N(3)*16),N,$E("   ",1,6-$L(N)),$$FMTE^XLFDT(N(2),"5ZD") S N(3)=N(3)+1 I '(N(3)#5) W ! S N(3)=0
 Q
 ;
YSDOT ;
 W $$FMTE^XLFDT(YSDOT,"5ZD")," "
 Q
 ;
DTS S YSDOT=0
DT0 S YSDOT=$O(^MR(YSDFN,M,"B",YSDOT)) I YSDOT D YSDOT G DT0
 Q
 ;
DT1 S YSDOT=$O(^PTX(YSDFN,M,YSDOT)) I YSDOT D YSDOT G DT1
 Q
 ;
DT2 S YSDOT=$O(^YS(615,YSDFN,M,"B",YSDOT)) I YSDOT D YSDOT G DT1
 Q

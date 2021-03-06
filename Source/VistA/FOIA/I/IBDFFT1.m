IBDFFT1 ;ALB/MAF - FORMS TRACKING CONTINUED - JUL 6 1995
 ;;3.0;AUTOMATED INFO COLLECTION SYS;**16**;APR 24, 1997
 ;
START ;
 N IBDCNT,IBDCNT1,IBDFTIME
 S (IBDCNT,IBDNKA,IBDCNT1,VALMCNT)=0
 D KILL^VALM10()
 D @(IBDFL)
 N IBDFDV,IBDFCL,IBDNODE,IBDFTMP,IBDFPAT,IBDFPT,IBDFT
 S (IBDFDV,IBDFCL,IBDFPT)=0
 ;
 I $D(VAUTG) D
 .N IBDFGR
 .S IBDFGR=0
 .F IBDFDIV=0:0 S IBDFDV=$O(^TMP("FTRK",$J,IBDFDV)) Q:IBDFDV']""  D:'$D(IBDFDIV(IBDFDV)) HEADER^IBDFFT2 F IBDFGRO=0:0 S IBDFGR=$O(^TMP("FTRK",$J,IBDFDV,IBDFGR)) Q:IBDFGR']""  D
 ..F IBDFCLI=0:0 S IBDFCL=$O(^TMP("FTRK",$J,IBDFDV,IBDFGR,IBDFCL)) Q:IBDFCL']""  D:'$D(IBDFGROP(IBDFDV,IBDFGR)) HEADER2^IBDFFT2 D:'$D(IBDFCLIN(IBDFGR,IBDFCL)) HEADER1^IBDFFT2 D
 ...F IBDFT=0:0 S IBDFT=$O(^TMP("FTRK",$J,IBDFDV,IBDFGR,IBDFCL,IBDFT)) Q:'IBDFT  F IBDFPAT=0:0 S IBDFPT=$O(^TMP("FTRK",$J,IBDFDV,IBDFGR,IBDFCL,IBDFT,IBDFPT)) Q:IBDFPT']""  D
 ....F IBDFIFN=0:0 S IBDFIFN=$O(^TMP("FTRK",$J,IBDFDV,IBDFGR,IBDFCL,IBDFT,IBDFPT,IBDFIFN)) Q:'IBDFIFN  S IBDX="" F  S IBDX=$O(^TMP("FTRK",$J,IBDFDV,IBDFGR,IBDFCL,IBDFT,IBDFPT,IBDFIFN,IBDX)) Q:IBDX=""  S IBDFTMP=^(IBDX) D SETARR
 I '$D(VAUTG) D
 .F IBDFDIV=0:0 S IBDFDV=$O(^TMP("FTRK",$J,IBDFDV)) Q:IBDFDV']""  D:'$D(IBDFDIV(IBDFDV)) HEADER^IBDFFT2 F IBDFCLI=0:0 S IBDFCL=$O(^TMP("FTRK",$J,IBDFDV,IBDFCL)) Q:IBDFCL']""  D:'$D(IBDFCLIN(IBDFDV,IBDFCL)) HEADER1^IBDFFT2 D
 ..F IBDFT=0:0 S IBDFT=$O(^TMP("FTRK",$J,IBDFDV,IBDFCL,IBDFT)) Q:'IBDFT  F IBDFPAT=0:0 S IBDFPT=$O(^TMP("FTRK",$J,IBDFDV,IBDFCL,IBDFT,IBDFPT)) Q:IBDFPT']""  D
 ...F IBDFIFN=0:0 S IBDFIFN=$O(^TMP("FTRK",$J,IBDFDV,IBDFCL,IBDFT,IBDFPT,IBDFIFN)) Q:'IBDFIFN  S IBDX="" F  S IBDX=$O(^TMP("FTRK",$J,IBDFDV,IBDFCL,IBDFT,IBDFPT,IBDFIFN,IBDX)) Q:IBDX=""  S IBDFTMP=^(IBDX) D SETARR
 I '$D(^TMP("FRM",$J)) D NUL^IBDFFT2
 Q
CLN ;  -- Loop clinics
 N IBDFCLIN
 I VAUTC=1 F IBDFCLIN=0:0 S IBDFCLIN=$O(^SC(IBDFCLIN)) Q:'IBDFCLIN  D CK(IBDFCLIN) I QUIT=1 D BLD
 I VAUTC=0 F IBDFCLIN=0:0 S IBDFCLIN=$O(VAUTC(IBDFCLIN)) Q:'IBDFCLIN  D CK(IBDFCLIN) I QUIT=1 D BLD
 D TRACKING Q
PAT ;  -- Loop patients
 N IBDFCLIN,IBDFPAT
 I VAUTN=1 F IBDFPAT=0:0 S IBDFPAT=$O(^DPT(IBDFPAT)) Q:'IBDFPAT  F IBDFT=IBDFBEG:0 S IBDFT=$O(^DPT(IBDFPAT,"S",IBDFT)) Q:'IBDFT!($P(IBDFT,".",1)>IBDFEND)  I $D(^DPT(IBDFPAT,"S",IBDFT,0)) D SET
 I VAUTN=0 F IBDFPAT=0:0 S IBDFPAT=$O(VAUTN(IBDFPAT)) Q:'IBDFPAT  F IBDFT=IBDFBEG:0 S IBDFT=$O(^DPT(IBDFPAT,"S",IBDFT)) Q:'IBDFT!($P(IBDFT,".",1)>IBDFEND)  I $D(^DPT(IBDFPAT,"S",IBDFT,0)) D SET
 D TRACKING Q
GRP D GRP1^IBDFFT
 N IBDFGRP,IBDFCLIN
 F IBDFGRP=0:0 S IBDFGRP=$O(VAUTG(IBDFGRP)) Q:'IBDFGRP  F IBDFCLIN=0:0 S IBDFCLIN=$O(VAUTG(IBDFGRP,IBDFCLIN)) Q:'IBDFCLIN  D CK(IBDFCLIN) I QUIT=1 D BLD
 D TRACKING Q
 ;
 ;
SET S IBDFCLIN=$P(^DPT(IBDFPAT,"S",IBDFT,0),"^",1) D CK(IBDFCLIN) I QUIT=1 S DFN=IBDFPAT D CK1 Q
 Q
 ;
 ;
CK(XCL) ;  -- Check clinic, division, form
 Q:'$D(^SC(+XCL,0))
 S QUIT=0
 S IBDFNODE=$G(^SC(XCL,0))
 Q:$P(IBDFNODE,"^",3)'="C"
 I $G(VAUTD)=0 I $P(IBDFNODE,"^",15)  Q:'$D(VAUTD($P(IBDFNODE,"^",15)))
 D CHECK^IBDFFT(XCL)
 Q:QUIT=0
 Q
 ;
 ;
BLD ; -- scan appts
 F IBDFT=IBDFBEG:0 S IBDFT=$O(^SC(IBDFCLIN,"S",IBDFT)) Q:'IBDFT!($P(IBDFT,".",1)>IBDFEND)  D
 .F IBDFDA=0:0 S IBDFDA=$O(^SC(IBDFCLIN,"S",IBDFT,1,IBDFDA)) Q:'IBDFDA  I $D(^SC(IBDFCLIN,"S",IBDFT,1,IBDFDA,0)) S IBDFSA=^(0) S DFN=+IBDFSA D CK1
 Q
CK1 ; -- 
 N IBDFXPC,IBDFYPC
 S IBDFXPC=$S($D(VAUTC)!($D(VAUTG)):$P(IBDFNODE,"^",1),1:$P(^DPT(IBDFPAT,0),"^",1))
 S IBDFYPC=$S($D(VAUTC)!($D(VAUTG)):$P(^DPT(DFN,0),"^",1),1:$P(IBDFNODE,"^",1))
 I $D(^IBD(357.96,"APTAP",DFN,IBDFT)) S IBDFIFN=0 F  S IBDFIFN=$O(^IBD(357.96,"APTAP",DFN,IBDFT,IBDFIFN)) Q:'IBDFIFN  I $D(^IBD(357.96,IBDFIFN,0)) D
 .I $D(VAUTG) S ^TMP("FTRK",$J,$S($D(^DG(40.8,+$P(IBDFNODE,"^",15),0)):$P(^DG(40.8,$P(IBDFNODE,"^",15),0),"^",1),1:"NOT SPECIFIED"),$P(^IBD(357.99,IBDFGRP,0),"^",1),IBDFXPC,IBDFT,IBDFYPC,DFN,+IBDFIFN)=IBDFCLIN_"^"_^IBD(357.96,IBDFIFN,0)
 .I '$D(VAUTG) S ^TMP("FTRK",$J,$S($D(^DG(40.8,+$P(IBDFNODE,"^",15),0)):$P(^DG(40.8,$P(IBDFNODE,"^",15),0),"^",1),1:"NOT SPECIFIED"),IBDFXPC,IBDFT,IBDFYPC,DFN,+IBDFIFN)=IBDFCLIN_"^"_^IBD(357.96,IBDFIFN,0)
 .Q
 E  D
 .I $D(VAUTG) S ^TMP("FTRK",$J,$S($D(^DG(40.8,+$P(IBDFNODE,"^",15),0)):$P(^DG(40.8,$P(IBDFNODE,"^",15),0),"^",1),1:"NOT SPECIFIED"),$P(^IBD(357.99,IBDFGRP,0),"^",1),IBDFXPC,IBDFT,IBDFYPC,DFN,0)=IBDFCLIN_"^^"_DFN_"^"_IBDFT
 .I '$D(VAUTG) S ^TMP("FTRK",$J,$S($D(^DG(40.8,+$P(IBDFNODE,"^",15),0)):$P(^DG(40.8,$P(IBDFNODE,"^",15),0),"^",1),1:"NOT SPECIFIED"),IBDFXPC,IBDFT,IBDFYPC,DFN,0)=IBDFCLIN_"^^"_DFN_"^"_IBDFT
 Q
 ;
SETARR ;  -- Set up Listman array
 S DFN=$P(IBDFTMP,"^",3)
 I '$D(^TMP("CNT",$J,$S(IBDFDV]"":IBDFDV,1:"NOT SPECIFIED"),IBDFCL)) D
 .S ^TMP("CNT",$J,$S(IBDFDV]"":IBDFDV,1:"NOT SPECIFIED"),IBDFCL)="0^0^0^0^0^0^0"
 .I $D(VAUTG) I '$D(^TMP("COUNT",$J,$S(IBDFDV]"":IBDFDV,1:"NOT SPECIFIED"),IBDFGR,IBDFCL)) D
 ..S ^TMP("COUNT",$J,$S(IBDFDV]"":IBDFDV,1:"NOT SPECIFIED"),IBDFGR,IBDFCL)=1
 I $D(VAUTG) K IBDFLAG I $D(^TMP("COUNT",$J,IBDFCL,IBDFT,IBDFIFN)) I IBDFGR=^TMP("COUNT",$J,IBDFCL,IBDFT,IBDFIFN) D COUNT
 I $D(VAUTG) I '$D(^TMP("COUNT",$J,IBDFCL,IBDFT,IBDFIFN)) D COUNT
 I '$D(VAUTG) S $P(^TMP("CNT",$J,IBDFDV,IBDFCL),"^",1)=$P(^TMP("CNT",$J,IBDFDV,IBDFCL),"^",1)+1
 S IBDCNT1=IBDCNT1+1
 S IBDCNT=IBDCNT+1,VALMCNT=VALMCNT+1
 S X=""
 S IBDFVAL=$J(IBDCNT1_")",5)
 S X=$$SETSTR^VALM1(IBDFVAL,X,1,5)
 S IBDFVAL=$P($G(IBDFTMP),"^",2)
 S X=$$SETSTR^VALM1(IBDFVAL,X,7,8)
 S IBDFVAL=$P($G(IBDFTMP),"^",4) I IBDFVAL S DNKA=$$DNKA(DFN,IBDFVAL),IBDFVAL=$P($$FMTE^XLFDT(IBDFVAL,2),":",1,2)
 S X=$$SETSTR^VALM1(IBDFVAL,X,17,14)
 I $D(VAUTC)!($D(VAUTG)) S (IBDFVAL,IBDFN)=$P($G(IBDFTMP),"^",3) I IBDFVAL]"" S IBDFVAL=$P(^DPT(IBDFVAL,0),"^",1)
 I $D(VAUTN) S (IBDFVAL,IBDFN)=$P($G(IBDFTMP),"^",1) I IBDFVAL]"" S IBDFVAL=$P(^SC(IBDFVAL,0),"^",1)
 S X=$$SETSTR^VALM1(IBDFVAL,X,34,15)
 S IBDFVAL=$P($G(IBDFTMP),"^",6)
 I IBDFVAL]"" S IBDFVAL=$E(IBDFVAL,4,5)_"/"_$E(IBDFVAL,6,7)_"/"_$E(IBDFVAL,2,3) I '$D(VAUTG)!($D(VAUTG)&($D(IBDFLAG))) S $P(^TMP("CNT",$J,IBDFDV,IBDFCL),"^",2)=+($P(^TMP("CNT",$J,IBDFDV,IBDFCL),"^",2))+1
 S X=$$SETSTR^VALM1(IBDFVAL,X,50,8)
 S VAL=$P($G(IBDFTMP),"^",12)
 S IBDFVAL=$P($G(IBDFTMP),"^",7)
 I IBDFVAL]"" S IBDFVAL=$E(IBDFVAL,4,5)_"/"_$E(IBDFVAL,6,7)_"/"_$E(IBDFVAL,2,3) I '$D(VAUTG)!($D(VAUTG)&($D(IBDFLAG))) I VAL=2 S $P(^TMP("CNT",$J,IBDFDV,IBDFCL),"^",3)=+($P(^TMP("CNT",$J,IBDFDV,IBDFCL),"^",3))+1
 S X=$$SETSTR^VALM1(IBDFVAL,X,61,8)
 N IBDFXX
 S IBDFXX=$S(VAL=3:3,VAL=6:5,1:"")
 I IBDFXX]"" I '$D(VAUTG)!($D(VAUTG)&($D(IBDFLAG))) S $P(^TMP("CNT",$J,IBDFDV,IBDFCL),"^",IBDFXX)=$P(^TMP("CNT",$J,IBDFDV,IBDFCL),"^",IBDFXX)+1 S $P(^TMP("CNT",$J,IBDFDV,IBDFCL),"^",6)=$P(^TMP("CNT",$J,IBDFDV,IBDFCL),"^",6)+1
 S VAL=$S(DNKA:$P(DNKA,"^",2),VAL=1:"PRINTED",VAL=2:"SCANNED",VAL=3:"SCD/PCE",VAL=4:"SCD w/ER",VAL=5:"DENTRY",VAL=6:"DE to PCE",VAL=7:"DE w/ER",VAL=11:"PEND Pgs",VAL=12:"ER/NOTRN",20:"AVAIL DE",1:"NOT PRNT")
 I DNKA S $P(^TMP("CNT",$J,IBDFDV,IBDFCL),"^",7)=+($P(^TMP("CNT",$J,IBDFDV,IBDFCL),"^",7))+1
 S X=$$SETSTR^VALM1(VAL,X,72,8)
 S IBDFVAL=$S(DNKA:"",1:$$SCHSTAT^IBDFFT($P(IBDFTMP,"^",3),$P(IBDFTMP,"^",4)))
 S X=$$SETSTR^VALM1(IBDFVAL,X,82,12)
 S IBDFVAL=$S($P(IBDFTMP,"^",14):" Yes",1:" No")
 S X=$$SETSTR^VALM1(IBDFVAL,X,96,6)
 ;
 ;
TMP ; -- Set up TMP Array
 S ^TMP("FRM",$J,IBDCNT,0)=$$LOWER^VALM1(X),^TMP("FRM",$J,"IDX",VALMCNT,IBDCNT1)=""
 S ^TMP("FRMIDX",$J,IBDCNT1)=VALMCNT_"^"_$P(IBDFTMP,"^",2)_"^"_$P(IBDFTMP,"^",3)_"^"_$P(IBDFTMP,"^",4)_"^"_$P(IBDFTMP,"^",6)_"^"_$P(IBDFTMP,"^",7)_"^"_$P(IBDFTMP,"^",12)
 D NOW^%DTC S IBDFTIME=% S X1=$S($P(IBDFTMP,"^",7):$P(IBDFTMP,"^",7),1:IBDFTIME),X2=$P(IBDFTMP,"^",4) D ^%DTC S $P(^TMP("CNT",$J,IBDFDV,IBDFCL),"^",4)=+($P(^TMP("CNT",$J,IBDFDV,IBDFCL),"^",4))+X
 Q
COUNT ;
 S ^TMP("COUNT",$J,IBDFCL,IBDFT,IBDFIFN)=IBDFGR,IBDFLAG=1
 S $P(^TMP("CNT",$J,IBDFDV,IBDFCL),"^",1)=$P(^TMP("CNT",$J,IBDFDV,IBDFCL),"^",1)+1
 Q
TRACKING ;  -- loops thru forms tracking file 357.96
 ;     X-reference ^IBD(357.96,"ADATNA", Appt date/time, 1 or 0, IFN).
 ;     1 = forms tracking file entry but no scheduled appt associated
 ;     0 = forms tracking file entry with associated scheduled appt.
 N IBDFCLIN,IBAPPTDT,IBDFPAT,IBDFTRK,IBDFX,IBDFT
 S IBDFX=""
 F IBDFT=IBDFBEG:0 S IBDFT=$O(^IBD(357.96,"ADATNA",IBDFT)) Q:'IBDFT!(IBDFT>IBDFEND)  S IBDFTRK=0 F  S IBDFTRK=$O(^IBD(357.96,"ADATNA",IBDFT,1,IBDFTRK)) Q:'IBDFTRK  D
 .Q:'$G(^IBD(357.96,IBDFTRK,0))
 .S IBDFCLIN=$P(^IBD(357.96,IBDFTRK,0),"^",10)
 .I IBDFCLIN']"" Q
 .S IBDFPAT=$P(^IBD(357.96,IBDFTRK,0),"^",2)
 .D CK(IBDFCLIN) I QUIT=1 D
 ..I $D(VAUTC),VAUTC=0,'$D(VAUTC(IBDFCLIN)) Q
 ..I $D(VAUTN),VAUTN=0,'$D(VAUTN(IBDFPAT)) Q
 ..N IBDFXPC,IBDFYPC
 ..S IBDFXPC=$S($D(VAUTC):$P(IBDFNODE,"^",1),$D(VAUTG):$P(IBDFNODE,"^",1),1:$P(^DPT(IBDFPAT,0),"^",1))
 ..S IBDFYPC=$S($D(VAUTC)!($D(VAUTG)):$P(^DPT(IBDFPAT,0),"^",1),1:$P(IBDFNODE,"^",1))
 ..I '$D(VAUTG) S ^TMP("FTRK",$J,$S($D(^DG(40.8,+$P(IBDFNODE,"^",15),0)):$P(^DG(40.8,$P(IBDFNODE,"^",15),0),"^",1),1:"NOT SPECIFIED"),IBDFXPC,IBDFT,IBDFYPC,IBDFPAT,IBDFTRK)=IBDFCLIN_"^"_^IBD(357.96,IBDFTRK,0)
 ..I $D(VAUTG) D
 ...N IBDFGRP,IBDFCLNN,IBDFCLN,IBDFGR
 ...S (IBDFCLN,IBDFGR)=0
 ...F IBDFGR=0:0 S IBDFGR=$O(VAUTG(IBDFGR)) Q:IBDFGR']""  F IBDFCLN=0:0 S IBDFCLN=$O(VAUTG(IBDFGR,IBDFCLN)) Q:IBDFCLN']""  I IBDFCLN=IBDFCLIN D
 ....N IBX,IBY
 ....S IBX=$P($G(^IBD(357.99,IBDFGR,0)),"^"),IBY=$P($G(^SC(IBDFCLN,0)),"^")
 ....S ^TMP("FTRK",$J,$S($D(^DG(40.8,+$P(IBDFNODE,"^",15),0)):$P(^DG(40.8,$P(IBDFNODE,"^",15),0),"^",1),1:"NOT SPECIFIED"),IBX,IBY,IBDFT,IBDFYPC,IBDFPAT,IBDFTRK)=IBDFCLIN_"^"_^IBD(357.96,IBDFTRK,0)
 Q
 ;
DNKA(DFN,APPT) ;
 ; -- return did not keep appointment
 N STATUS,DNKA
 S DNKA=0
 S STATUS=$P($G(^DPT(+$G(DFN),"S",+$G(APPT),0)),"^",2)
 I STATUS]"" I "^N^C^NA^CA^PC^PCA^"[STATUS S DNKA=1_"^"_$S(STATUS["N":"NO SHOW",1:"CANCELED")
 Q DNKA

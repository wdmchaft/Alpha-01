LRRP8 ;DALISC/TNN/J0 - WKLD STATS REPORT BY SHIFT ;8/11/97
 ;;5.2;LAB SERVICE;**121**;Sep 27, 1994
EN ;*** Entry point and control block ***
 S (LREND,LRSTFLG)=0
 D ASKCOM^LRCAPMR2
 D ASK^LRRP8A
DQ ;
 D:'LREND INIT
 D:'LREND BUILD^LRRP8B
 D:'LREND PRINT^LRRP8C
 D CLN
 Q
 ;
INIT ;*** Initialize some variables ***
 K ^TMP("LR",$J)
 U IO
 I LRTO<LRFR S X=LRFR,LRFR=LRTO,LRTO=X
 D PRTINIT^LRCAPU
 S LRHDR="WORKLOAD STATISTICS BY ACCESSION AREA AND SHIFTS"
 S LRHDR2=LRDTH
 Q
 ;
CLN ;*** Clean up ***
 D ^%ZISC,PRTCLN^LRCAPU,WKLDCLN^LRCAPU,CLNMAN^LRCAPMR1
 K ^TMP("LR",$J)
 K LRCDT,LRFR,LRFRV,LRFRD,LRTO,LRTOV,LRTOD,LRDTH,LRDSH,LRSTRT,LRSTOP,LRUC
 K LRCAPS,LRCC,LRCAPNAM,LRCAPNUM,LRCAPFLG,LRCAPIFN,LRA,LRAA,LRCCNT,LRANAM
 K LRREC,LRTIM,LRRPT,LREND,LRST,LRSTFLG,LRNSFT,LRSHFT,LRIN,LRPCT,LRSCNT
 K LRACNT,LRGCNT,LRCONT,LRSQRM,LRMNODE,LRGSTND,LRGQC,LRGRPT,LRGMANL,LRDR
 K LRDATE,LRCOM,LRTCOM,LRCOMM,LRCM
 K DIC,DIR,X,Y,%ZIS,POP,ZTRTN,ZTDESC,ZTSAVE,ZTSK,DTOUT,DUOUT,DIRUT
 Q

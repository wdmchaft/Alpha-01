HBHCRP24 ; LR VAMC(IRMS)/MJT-HBHC file 631 rpt, All active (admitted but not D/C) cases by date range, sorted by pt name, includes: pt name, last 4, date, case manager, most recent visit date, visit discipline, & total; ; 12/21/05 3:41pm
 ;;1.0;HOSPITAL BASED HOME CARE;**21,22**;NOV 01, 1993;Build 2
 D START^HBHCUTL
 G:(HBHCBEG1=-1)!(HBHCEND1=-1) EXIT
 S %ZIS="Q",HBHCCC=0 K IOP,ZTIO,ZTSAVE D ^%ZIS G:POP EXIT
 I $D(IO("Q")) S ZTRTN="DQ^HBHCRP24",ZTDESC="HBPC Expanded Program Census Report",ZTSAVE("HBHC*")="" D ^%ZTLOAD G EXIT
DQ ; De-queue
 U IO
 K ^TMP("HBHC",$J)
 S HBHCTOT=0,$P(HBHCY,"-",81)="",HBHCTMP1="170^171^172^173",HBHCTMP2="174^175^176^177^178"
 S HBHCHEAD="Expanded Program Census",HBHCHDR="W !,?16,""Last"",?23,""Admission"",?38,""Case"",?49,""Most Recent"",?70,""Visit"",!,""Patient Name"",?16,""Four"",?23,""Date"",?38,""Manager"",?49,""Visit Date/Time"",?70,""Discipline"""
 S HBHCCOLM=(80-(30+$L(HBHCHEAD))\2) S:HBHCCOLM'>0 HBHCCOLM=1
 D TODAY^HBHCUTL D:IO'=IO(0)!($D(IO("S"))) HDRRANGE^HBHCUTL
 I '$D(IO("S")),(IO=IO(0)) S HBHCCC=HBHCCC+1 D HDRRANGE^HBHCUTL
LOOP ; Loop thru ^HBHC(631) "AD" (admission date) cross-ref to build report
 S X1=HBHCBEG1,X2=-1 D C^%DTC S HBHCADDT=X
 F  S HBHCADDT=$O(^HBHC(631,"AD",HBHCADDT)) Q:(HBHCADDT="")!(HBHCADDT>HBHCEND1)  S HBHCDFN="" F  S HBHCDFN=$O(^HBHC(631,"AD",HBHCADDT,HBHCDFN)) Q:HBHCDFN=""  S HBHCNOD0=^HBHC(631,HBHCDFN,0) D:$P(HBHCNOD0,U,15)=1 PROCESS
 W:'$D(^TMP("HBHC",$J)) !!,"No data found for Date Range selected."
 I $D(^TMP("HBHC",$J)) D PRTLOOP W !!,HBHCZ,!,"Program Census Total: ",HBHCTOT,!,HBHCZ
 D ENDRPT^HBHCUTL1
EXIT ; Exit module
 D ^%ZISC
 K HBHCADDT,HBHCBEG1,HBHCBEG2,HBHCCASE,HBHCCOLM,HBHCCC,HBHCDAT,HBHCDATE,HBHCDFN,HBHCDISC,HBHCDPT,HBHCDPT0,HBHCEND1,HBHCEND2,HBHCHDR,HBHCHEAD,HBHCI,HBHCNAME,HBHCNOD0,HBHCPAGE,HBHCSTOP,HBHCTDY
 K HBHCTMP,HBHCTMP1,HBHCTMP2,HBHCTOT,HBHCY,HBHCZ,X,X1,X2,Y,^TMP("HBHC",$J)
 Q
PROCESS ; Process record & build ^TMP("HBHC",$J) global
 Q:($P(HBHCNOD0,U,40)]"")&($P(HBHCNOD0,U,40)<HBHCEND1)
 S HBHCDPT=$P(HBHCNOD0,U),HBHCDPT0=^DPT(HBHCDPT,0),(HBHCCASE,HBHCDAT,HBHCDATE)=""
 S HBHCNOD1=$G(^HBHC(631,HBHCDFN,1)) S:$P(HBHCNOD1,U,13)]"" HBHCCASE=$P(^VA(200,$P(^HBHC(631.4,$P(HBHCNOD1,U,13),0),U,2),0),U)
 S HBHCI=0 F  S HBHCI=$O(^HBHC(632,"B",HBHCDPT,HBHCI)) Q:HBHCI'>0  I $P(^HBHC(632,HBHCI,0),U,7)="" S:$P(^HBHC(632,HBHCI,0),U,2)>HBHCDAT HBHCDATE=$P(^HBHC(632,HBHCI,0),U,2)_U_HBHCI S HBHCDAT=$P(^HBHC(632,HBHCI,0),U,2)
 S:HBHCDATE]"" HBHCSTOP=$P(^DIC(40.7,$P(^SC($P(^HBHC(632,$P(HBHCDATE,U,2),0),U,3),0),U,7),0),U,2)
 S:(HBHCDATE]"")&(HBHCTMP1[HBHCSTOP) HBHCDISC=$S(HBHCSTOP=170:"Physician",HBHCSTOP=171:"RN/RNP/PA",HBHCSTOP=172:"Nurse Ext",HBHCSTOP=173:"Soc Worker",1:"")
 S:(HBHCDATE]"")&(HBHCTMP2[HBHCSTOP) HBHCDISC=$S(HBHCSTOP=174:"Therapist",HBHCSTOP=175:"Dietician",HBHCSTOP=176:"Pharmacist",HBHCSTOP=177:"Other",HBHCSTOP=178:"Phone",1:"")
 S:HBHCDATE="" HBHCDATE="* No HBPC Visits *",HBHCDISC="n/a"
 S ^TMP("HBHC",$J,$P(HBHCDPT0,U),HBHCADDT)=$E($P(HBHCDPT0,U,9),6,9)_U_HBHCCASE_U_$P(HBHCDATE,U)_U_HBHCDISC
 Q
PRTLOOP ; Print loop
 S HBHCNAME="" F  S HBHCNAME=$O(^TMP("HBHC",$J,HBHCNAME)) Q:HBHCNAME=""  S HBHCADDT="" F  S HBHCADDT=$O(^TMP("HBHC",$J,HBHCNAME,HBHCADDT)) Q:HBHCADDT=""  D PRINT
 Q
PRINT ; Print report
 I ($D(ZTRTN)!(HBHCCC=0))&((IOSL-$Y)<5) W @IOF D HDRRANGE^HBHCUTL
 S Y=HBHCADDT D DD^%DT S HBHCDAT=Y
 S HBHCTMP=^TMP("HBHC",$J,HBHCNAME,HBHCADDT)
 S Y=$P(HBHCTMP,U,3) D DD^%DT
 W !,$E(HBHCNAME,1,14),?16,$P(HBHCTMP,U),?23,HBHCDAT,?38,$S(($L($P($P(HBHCTMP,U,2),","))>8):$E($P(HBHCTMP,U,2),1,8),1:$P($P(HBHCTMP,U,2),",")),?49,$E(Y,1,18),?70,$P(HBHCTMP,U,4),!,HBHCY
 S HBHCTOT=HBHCTOT+1
 Q

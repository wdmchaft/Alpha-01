DGPSEU2 ;ALB/ERC - REPORTS FOR PSEUDO SSN ; 1/9/06 7:46am
 ;;5.3;Registration;**653**;Aug 13, 1993;Build 2
 ;
 ;creates a report of all dependents with pseudo SSNs
 ;can call for one Pseudo SSN Reason or can call for all reasons
 ;sorted by reason
 ;
TSK2 ;
 N DGQ,DGQUIT,DGREAS,DGXREAS,DGTXT
 N ZTRTN,ZTDESC,ZTSK,ZTIO,ZTDTH,POP,IO,IOBS,IOF,IOHG,IOM,ION,IOPAR
 N IOS,IOSL,IOST,IOT,IOUPAR,IOXY,%ZIS,ZTSAVE
 K ^TMP("DGEVC",$J)
 S DGQUIT=0
 D QUESREAS^DGPSEUDO Q:DGQUIT
 S %ZIS="Q"
 D ^%ZIS I $G(POP) D ^%ZISC,HOME^%ZIS W !,"Job Terminated!" Q
 I $D(IO("Q")) D  Q
 . S ZTRTN="RPT2^DGPSEU2"
 . S ZTDESC="DEPENDENTS WITH PSEUDO SOCIAL SECURITY NUMBERS"
 . S (ZTSAVE("DGXREAS"),ZTSAVE("DGXVET"))=""
 . D ^%ZTLOAD
 . S DGTXT=$S($G(ZTSK):"Task: "_ZTSK_" Queued.",1:"Error: Process not queued!")
 . W !,DGTXT
RPT2 ;
 N DGC,DGPAGE,DGQ
 S (DGQUIT,DGPAGE)=0
 S DGC=0
 D LOOP2
 D HDR2
 I $G(DGC)'>0 W !!?25,"****NO RECORDS TO REPORT****" W ! D PAUSE^DGPSEUDO Q
 D REP2(DGXREAS)
 D ^%ZISC,HOME^%ZIS
 K ^TMP("DGEVC",$J)
 Q
LOOP2 ;
 I $E(IOST,1,2)["C-" U IO(0) W !!,"Scanning file...."
 U IO
 N DGCT,DGIEN13,DGX
 K ^TMP("DGEVC",$J)
 S DGX=999999999
 S DGCT=0
 S ^TMP("DGEVC",$J,"COUNT")=0
 S ^TMP("DGEVC",$J,"COUNT","REFUSED TO PROVIDE")=0
 S ^TMP("DGEVC",$J,"COUNT","SSN UNKNOWN/FOLLOW-UP REQUIRED")=0
 S ^TMP("DGEVC",$J,"COUNT","NO SSN ASSIGNED")=0
 S ^TMP("DGEVC",$J,"COUNT","NULL")=0
 F  S DGX=$O(^DGPR(408.13,"SSN",DGX)) Q:DGX=""  D
 . I DGX'["P" Q
 . S DGIEN13=0
 . F  S DGIEN13=$O(^DGPR(408.13,"SSN",DGX,DGIEN13)) Q:'DGIEN13  D
 . . Q:'$D(^DGPR(408.13,DGIEN13,0))
 . . D PSEU2
 Q
PSEU2 ;
 N DGARR,DGCT,DGDFN,DGDOB,DGERR,DGIEN12,DGNAM,DGPAT,DGPSSN,DGSSN
 I $D(^TMP("DGEVC",$J,DGIEN13)) Q
 D GETS^DIQ(408.13,DGIEN13_",",".01;.09;.1","EI","DGARR","DGERR")
 I $D(DGERR) K DGERR Q
 I $G(DGARR(408.13,DGIEN13_",",.09,"I"))'["P" K DGARR Q
 S DGDEPNAM=$G(DGARR(408.13,DGIEN13_",",.01,"I"))
 S DGDEPSSN=$G(DGARR(408.13,DGIEN13_",",.09,"I"))
 S DGREASON=$G(DGARR(408.13,DGIEN13_",",.1,"E"))
 I $G(DGREASON)']"" S DGREASON="NULL"
 I DGXREAS'="ALL",DGXREAS'=DGREASON K DGARR Q
 S DGIEN12=0
 S DGIEN12=$O(^DGPR(408.12,"C",DGIEN13_";DGPR(408.13,",DGIEN12))
 I $G(DGIEN12)']"" K DGARR Q
 I '$D(^DGPR(408.12,DGIEN12,0)) K DGARR Q
 D GETS^DIQ(408.12,DGIEN12_",",".01;.02","EI","DGARR","DGERR")
 I $D(DGERR) K DGARR,DGERR Q
 S DGDFN=$G(DGARR(408.12,DGIEN12_",",.01,"I"))
 I '$D(^DPT(DGDFN)),($G(^DPT(DGDFN,0))']"") K DGARR Q
 S DGREL=$G(DGARR(408.12,DGIEN12_",",.02,"E"))
 S DGREL=$$GETREL(DGREL)
 D GETS^DIQ(2,DGDFN_",",".01;.09","EI","DGARR","DGERR")
 I $D(DGERR) K DGARR,DGERR Q
 S DGPATNAM=$G(DGARR(2,DGDFN_",",.01,"E"))
 S DGPATSSN=$G(DGARR(2,DGDFN_",",.09,"I"))
 S DGC=DGC+1
 S ^TMP("DGEVC",$J,DGPATNAM,DGDFN,DGDEPNAM,DGIEN13)=DGPATSSN_"^"_DGREL_"^"_DGDEPSSN_"^"_DGREASON
 S ^TMP("DGEVC",$J,"COUNT")=DGC
 S ^TMP("DGEVC",$J,"COUNT",DGREASON)=$G(^TMP("DGEVC",$J,"COUNT",DGREASON))+1
 K DGARR,DGDFN,DGERR,DGDEPNAM,DGDEPSSN,DGPATNAM,DGPATSSN,DGREASON,DGREL
 Q
HDR2 ;
 N DGDATE,DGL,DGLINE,DGT,Y ;display veteran, non-vet or both
 I $E(IOST,1,2)["C-" W @IOF
 S DGPAGE=DGPAGE+1
 W !?((IOM-46)\2),"Pseudo SSN Report for Means Test Dependents",?70,"Page:"_DGPAGE
 S DGT="Report shows "_$S(DGXREAS="NULL":"<NONE ENTERED>",1:DGXREAS)
 S DGL=$L(DGT)
 W !?((IOM-DGL)\2),DGT
 S Y=DT X ^DD("DD") S DGDATE=Y
 W !?62,"Date: "_$G(DGDATE)
 W !!,"PATIENT",?27,"PATIENT SSN"
 W !?5,"DEPENDENT",?38,"RELATIONSHIP",?52,"DEP. PSSN",?64,"PSSN REASON"
 N DGZ
 W !
 F DGZ=1:1:IOM W "-"
 Q
REP2(DGXREAS) ;
 N DG0,DGCT,DGDNAM,DGIEN,DGN,DGPNAM,DGRR
 S (DGDNAM,DGN,DGDFN,DGPNAM)=""
 S DGCT=0
 F  S DGPNAM=$O(^TMP("DGEVC",$J,DGPNAM)) Q:DGPNAM']""!($G(DGQ))  D
 . I DGPNAM="COUNT",($O(^TMP("DGEVC",$J,DGPNAM,""))'>0) Q
 . F  S DGDFN=$O(^TMP("DGEVC",$J,DGPNAM,DGDFN)) Q:DGDFN'>0!($G(DGQ))  D
 . . N DG0
 . . I $E(IOST,1,2)["C-",($Y>(IOSL-4)) D PAUSE^DGPSEUDO Q:$G(DGQ)
 . . I $Y>(IOSL-4) D
 . . . W @IOF
 . . . D HDR2
 . . S DG0=^DPT(DGDFN,0)
 . . S DGSSN=$P(DG0,U,9)
 . . W !!,$E($G(DGPNAM),1,25),?27,$G(DGSSN)
 . . S (DGDNAM,DGIEN)=""
 . . F  S DGDNAM=$O(^TMP("DGEVC",$J,DGPNAM,DGDFN,DGDNAM)) Q:DGDNAM']""!($G(DGQ))  D
 . . . F  S DGIEN=$O(^TMP("DGEVC",$J,DGPNAM,DGDFN,DGDNAM,DGIEN)) Q:DGIEN'>0!($G(DGQ))  D
 . . . . S DGN=^TMP("DGEVC",$J,DGPNAM,DGDFN,DGDNAM,DGIEN)
 . . . . S DGRR=$P(DGN,U,4)
 . . . . S DGRR=$S(DGRR["REF":"REF TO PROVIDE",DGRR["UNKN":"SSN UNK-F/U REQ",DGRR["NULL":"<NONE ENTERED>",1:DGRR)
 . . . . I $E(IOST,1,2)["C-",($Y>(IOSL-4)) D PAUSE^DGPSEUDO Q:$G(DGQ)
 . . . . I $Y>(IOSL-4) D
 . . . . . W @IOF
 . . . . . D HDR2
 . . . . . W !,$E($G(DGPNAM),1,25),?27,$G(DGSSN)
 . . . . W !?5,$G(DGDNAM),?38,$E($P(DGN,U,2),1,12),?52,$P(DGN,U,3),?64,$G(DGRR)
 . . . . S DGCT=DGCT+1
 I DGCT=DGC D
 . I $E(IOST,1,2)["C-",($Y>(IOSL-6)) D PAUSE^DGPSEUDO Q:$G(DGQ)
 . I $Y>(IOSL-6) D
 . . W @IOF
 . . D HDR2
 . W !!?5,"Total number of dependents with Pseudo SSNs for this report: "_DGC
 . I DGXREAS="ALL" D
 . . W !?31,"Dependents who REFUSED TO PROVIDE: "_^TMP("DGEVC",$J,"COUNT","REFUSED TO PROVIDE")
 . . W !?29,"Dependents who have NO SSN ASSIGNED: "_^TMP("DGEVC",$J,"COUNT","NO SSN ASSIGNED")
 . . W !?33,"Dependents who have SSN UNKNOWN: "_^TMP("DGEVC",$J,"COUNT","SSN UNKNOWN/FOLLOW-UP REQUIRED")
 . . W !?22,"Dependents who have no PSSN Reason entered: "_^TMP("DGEVC",$J,"COUNT","NULL")
 W !
 I $E(IOST,1,2)["C-",('$G(DGQ)) D PAUSE^DGPSEUDO
 D ^%ZISC,HOME^%ZIS
 Q
GETREL(DGREL) ;some relationships will need to be abbreviated to fit the 12 
 ; char spacing limit
 I DGREL']"" Q DGREL
 I $P(DGREL,"-")="GREAT" S $P(DGREL,"-")="GR"
 Q DGREL

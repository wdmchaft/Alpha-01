HLCSORA2 ;ALB/MFK - OUTPUT ROUTINE FOR HL7- 11/10/94
 ;;1.6;HEALTH LEVEL SEVEN;;Oct 13, 1995
START ;
 N HLAAA,HLDONE,HLLINE,HLPAGE,HLTODAY,HLTITLE,HLDATA,%H,DIRUT,X,Y
 S HLDONE=0,HLLINE="",%H=$P($H,",",1),HLPAGE=1
 D YX^%DTC
 S HLTODAY=Y,HLTITLE="HL7 LOGICAL LINK INFORMATION"
 D HLTITLE
 F  S HLLINE=$O(^TMP("HLCSORAT",$J,HLLINE)) Q:(HLLINE="")!(HLDONE)  D LINEE
 Q
LINEE ;
 S HLDATA=""
LINE Q:HLDONE!(HLLINE="")
 D:$Y+4>IOSL HEADING
 Q:HLDONE
 S HLDATA=$O(^TMP("HLCSORAT",$J,HLLINE,HLDATA)) Q:(HLDATA="")
 S HLZ=1
 W $G(^TMP("HLCSORAT",$J,HLLINE,HLDATA)),!
 G LINE
 Q
HEADING ;
 F HLAAA=$Y:1:(IOSL-3) W !
 ; if a CRT device, ask for RETURN to continue or "^" to quit
 I ($E(IOST,1,2)="C-")
 I  S DIR(0)="E" D ^DIR K DIR I Y=0!(Y="")!($D(DIRUT)) S HLDONE=1 Q
 D HLTITLE
 Q
HLTITLE Q:HLDONE
 I ($E(IOST,1,2)="C-")!(HLPAGE'=1) W @IOF
 W !,HLTODAY,?25,HLTITLE,?68,"   PAGE ",HLPAGE,!
 S X="",$P(X,"=",IOM)="" W X,!
 S HLPAGE=HLPAGE+1
 Q

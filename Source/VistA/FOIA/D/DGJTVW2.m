DGJTVW2 ;ALB/MAF - DISPLAY SCREENS FOR INCOMPLETE RECORDS TRACKING (LIST PROCESSOR) CONT. ; SEP 31,1992@900
 ;;1.0;Incomplete Records Tracking;;Jun 25, 2001
 K ^TMP("DGJRPT",$J)
 S X="",(VALMCNT,DGJCNT)=0,VALMBG=1
 S X=$$SETSTR^VALM1(DGJTHDR,X,25,$L(DGJTHDR)) D TMP
 S X=""
 S X=$$SETSTR^VALM1("1)",X,1,2)
 S X=$$SETSTR^VALM1("3)",X,42,2) D TMP
 S X=""
 S DGJVAL=$P(DGJTNO,"^",2) S DGJVAL=$S($D(^VAS(393.3,DGJVAL,0)):$P(^(0),"^",1),1:"")
 I DGJVAL="DISCHARGE SUMMARY" S (DGJTX4,X1)=$P(^DGPM(+$P(DGJTNO,"^",4),0),"^",1),DGJTX3=+$P(DGJTNO,"^",3) S X2=2 D C^%DTC I DGJTX3<X&($P(DGJTNO,"^",3)>DGJTX4) S DGJVAL="DISCHARGE SUM <48" S DGJTSF=1 K DGJTX3,DGJTX4
 S X=$$SETSTR^VALM1("     *Type of Report: ",X,1,22)
 S X=$$SETSTR^VALM1(DGJVAL,X,23,18)
 S DGJVAL=$P(DGJTNDT,"^",1),Y=DGJVAL I DGJVAL]"" X ^DD("DD") S DGJVAL=Y
 I $D(DGJTSF),DGJVAL']"",$P(DGJTNDT,"^",5)]"" D SFRM
 S X=$$SETSTR^VALM1("      Date Dictated: ",X,42,21)
 S X=$$SETSTR^VALM1(DGJVAL,X,63,18) D TMP
 S X=""
 S DGJVAL="         "_$S($P(DGJTNO,"^",2)=1&('$D(DGJTVIEW)):"*",1:" ")_"Event Date: "
 S X=$$SETSTR^VALM1(DGJVAL,X,1,22)
 S DGJVAL=$P(DGJTNO,"^",3),Y=DGJVAL I DGJVAL]"" X ^DD("DD") S DGJVAL=Y
 S X=$$SETSTR^VALM1(DGJVAL,X,23,18)
 S DGJVAL=$P(DGJTNDT,"^",2) S DGJVAL=$S($D(^VA(200,+DGJVAL,0)):$P(^(0),"^"),1:"")
 I $D(DGJTSF),DGJVAL']"",$P(DGJTNDT,"^",5)]"" D SFRM
 S X=$$SETSTR^VALM1("        Dictated By: ",X,42,21)
 S X=$$SETSTR^VALM1(DGJVAL,X,63,18) D TMP
 S X="",DGJVAL="          "_$S('$D(DGJTVIEW):"*",1:"")_"Admission: "
 S X=$$SETSTR^VALM1(DGJVAL,X,1,22)
 I $P(DGJTNO,"^",4)]"" S DGJVAL=$P(DGJTNO,"^",4) S Y=$S($D(^DGPM(+DGJVAL,0)):+^DGPM(DGJVAL,0),1:"") X ^DD("DD") S DGJVAL=Y
 I $P(DGJTNO,"^",4)']"" S DGJVAL="OUTPATIENT"
 S X=$$SETSTR^VALM1(DGJVAL,X,23,18)
 S X=$$SETSTR^VALM1("   Date Transcribed: ",X,42,21)
 S DGJVAL=$P(DGJTNDT,"^",3),Y=DGJVAL I DGJVAL]"" X ^DD("DD") S DGJVAL=Y
 I $D(DGJTSF),DGJVAL']"",$P(DGJTNDT,"^",1)']"",$P(DGJTNDT,"^",5)]"" D SFRM
 S X=$$SETSTR^VALM1(DGJVAL,X,63,18) D TMP
 S X=""
 S X=$$SETSTR^VALM1("           *Division: ",X,1,22)
 S DGJVAL=$P(DGJTNO,"^",6) S DGJVAL=$S($D(^DG(40.8,+DGJVAL,0)):$P(^(0),"^",1),1:"")
 S X=$$SETSTR^VALM1(DGJVAL,X,23,18)
 S Y=$P(DGJTNDT,"^",4),C=$P(^DD(393,10.04,0),"^",2) D Y^DIQ K C S DGJVAL=Y
 I $D(DGJTSF),DGJVAL']"",$P(DGJTNDT,"^",1)']"",$P(DGJTNDT,"^",5)]"" D SFRM
 S X=$$SETSTR^VALM1("     Transcribed By: ",X,42,21)
 S X=$$SETSTR^VALM1(DGJVAL,X,63,18) D TMP
 S DGJVAL=$P(DGJTNO,"^",5) S DGJVAL=$S($D(^SC(+DGJVAL,0)):$P(^(0),"^"),1:"")
 S DGJVAL="           "_$S($P(DGJTNO,"^",2)=1&('$D(DGJTVIEW)):"*",1:" ")_"Location: "
 S X=""
 S X=$$SETSTR^VALM1(DGJVAL,X,1,22)
 S DGJVAL=$P(DGJTNO,"^",5) S DGJVAL=$S($D(^SC(+DGJVAL,0)):$P(^(0),"^"),1:"")
 S X=$$SETSTR^VALM1(DGJVAL,X,23,18)
 S X=$$SETSTR^VALM1("        Date Signed: ",X,42,21)
 S DGJVAL=$P(DGJTNDT,"^",5),Y=DGJVAL I DGJVAL]"" X ^DD("DD") S DGJVAL=Y
 S X=$$SETSTR^VALM1(DGJVAL,X,63,18) D TMP
 S DGJVAL=$P(DGJTNO,"^",8)
 S DGJVAL=$S($D(^DG(393.1,+DGJVAL,0)):$P(^(0),"^",1),1:"")
 S X=""
 S X=$$SETSTR^VALM1("            *Service: ",X,1,22)
 S X=$$SETSTR^VALM1(DGJVAL,X,23,18)
 S DGJVAL=$P(DGJTNO,"^",8)
 S DGJVAL=$S($D(^DG(393.1,+DGJVAL,0)):$P(^(0),"^",1),1:"")
 S X=$$SETSTR^VALM1(DGJVAL,X,23,18)
 S DGJVAL=$P(DGJTNDT,"^",6) S DGJVAL=$S($D(^VA(200,+DGJVAL,0)):$P(^(0),"^"),1:"")
 S X=$$SETSTR^VALM1("          Signed By: ",X,42,21)
 S X=$$SETSTR^VALM1(DGJVAL,X,63,18) D TMP
 S X=""
 S X=$$SETSTR^VALM1("   Phys. Responsible: ",X,1,22)
 S DGJVAL=$P(DGJTNO,"^",12) S DGJVAL=$S($D(^VA(200,+DGJVAL,0)):$P(^(0),"^"),1:"")
 S X=$$SETSTR^VALM1(DGJVAL,X,23,18)
 I $P(DGJTDEL,"^",3)=1 S X=$$SETSTR^VALM1("      Date Reviewed: ",X,42,21)
 I $P(DGJTDEL,"^",3)=1 S DGJVAL=$P(DGJTNDT,"^",7),Y=DGJVAL I DGJVAL]"" X ^DD("DD") S DGJVAL=Y S X=$$SETSTR^VALM1(DGJVAL,X,63,18)
 D TMP
 S DGJVAL=$P(DGJTNDT,"^",8) S DGJVAL=$S($D(^VA(200,+DGJVAL,0)):$P(^(0),"^"),1:"")
 S X=""
 I $P(DGJTDEL,"^",3)=1 S X=$$SETSTR^VALM1("        Reviewed By: ",X,42,21),X=$$SETSTR^VALM1(DGJVAL,X,63,18) D TMP
 S X=""
 S X=$$SETSTR^VALM1("2)",X,1,2) D TMP
 Q
 ;
 ;
TMP S DGJCNT=DGJCNT+1,VALMCNT=VALMCNT+1
 S ^TMP("DGJRPT",$J,DGJCNT,0)=X,^TMP("DGJRPT",$J,"IDX",VALMCNT,DGJCNT)=""
 S ^TMP("RPTIDX",$J,DGJCNT)=VALMCNT
 Q
SFRM S DGJVAL="NOT APPLICABLE"
 Q

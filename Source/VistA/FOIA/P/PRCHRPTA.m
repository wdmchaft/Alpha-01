PRCHRPTA ;WISC/TKW-PUBLIC LAW 100-322 REPORT--CONTINUED ;4/13/93  11:15
V ;;5.1;IFCAP;*89*;Oct 20, 2000
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 ;
EN S (PRCHTOT,PRCHTOTD)=0 D RD1 W $C(13) Q  ;W ?132,"___________",!!,?120,"**TOTAL**",?132,$J(PRCHTOT,11,2),!,$C(13)
 Q
 ;
RD1 ;PRINT DETAILED REPORT
 S PRCHFSC="" F J=1:1 S PRCHFSC=$O(^TMP($J,"R",PRCHFSC)) G:PRCHFSC="" Q D:PRCHDY>55 HDR S PRCHDY=PRCHDY+1 D RD2
 Q
 ;
RD2 S PRCHDESC=0 F  S PRCHDESC=$O(^TMP($J,"R",PRCHFSC,PRCHDESC)) Q:PRCHDESC=""  D RD3
 Q
 ;
RD3 S (PRCHV,L)="" F  S PRCHV=$O(^TMP($J,"R",PRCHFSC,PRCHDESC,PRCHV)) Q:PRCHV=""  F  S L=$O(^TMP($J,"R",PRCHFSC,PRCHDESC,PRCHV,L)) Q:L=""  S PRCHSRC="" D
   . F  S PRCHSRC=$O(^TMP($J,"R",PRCHFSC,PRCHDESC,PRCHV,L,PRCHSRC)) Q:PRCHSRC=""  S X=^(PRCHSRC) I "2B"[$P(X,U,9) S AVRG=$S($P(X,U,4)'=0:($P(X,U,6))/($P(X,U,4)),1:0) S PRCHTOTD=PRCHTOTD+$P(X,U,6) D PRT
 Q
 ;
PRT D:PRCHDY>60 HDR W PRCHDESC,?32,$J($P(X,U,12),5),?39,PRCHFSC,?46,$S($P(X,U,2)'=0:$P(X,U,2),1:"-"),?61,$J($FN($P(X,U,4),","),7),?73,$S(PRCHV'=0:PRCHV,1:"-")
 W ?77,$J($P(X,U,6),11,2),?90,$J($P(X,U,10),9,2),?101,$J($P(X,U,11),9,2),?112,$J(AVRG,9,2),!
 S PRCHTOT=PRCHTOT+$P(X,U,6),PRCHDY=PRCHDY+1
 Q
 ;
HDR S PRCHPAGE=PRCHPAGE+1 W @IOF,?55,"P.L. 100-322 Local Procurement",!,?56,"Aggregated Item Detail Report",!,?108,$P(PRCHPDAT,"@"),?122,"PAGE ",PRCHPAGE,!
 W ?4,"STATION: "_PRC("SITE")_"-"_PRCHSITE,!,"Dates Received: "_PRCHFT_"    FSC CODES: " F I=0:0 S I=$O(^TMP($J,"FSCG",I)) Q:'I  W I_" "
 W "    Non-Expendable Purchases NOT Included",!
 W ?33,"ITEM",?63,"TOTAL",?82,"TOTAL",?96,"LOW",?106,"HIGH",?115,"AVERAGE",!
 W "DESCRIPTION",?32,"NUMBER",?40,"FSC",?48,"N.I.I.N",?62,"QUANTITY",?72,"UNIT",?81,"DOLLARS",?95,"COST",?106,"COST",?116,"COST",!
 F I=0:1:(IOM-2) W "-"
 W !! S PRCHDY=7
 Q
 ;
EN2 ;PRINT SUMMARY TOTALS
 D HDR2,RDS
 Q
 ;
RDS S (PRCHFSCG,PRCHGT,PRCHT)="" F PRCHFSC=0:0 S PRCHFSC=$O(^TMP($J,"FSC",PRCHFSC)) Q:'PRCHFSC  D:$E(PRCHFSC,1,2)'=PRCHFSCG GT S X=^TMP($J,"FSC",PRCHFSC) D ACM W PRCHFSC_" "_$P(^TMP($J,"FSC",PRCHFSC),U,1) D PRT2
 S:PRCHFSCG]"" PRCHFSCG="END" D GT,T
 Q
 ;
PRT2 S PRCHTOT=+$P(X,U,2)
 W ?41,$J(PRCHTOT,11,2) I PRCHTOT W ?60,$J($P(X,U,3),11,2),?79,$J((($P(X,U,3)/PRCHTOT)*100),6,2),?91,$J($P(X,U,4),11,2),?110,$J((($P(X,U,4)/PRCHTOT)*100),6,2),?122,$J($P(X,U,5),11,2),?141,$J((($P(X,U,5)/PRCHTOT)*100),6,2)
 W ! S PRCHDY=PRCHDY+1
 Q
 ;
GT ;PRINT GROUP SUB-TOTALS
 D:PRCHDY>55 HDR2 I PRCHFSCG="" G GT1
 W ?2,"SUB-TOTAL" S X=PRCHGT D PRT2 Q:PRCHFSCG="END"
 ;
GT1 S PRCHGT="",PRCHFSCG=$E(PRCHFSC,1,2)
 W !,?2,"FSC GROUP: "_$S($D(^TMP($J,"FSCG",PRCHFSCG)):^(PRCHFSCG),1:"**INVALID**"),! S PRCHDY=PRCHDY+2
 Q
 ;
ACM F I=2:1:5 S $P(PRCHGT,U,I)=$P(PRCHGT,U,I)+$P(X,U,I),$P(PRCHT,U,I)=$P(PRCHT,U,I)+$P(X,U,I)
 Q
 ;
T S I="___________" W ?41,I,?60,I,?79,$E(I,1,6),?91,I,?110,$E(I,1,6),?122,I,?141,$E(I,1,6),!
 S PRCHDY=PRCHDY+1,X=PRCHT W ?1,"* TOTAL *" D PRT2
 Q
 ;
HDR2 S PRCHPAGE=PRCHPAGE+1 W @IOF,?55,"P.L. 100-322 SUMMARY TOTALS REPORT",?108,$P(PRCHPDAT,"@"),?122,"PAGE ",PRCHPAGE,!
 W ?4,"STATION: "_PRC("SITE")_"-"_PRCHSITE,!,"Dates Received: "_PRCHFT_"      FSC CODES: " F I=0:0 S I=$O(^TMP($J,"FSCG",I)) Q:'I  W I_" "
 W "     Non-Expendable Purchases NOT Included",!!
 W ?63,"ALL OPEN",?81,"% OF",?94,"OPEN MKT",?112,"% OF",?123,"OPEN MARKET",?143,"% OF",!
 W "FSC",?47,"TOTAL",?65,"MARKET",?80,"TOTAL",?93,"EMERGENCY",?111,"TOTAL",?120,"LESS EMERGENCY",?142,"TOTAL",!
 F J=0:1:(IOM-2) W "-"
 W !! S PRCHDY=8
 Q
 ;
NONE ; perform this if no records were gathered
 D HDR
 W !,"No records matched the selected criteria.",!
 Q
 ;
Q D:PRCHDY>55 HDR S PRCHDY=PRCHDY+1 W ?77,"___________",!," * TOTAL *",?80,PRCHTOTD
 Q
